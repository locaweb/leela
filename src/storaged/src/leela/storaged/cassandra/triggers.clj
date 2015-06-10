(ns leela.storaged.cassandra.triggers
  (:require
   [leela.storaged.cassandra.config :refer :all])
  (:import
   org.slf4j.Logger
   java.nio.ByteBuffer
   org.slf4j.LoggerFactory
   org.apache.cassandra.db.Cell
   org.apache.cassandra.db.Mutation
   org.apache.cassandra.db.ColumnFamily
   org.apache.cassandra.triggers.ITrigger
   org.apache.cassandra.db.composites.CType
   org.apache.cassandra.utils.ByteBufferUtil
   org.apache.cassandra.cql3.ColumnIdentifier
   org.apache.cassandra.db.composites.CellName
   org.apache.cassandra.db.composites.Composite
   org.apache.cassandra.cql3.ColumnSpecification
   org.apache.cassandra.db.marshal.CompositeType
   org.apache.cassandra.db.ArrayBackedSortedColumns))

(def ^{:dynamic true :tag Logger} *logger*)

(def metrics-cf (ref nil))
(def sequence-cf (ref nil))
(def column-object (ColumnIdentifier. "object" true))

(defn- read-cf ^ColumnFamily [ref ^String kspace ^String name]
  (.create ArrayBackedSortedColumns/factory kspace name))
  ;; (if-let [cf @ref]
  ;;   cf
  ;;   (let [cf ]
  ;;     (dosync (alter ref #(if %1 %1 %2) cf)))))

(defmacro select-coll-names [cf selector-fn]
  `(let [cols# (.. ~cf (metadata) (~selector-fn))]
     (map #(.-name ^ColumnSpecification %) cols#)))

(defn- warn [msg]
  (.warn *logger* msg))

(defn- mmap [& args]
  (apply vector (reverse args)))

(defn- mget
  ([mm key] (mget mm key nil))
  ([mm key default]
   ((reduce #(fn [] (get %2 key (%1))) (constantly default) mm))))

(defn- to-coll [x]
  (if (coll? x) x [x]))

(defn- empty-bbuff? [^ByteBuffer buff]
  (= 0 (.remaining buff)))

(defn- seq-cell-names [^ColumnFamily cf]
  (letfn [(cell-name [^Cell cell]
            (.name cell))
          (split-name [^CellName cname]
            (map #(.get cname %) (range (.clusteringSize cname))))
          (empty-cell? [^Cell cell]
            (empty-bbuff? (.value cell)))]
    (map (comp split-name cell-name)
         (filter (complement empty-cell?) (seq cf)))))

(defn- partition-column-names [^ColumnFamily cf]
  (select-coll-names cf partitionKeyColumns))

(defn- regular-column-names [^ColumnFamily cf]
  (select-coll-names cf regularAndStaticColumns))

(defn- cluster-column-names [^ColumnFamily cf]
  (select-coll-names cf clusteringColumns))

(defn- partition-columns [^ColumnFamily cf key]
  (let [names      (partition-column-names cf)
        ctype      (.. cf (metadata) (getKeyValidatorAsCType) (fromByteBuffer key))
        split-type (fn [^Composite c]
                     (map #(.get c %) (range (.size c))))]
    (reduce #(apply (partial assoc %1) %2) {}
            (map list names (split-type ctype)))))

(defn- cell-values [^ColumnFamily cf]
  (filter (complement empty-bbuff?) (map #(.value ^Cell %) (seq cf))))

(defn- regular-columns [^ColumnFamily cf]
  (let [names (regular-column-names cf)
        pairs (map list names (cell-values cf))]
    (reduce (partial apply assoc) {} pairs)))

(defn- cluster-columns [^ColumnFamily cf]
  (let [names     (cluster-column-names cf)
        cells     (map (partial map list names) (seq-cell-names cf))
        with-conj (fn [m [k v]]
                    (if-let [oval (get m k)]
                      (assoc m k (conj oval v))
                      (assoc m k v)))]
    (reduce (partial reduce with-conj) {} cells)))

(defn- equalize-list [xss]
  (let [size (apply max (map count xss))]
    (map #(take size (cycle %)) xss)))

(defn- transpose [xss]
  (apply map list xss))

(defn- make-keys ^ByteBuffer [^ColumnFamily cf parts]
  (let [ctype  (.. cf (metadata) (getKeyValidatorAsCType))
        names  (partition-column-names cf)
        values (equalize-list (map #(to-coll (mget parts % [])) names))]
    (map #(.toByteBuffer (.make ctype (into-array ByteBuffer %))) (transpose values))))

(defn- make-cols [^ColumnFamily cf parts name values]
  (let [ctype     (.getComparator cf)
        names     (cluster-column-names cf)
        values    (equalize-list (map #(to-coll (mget parts % values)) names))
        conj-last (fn [xs x]
                    (if (vector? xs)
                      (conj xs x)
                      (concat xs [x])))]
    (map #(.makeCellName ctype (into-array Object (conj-last % name))) (transpose values))))

(defn- mutate [^ColumnFamily cf ^ByteBuffer pkey ckeys cvals]
  (let [cfname   (.. cf (metadata) -cfName)
        ksname   (.. cf (metadata) -ksName)
        mutation (Mutation. ksname pkey)]
    (doseq [[key val] (map list ckeys cvals)]
      (.add mutation cfname key val (System/currentTimeMillis)))
    mutation))

(defn- index-metric [o-key o-cf n-cf]
  (let [parts  (mmap (partition-columns o-cf o-key)
                     (cluster-columns o-cf)
                     (regular-columns o-cf)
                     {(ColumnIdentifier. "location" true) (ByteBufferUtil/bytes "cassandra://")})
        n-keys (make-keys n-cf parts)
        c-keys (make-cols n-cf parts ByteBufferUtil/EMPTY_BYTE_BUFFER  (cell-values o-cf))]
    [(mutate n-cf (first n-keys) c-keys (repeat ByteBufferUtil/EMPTY_BYTE_BUFFER))]))

(deftype MTrigger []
  ITrigger
  (augment [this key o-cf]
    (require 'leela.storaged.cassandra.triggers)
    (binding [*logger* (LoggerFactory/getLogger (class this))]
      (let [kspace (.. o-cf (metadata) -ksName)
            n-cf   (read-cf metrics-cf kspace metrics-idx-table)]
        (index-metric key o-cf n-cf)))))

(def mtrigger-class (.getName MTrigger))
