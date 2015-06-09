(ns leela.storaged.cassandra.triggers
  (:import
   org.slf4j.Logger
   java.nio.ByteBuffer
   org.slf4j.LoggerFactory

   org.apache.cassandra.utils.ByteBufferUtil

   org.apache.cassandra.db.Cell
   org.apache.cassandra.db.Mutation
   org.apache.cassandra.db.ColumnFamily
   org.apache.cassandra.triggers.ITrigger
   org.apache.cassandra.db.composites.CType
   org.apache.cassandra.cql3.ColumnIdentifier
   org.apache.cassandra.db.composites.CellName
   org.apache.cassandra.db.composites.Composite
   org.apache.cassandra.cql3.ColumnSpecification
   org.apache.cassandra.db.marshal.CompositeType
   org.apache.cassandra.db.ArrayBackedSortedColumns))

(def ^{:dynamic true :tag Logger} *logger*)

(defmacro select-coll-names [cf selector-fn]
  (letfn [(get-name [^ColumnSpecification x]
            (.-name x))]
    `(let [cols# (.. ~cf (metadata) (~selector-fn))]
       (map ~get-name cols#))))

(defn- warn [msg]
  (.warn *logger* msg))

(defn- mmap [& args]
  (apply vector (reverse args)))

(defn- mget
  ([mm key] (mget mm key nil))
  ([mm key default]
   (let [v ((reduce #(fn [] (get %2 key (%1))) (constantly default) mm))]
     (if (coll? v) v [v]))))

(defn- reduce-with-conj [m kv]
  (letfn [(assoc-with [m [k v]]
            (if-let [oval (get m k)]
              (assoc m k (conj oval v))
              (assoc m k v)))]
    (reduce assoc-with m kv)))

(defn- empty-bbuff? [^ByteBuffer buff]
  (= 0 (.remaining buff)))

(defn- empty-cell? [^Cell cell]
  (empty-bbuff? (.value cell)))

(defn- split-type [^Composite c]
  (map #(.get c %) (range (.size c))))

(defn- pack [k v]
  (list k v))

(defn- seq-cell-names [^ColumnFamily cf]
  (letfn [(cell-name [^Cell cell]
            (.name cell))
          (split-c-name [^CellName cname]
            (map #(.get cname %) (range (.clusteringSize cname))))
          (split-r-name [^CellName cname]
            (map #(.get cname %) (range (.clusteringSize cname) (.size cname))))
          (split-names [^CellName cname]
            [(split-c-name cname) (split-r-name cname)])]
    (map (comp split-names cell-name)
         (filter (complement empty-cell?) (seq cf)))))

(defn- partition-column-names [^ColumnFamily cf]
  (select-coll-names cf partitionKeyColumns))

(defn- regular-column-names [^ColumnFamily cf]
  (select-coll-names cf regularAndStaticColumns))

(defn- cluster-column-names [^ColumnFamily cf]
  (select-coll-names cf clusteringColumns))

(defn- partition-columns [^ColumnFamily cf key]
  (let [names (partition-column-names cf)
        ctype (.. cf (metadata) (getKeyValidatorAsCType) (fromByteBuffer key))]
    (reduce #(apply (partial assoc %1) %2) {}
            (map pack names (split-type ctype)))))

(defn- cell-values [^ColumnFamily cf]
  (filter (complement empty-bbuff?) (map (fn [^Cell c] (.value c)) (seq cf))))

(defn- regular-columns [^ColumnFamily cf]
  (let [names (regular-column-names cf)
        pairs (map pack names (cell-values cf))]
    (reduce (partial apply assoc) {} pairs)))

(defn- cluster-columns [^ColumnFamily cf]
  (let [names (cluster-column-names cf)
        cells (map (partial map pack names) (map first (seq-cell-names cf)))]
    (reduce reduce-with-conj {} cells)))

(defn- equalize-list [xss]
  (let [size (apply max (map count xss))]
    (map #(take size (cycle %)) xss)))

(defn- transpose [xss]
  (apply map list xss))

(defn- make-keys ^ByteBuffer [^ColumnFamily cf parts]
  (let [ctype  (.. cf (metadata) (getKeyValidatorAsCType))
        names  (partition-column-names cf)
        values (equalize-list (map #(mget parts % []) names))]
    (map #(.toByteBuffer (.make ctype (into-array ByteBuffer %))) (transpose values))))

(defn- conj-last [xs x]
  (if (vector? xs)
    (conj xs x)
    (concat xs [x])))

(defn- make-cols [^ColumnFamily cf parts name values]
  (let [ctype  (.getComparator cf)
        names  (cluster-column-names cf)
        values (equalize-list (map #(mget parts % values) names))]
    (map #(.makeCellName ctype (into-array Object (conj-last % name))) (transpose values))))

(defn- mutate [^ColumnFamily cf ^ByteBuffer pkey ckeys cvals]
  (let [cfname   (.. cf (metadata) -cfName)
        ksname   (.. cf (metadata) -ksName)
        mutation (Mutation. ksname pkey)]
    (doseq [[key val] (map list ckeys cvals)]
      (.add mutation cfname key val (System/currentTimeMillis)))
    mutation))

(defn index-sequence [o-key ^ColumnFamily o-cf]
  (let [n-cf   (.create ArrayBackedSortedColumns/factory (.. o-cf (metadata) -ksName) "seq_index")
        vname  (ColumnIdentifier. "object" true)
        parts  (mmap (partition-columns o-cf o-key)
                     (cluster-columns o-cf)
                     (regular-columns o-cf))
        n-keys (make-keys n-cf parts)
        c-keys (make-cols n-cf parts (.-bytes vname) (cell-values o-cf))
        values (mget parts vname [])]
    [(mutate n-cf (first n-keys) c-keys (mget parts vname))]))

(defn index-metric [o-key ^ColumnFamily o-cf]
  (let [n-cf  (.create ArrayBackedSortedColumns/factory (.. o-cf (metadata) -ksName) "data_index")
        parts (mmap (partition-columns o-cf o-key)
                    (cluster-columns o-cf)
                    (regular-columns o-cf)
                    {(ColumnIdentifier. "location" true) (ByteBufferUtil/bytes "cassandra://")})
        n-keys (make-keys n-cf parts)
        c-keys (make-cols n-cf parts ByteBufferUtil/EMPTY_BYTE_BUFFER  (cell-values o-cf))]
    [(mutate n-cf (first n-keys) c-keys (repeat ByteBufferUtil/EMPTY_BYTE_BUFFER))]))

(deftype SeqTrigger []
  ITrigger
  (augment [this key cf]
    (require 'leela.storaged.cassandra.triggers)
    (binding [*logger* (LoggerFactory/getLogger SeqTrigger)]
      (index-sequence key cf))))

(deftype MetricTrigger []
  ITrigger
  (augment [this key cf]
    (require 'leela.storaged.cassandra.triggers)
    (binding [*logger* (LoggerFactory/getLogger MetricTrigger)]
      (index-metric key cf))))
