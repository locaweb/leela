(ns leela.storaged.cassandra.triggers
  (:import
   org.slf4j.Logger
   java.nio.ByteBuffer
   org.slf4j.LoggerFactory

   org.apache.cassandra.utils.ByteBufferUtil
   
   org.apache.cassandra.db.Cell
   org.apache.cassandra.db.ColumnFamily
   org.apache.cassandra.triggers.ITrigger
   org.apache.cassandra.db.composites.CellName
   org.apache.cassandra.cql3.ColumnSpecification
   org.apache.cassandra.db.marshal.CompositeType))

(def ^{:dynamic true :tag Logger} *logger*)

(defn- warn [msg]
  (.warn *logger* msg))

(defn- reduce-with-set [m kv]
  (letfn [(assoc-with [m [k v]]
            (if-let [oval (get m k)]
              (assoc m k (conj oval v))
              (assoc m k #{v})))]
    (reduce assoc-with m kv)))

(defn- pack [^ColumnSpecification spec key]
  [(ByteBufferUtil/string (.. spec -name -bytes)) (ByteBufferUtil/string key)])

(defn- empty-cell? [^Cell cell]
  (= 0 (.. cell (value) (remaining))))

(defn- split-type [^CompositeType type key]
  (.split type key))

(defn- cells-names [^ColumnFamily cf]
  (letfn [(cell-name [^Cell cell]
            (.name cell))
          (split-name [^CellName cname]
            (map #(.get cname %) (range (.clusteringSize cname))))]
    (map (comp split-name cell-name)
         (filter (complement empty-cell?) (seq cf)))))

(defn partition-keys [^ColumnFamily cf key]
  (let [cols (.. cf (metadata) (partitionKeyColumns))]
    (if (= 1 (count cols))
      (apply (partial assoc {}) (pack (first cols) key))
      (let [type (.. cf (metadata) (getKeyValidator))]
        (reduce #(apply (partial assoc %1) %2) {}
                (map pack cols (split-type type key)))))))

(defn clustering-keys [^ColumnFamily cf]
  (let [cnames (seq (.. cf (metadata) (clusteringColumns)))
        cells (map (partial map pack cnames) (cells-names cf))]
    (reduce reduce-with-set {} cells)))

(defn augment [key cf]
  (let [ckeys (clustering-keys cf)
        pkeys (partition-keys cf key)]
    (warn (format "ckeys=%s" (str ckeys)))
    (warn (format "pkeys=%s" (str pkeys)))))

(deftype MetricsIndexTrigger []
  ITrigger
  (augment [this key cf]
    (require 'leela.storaged.cassandra.triggers)
    (binding [*logger* (LoggerFactory/getLogger MetricsIndexTrigger)]
      (augment key cf))))
