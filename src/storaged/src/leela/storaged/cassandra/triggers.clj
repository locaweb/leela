(ns leela.storaged.cassandra.triggers
  (:import
   java.nio.ByteBuffer
   org.apache.cassandra.db.Cell
   org.apache.cassandra.db.ColumnFamily
   org.apache.cassandra.triggers.ITrigger
   org.apache.cassandra.cql3.ColumnSpecification
   org.apache.cassandra.db.marshal.CompositeType)
  (:require
   [clojure.tools.logging :refer [warn]]))

(defn- partition-key-1 [^ColumnSpecification col key]
  [(.. col -name -bytes) key])

(defn partition-keys [^ColumnFamily cf key]
  (let [cols (.. cf (metadata) (partitionKeyColumns))]
    (if (= 1 (count cols))
      (partition-key-1 (first cols) key)
      (let [^CompositeType type (.. cf (metadata) (getKeyValidator))]
        (map partition-key-1 cols (.split type key))))))

(defn empty-cell? [^Cell cell]
  (= 0 (.. cell (value) (remaining))))

;; (defn clustering-keys [^ColumnFamily cf]
;;   (let [ccols (.. cf (metadata) (clusteringKeyColumns))
;;         cells (filter (complemet empty-cell? cf))]
;;     (->> )))

;; (deftype MetricsIndexTrigger
;;     ITrigger ((augument [this key cf]
;;                 (warn (format "partition-keys: %d" (count (partition-keys cf key))))
;;                 [])))
