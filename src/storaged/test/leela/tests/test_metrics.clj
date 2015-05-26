(ns leela.tests.test-metrics
  (:require
   [clojure.test :refer :all]
   [leela.tests.helpers :refer :all]
   [leela.storaged.bytes :as bytes]
   [leela.tests.pagination :refer :all]
   [leela.storaged.cassandra.metrics :as metrics]
   [leela.storaged.cassandra.connection :as conn]))

(install-keyspace-fixtures [metrics/create-schema])

(deftest-pagination-asc test-fetch-metric-pagination
  {:store-fn #(metrics/store-metric +cluster+ 0 0 0 % (byte-array 0))
   :fetch-fn #(conn/with-limit %2 (metrics/fetch-metric +cluster+ 0 0 0 %1))
   :mkrow-fn #(identity {:offset %})
   :view-fn  #(:offset %)})

(deftest-pagination-desc test-fetch-index-pagination
  {:store-fn #(metrics/index-metric +cluster+ 0 0 %1 ".")
   :fetch-fn #(conn/with-limit %2 (metrics/fetch-index +cluster+ 0 0 %1))
   :mkrow-fn #(identity {:bucket % :location "."})})
