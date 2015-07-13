(ns leela.storaged.cassandra.network.actions.metrics-test
  (:require
   [clojure.test :refer :all]
   [leela.tests.helpers :refer :all]
   [leela.storaged.bytes :as bytes]
   [leela.tests.pagination :refer :all]
   [leela.storaged.cassandra.metrics :as metrics]
   [leela.storaged.network.actions.metrics :refer :all]))

(install-keyspace-fixtures [metrics/create-schema])

(deftest test-get-metric-handler-empty
  (is (empty? (get-metrics-handler {:plane 0
                                    :metric 0
                                    :bucket 0
                                    :offset 0}))))

(deftest-pagination-asc test-get-metrics-handler-pagination
  (let [data  {:plane (rand-int 1000)
               :metric (rand-int 1000)
               :bucket (rand-int 1000)
               :datum (bytes/bytes-from-chars "foobar")}
        query {:plane (:plane data)
               :metric (:metric data)
               :bucket (:bucket data)}]
    {:store-fn #(put-metrics-handler (assoc data :offset %))
     :fetch-fn #(map :offset (get-metrics-handler (assoc query :offset %1 :limit %2)))
     :mkrow-fn identity}))

(deftest-pagination-desc test-get-index-handler-pagination
  (let [data  {:plane (rand-int 1000)
               :metric (rand-int 1000)
               :offset (rand-int 1000)
               :datum (bytes/bytes-from-chars "foobar")}
        query {:plane (:plane data)
               :metric (:metric data)}]
    {:store-fn #(put-metrics-handler (assoc data :bucket %))
     :fetch-fn #(get-index-handler (assoc query :bucket %1 :limit %2))
     :mkrow-fn #(identity {:bucket % :location "cassandra://"})}))
