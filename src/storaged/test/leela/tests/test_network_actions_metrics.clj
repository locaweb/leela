(ns leela.tests.test-network-actions-metrics
  (:require
   [clojure.test :refer :all]
   [leela.tests.helpers :refer :all]
   [leela.storaged.bytes :as bytes]
   [leela.tests.pagination :refer :all]
   [leela.storaged.cassandra.metrics :as metrics]
   [leela.storaged.network.actions.metrics :refer :all]))

(install-keyspace-fixtures [metrics/create-schema])

(deftest test-get-metric-handler-empty
  (is (empty? (get-metrics-handler {:schema 0
                                    :metric 0
                                    :bucket 0
                                    :offset 0}))))

(deftest test-get-metric-after-put-metric
  (let [data  {:schema (rand-int 1000)
               :metric (rand-int 1000)
               :bucket (rand-int 1000)
               :offset (rand-int 1000)
               :datum (bytes/bytes-from-chars "foobar")}
        query {:schema (:schema data)
               :metric (:metric data)
               :bucket (:bucket data)}]
    (put-metrics-handler data)
    (let [rows (get-metrics-handler query)]
      (is (= 1 (count rows)))
      (is (= (:offset data) (:offset (first rows))))
      (is (= (seq (:datum data)) (seq (:datum (first rows))))))))

;; (deftest-pagination-asc test-worker-pagination-on-metrics
;;   (let [data  {:schema (rand-int 1000)
;;                :metric (rand-int 1000)
;;                :bucket (rand-int 1000)
;;                :datum (bytes/bytes-from-chars "foobar")}
;;         query {:schema (:schema data)
;;                :metric (:metric data)
;;                :bucket (:metric data)}]
;;     {:store-fn #(put-metrics-handler (assoc data :offset %1))
;;      :fetch-fn #(get-metrics-handler (assoc query :offset %1 :limit %2))
;;      :mkrow-fn #(identity {:offset %})
;;      :view-fn  #(:offset %)}))
