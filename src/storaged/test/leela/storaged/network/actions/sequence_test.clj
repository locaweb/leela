(ns leela.storaged.network.actions.sequence-test
  (:require
    [clojure.test :refer :all]
    [leela.tests.helpers :refer :all]
    [leela.storaged.bytes :as bytes]
    [leela.storaged.cassandra.sequence :as c*]
    [leela.storaged.cassandra.connection :refer [with-limit]]
    [leela.storaged.network.actions.common :refer :all]
    [leela.storaged.network.actions.sequence :refer :all]
    [leela.tests.pagination :refer :all]
    [leela.storaged.cassandra.connection :as conn]))

(install-keyspace-fixtures [c*/create-schema])


(deftest test-get-fetch-block-handler-empty
    (is (empty? (get-fetch-block-handler {:plane 0}))))

;(deftest test-get-fetch-block-handler-not-empty
;   (let [plane (rand-int 100)]
;       (put-store-block-handler :plane)
;       (get-fetch-block-handler :plane)))

(defn debug [x]
    (println (pr-str x))
    x)

(deftest-pagination-token test-get-fetch-sequence-handler-pagination
  {:last-fn  (comp :object last)
   :view-fn  :seqid
   :store-fn #(put-store-sequence-handler {:plane 0 :seqid % :object (bytes/base64-from-chars (str %))})
   :mkrow-fn identity
   :fetch-fn (fn
               ([limit]
                (conn/with-limit limit (get-fetch-sequence-handler {:plane 0})))
               ([token limit]
                (conn/with-limit limit (get-fetch-sequence-handler {:plane 0 :object token}))))})