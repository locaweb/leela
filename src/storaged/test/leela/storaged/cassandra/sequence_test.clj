(ns leela.storaged.cassandra.sequence-test
  (:require
   [clojure.test :refer :all]
   [leela.tests.helpers :refer :all]
   [leela.storaged.bytes :as bytes]
   [leela.tests.pagination :refer :all]
   [clojurewerkz.cassaforte.query :as stmt]
   [leela.storaged.cassandra.sequence :as seq]
   [leela.storaged.cassandra.connection :as conn]))

(install-keyspace-fixtures [seq/create-schema])

(deftest test-fetch-block-empty-table
  (is (= [] (seq/fetch-block 0))))

(deftest test-fetch-block-singleton
  (is (= 1 (seq/alloc-block 0))))

(deftest test-alloc-block-concurrently
  (let [size 100
        keys (replicate size 0)
        data (pmap #(seq/alloc-block %) keys)]
    (is (= size (count (set data))))))

(deftest test-store-obj
  (is (true? (seq/store-obj 0 0 (bytes/bytes-from-chars "foobar")))))

(deftest test-store-obj-twice
  (is (true? (seq/store-obj 0 0 (bytes/bytes-from-chars "foobar"))))
  (is (false? (seq/store-obj 0 0 (bytes/bytes-from-chars "foobar")))))

(deftest test-store-obj-concurrently
  (letfn [(do-register [items]
            (let [block (seq/alloc-block 0)
                  tuples (map vector (range (count items)) items)]
              (every? true? (map #(seq/store-obj 0 (first %) (second %)) tuples))))
          (register-all [items]
            (every? true? (map do-register (partition 10 10 [] items))))]
    (let [dataset (for [a (a-z-range)
                        b (a-z-range)]
                    (bytes/bytes-from-chars [a b]))]
      (is (every? true? (pmap register-all (partition 10 10 [] dataset)))))))

(deftest test-fetch-seqid-on-empty-table
  (is (nil? (seq/fetch-seqid 0 (bytes/bytes-from-chars "foobar")))))

(deftest test-fetch-seqid-by-obj-after-store
  (let [data (bytes/bytes-from-chars "foobar")]
    (seq/store-obj 0 0 data)
    (is (= 0 (seq/fetch-seqid 0 data)))))

(deftest-pagination-token test-fetch-sequence-pagination
  {:last-fn  (comp :object last)
   :view-fn  :seqid
   :store-fn #(seq/store-obj 0 % (stmt/bigint->blob %))
   :mkrow-fn identity
   :fetch-fn (fn
               ([limit]
                (conn/with-limit limit (seq/fetch-sequence 0)))
               ([next limit]
                (conn/with-limit limit (seq/fetch-sequence 0 next))))})
