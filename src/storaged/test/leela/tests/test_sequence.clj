(ns leela.tests.test-sequence
  (:require
   [clojure.test :refer :all]
   [leela.tests.helpers :refer :all]
   [leela.storaged.bytes :as bytes]
   [leela.tests.pagination :refer :all]
   [leela.storaged.cassandra.sequence :as seq]
   [leela.storaged.cassandra.connection :as conn]))

(install-keyspace-fixtures [seq/create-schema])

(deftest test-fetch-block-empty-table
  (is (= [] (seq/fetch-block +cluster+ 0))))

(deftest test-fetch-block-singleton
  (is (= 0 (seq/alloc-block +cluster+ 0))))

(deftest test-alloc-block-concurrently
  (let [size 100
        keys (replicate size 0)
        data (pmap #(seq/alloc-block +cluster+ %) keys)]
    (is (= size (count (set data))))))

(deftest test-store-obj
  (is (true? (seq/store-obj +cluster+ 0 0 (bytes/bytes-from-chars "foobar")))))

(deftest test-store-obj-twice
  (is (true? (seq/store-obj +cluster+ 0 0 (bytes/bytes-from-chars "foobar"))))
  (is (false? (seq/store-obj +cluster+ 0 0 (bytes/bytes-from-chars "foobar")))))

(deftest test-store-obj-concurrently
  (letfn [(do-register [items]
            (let [block (seq/alloc-block +cluster+ 0)
                  tuples (map vector (range (count items)) items)]
              (every? true? (map #(seq/store-obj +cluster+ 0 (first %) (second %)) tuples))))
          (register-all [items]
            (every? true? (map do-register (partition 10 10 [] items))))]
    (let [dataset (for [a (a-z-range)
                        b (a-z-range)]
                    (bytes/bytes-from-chars [a b]))]
      (is (every? true? (pmap register-all (partition 10 10 [] dataset)))))))

(deftest test-fetch-obj-by-seqid-on-empty-table
  (is (nil? (seq/fetch-obj-by-seqid +cluster+ 0 0))))

(deftest test-fetch-seqid-by-obj-on-empty-table
  (is (nil? (seq/fetch-seqid-by-obj +cluster+ 0 (bytes/bytes-from-chars "foobar")))))

(deftest test-fetch-obj-by-seqid-after-store
  (let [data "foobar"]
    (seq/index-obj +cluster+ 0 0 (bytes/bytes-from-chars data))
    (is (= data (bytes/chars-from-bytes (seq/fetch-obj-by-seqid +cluster+ 0 0))))))

(deftest test-fetch-seqid-by-obj-after-store
  (let [data (bytes/bytes-from-chars "foobar")]
    (seq/store-obj +cluster+ 0 0 data)
    (is (= 0 (seq/fetch-seqid-by-obj +cluster+ 0 data)))))

(deftest-pagination-desc test-fetch-idx-pagination
  {:store-fn #(seq/index-obj +cluster+ 0 % (byte-array 0))
   :fetch-fn #(conn/with-limit %2 (seq/fetch-idx +cluster+ 0 %1))
   :mkrow-fn #(identity {:seqid %})
   :view-fn  #(:seqid %)})
