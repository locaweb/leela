(ns leela.storaged.cassandra.bitmap-test
  (:require
   [clojure.test :refer :all]
   [leela.tests.helpers :refer :all]
   [leela.storaged.bytes :as bytes]
   [leela.tests.pagination :refer :all]
   [leela.storaged.cassandra.bitmap :as bitmap]
   [leela.storaged.cassandra.connection :as conn]))

(install-keyspace-fixtures [bitmap/create-schema])

(deftest test-fetch-chunk-after-store-chunk
  (bitmap/store-chunk "000" (bytes/bytes-from-chars "foobar"))
  (is (= "foobar" (bytes/chars-from-bytes (bitmap/fetch-chunk "000")))))

(deftest test-fetch-chunk-on-empty-table
  (is (nil? (bitmap/fetch-chunk "000"))))

(deftest test-store-index-on-empty-table
  (is (true? (bitmap/store-index 0 "name" "leela" 0 (map str "abcde")))))

(deftest test-store-index-twice
  (is (true? (bitmap/store-index 0 "name" "leela" 0 (map str "abcde"))))
  (is (false? (bitmap/store-index 0 "name" "leela" 0 (map str "abcde")))))

(deftest test-fetch-index-after-store-index
  (bitmap/store-index 0 "name" "leela" 0 (map str "abcde"))
  (is (= [{:version 0
           :blocks (map str "abcde")}]
         (bitmap/fetch-index 0 "name" "leela"))))

(deftest test-fetch-index-by-gen-after-store-index
  (bitmap/store-index 0 "name" "leela" 0 (map str "abcde"))
  (bitmap/store-index 0 "name" "leela" 1 (map str "abcde"))
  (is (= [{:version 0
           :blocks (map str "abcde")}]
         (bitmap/fetch-index 0 "name" "leela" 1))))

(deftest-pagination-desc test-fetch-index-pagination
  {:store-fn #(bitmap/store-index 0 "key" "val" % [(str %)])
   :fetch-fn #(conn/with-limit %2 (bitmap/fetch-index 0 "key" "val" %1))
   :mkrow-fn #(identity {:version % :blocks [(str %)]})})
  