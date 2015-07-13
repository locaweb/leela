(ns leela.storaged.bytes-test
  (:import
   java.nio.ByteBuffer)
  (:require
   [clojure.test :refer :all]
   [leela.tests.helpers :refer :all]
   [leela.storaged.bytes :refer :all]))

(deftest test-bytes-from-chars
  (let [msg (random-name 100)]
    (is (= msg (chars-from-bytes (bytes-from-chars msg))))))

(deftest test-bytes-from-bytebuff
  (let [msg (bytes-from-chars (random-name 100))]
    (is (= (seq msg) (seq (bytes-from-bytebuff (bytebuff-from-bytes msg)))))))

(deftest test-concat-bytes
  (let [msg1  (random-name 100)
        msg2  (random-name 100)
        bytes (concat-bytes (bytes-from-chars msg1) (bytes-from-chars msg2))]
    (is (= (seq (bytes-from-chars (concat msg1 msg2))) (seq bytes)))))
