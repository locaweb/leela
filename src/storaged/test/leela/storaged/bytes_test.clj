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

(deftest test-bytes-from-long
  (let [num   (long (* (rand) Long/MAX_VALUE))
        noise (bytes-from-chars (random-name 100))
        bnum  (concat-bytes (bytes-from-long num) noise)]
    (is (= num (long-from-bytes-only bnum)))
    (is (= (seq noise) (seq (second (long-from-bytes bnum)))))))

(deftest test-bstr-from-chars
  (let [msg   (random-name (rand-int (int (Math/pow 2 Byte/SIZE))))
        noise (bytes-from-chars (random-name 100))
        bstr  (concat-bytes (bstr-from-chars msg) noise)]
    (is (= msg (chars-from-bstr-only bstr)))
    (is (= (seq noise) (seq (second (chars-from-bstr bstr)))))))

(deftest test-bytes=-on-same-object-must-yield-true
  (let [x (byte-array (repeatedly (rand-int 1000) #(rand-int 127)))]
    (is (bytes= x x))))

(deftest test-base64-byte-function-compose-to-identity
  (let [msg (bytes-from-chars (random-name (inc (rand-int 1000))))]
    (is (bytes= msg (bytes-from-base64 (base64-from-bytes msg))))))

(deftest test-base64?
  (let [msg (bytes-from-chars (random-name (inc (rand-int 1000))))]
    (is (base64? (base64-from-bytes msg)))))
