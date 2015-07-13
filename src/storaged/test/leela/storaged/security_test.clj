(ns leela.storaged.security-test
  (:import
   java.nio.ByteBuffer)
  (:require
   [clojure.test :refer :all]
   [clj-time.core :refer [now]]
   [clj-time.coerce :refer [to-long]]
   [leela.tests.helpers :refer :all]
   [leela.storaged.bytes :refer :all]
   [leela.storaged.security :refer :all]))

(deftest test-signature?
  (with-secret (random-name 32)
    (let [msg     (bytes-from-chars (random-name (+ 1 (rand-int 100))))
          sig     (signature (ByteBuffer/wrap msg))
          rnd     (bytes-from-chars (random-name signature-size))
          sig+msg (concat-bytes sig msg)
          rnd+msg (concat-bytes rnd msg)]
      (is (signature? (ByteBuffer/wrap sig+msg)))
      (is (not (signature? (ByteBuffer/wrap rnd+msg)))))))

(deftest test-check-signature!
  (with-secret (random-name 32)
    (let [msg     (bytes-from-chars (random-name (+ 1 (rand-int 100))))
          sig     (signature (ByteBuffer/wrap msg))
          sig+msg (ByteBuffer/wrap (concat-bytes sig msg))]
      (is (= (- (.remaining sig+msg) signature-size) (.remaining (check-signature! sig+msg)))))))

(deftest test-check-timestamp!
  (let [time (ByteBuffer/wrap (byte-array 8))]
    (.putLong (.slice time) (to-long (now)))
    (is (= 0 (.remaining (check-timestamp! (to-long (now)) time))))))

(deftest test-signature-short
  (with-secret (random-name 32)
    (let [msg (bytes-from-chars (random-name (rand-int signature-size)))]
      (is (not (signature? (ByteBuffer/wrap msg)))))))

(deftest test-timestamp?
  (let [time (ByteBuffer/wrap (byte-array 8))]
    (.putLong (.slice time) (to-long (now)))
    (is (timestamp? (to-long (now)) time))))

(deftest test-timestamp-limit
  (let [time (ByteBuffer/wrap (byte-array 8))
        unix (to-long (now))]
    (.putLong (.slice time) (+ unix *time-window-in-ms*))
    (is (not (timestamp? unix (.slice time))))
    (.putLong (.slice time) (- unix *time-window-in-ms*))
    (is (not (timestamp? unix (.slice time))))))

(deftest test-authenticate
  (with-secret (random-name 32)
    (let [msg  (bytes-from-chars (random-name (+ 1 (rand-int 100))))
          time (to-long (now))
          buff (ByteBuffer/wrap (byte-array (+ (count msg) signature-size 8)))
          sig  (signature (.. (.slice buff) (put msg) (putLong time)))]
      (.. (.slice buff) (put msg) (putLong time) (put sig))
      (is (= (count msg) (.remaining (authenticate buff)))))))
