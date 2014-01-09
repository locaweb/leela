(ns leela.blackbox.storage.cassandra-test
  (:use     [clojure.test]
            [clojurewerkz.cassaforte.query]
            [clojurewerkz.cassaforte.embedded]
            [clojurewerkz.cassaforte.multi.cql])
  (:require [leela.blackbox.f :as f]
            [clojurewerkz.cassaforte.client :as client]
            [leela.blackbox.storage.cassandra :as storage]))

(defn storage-cleanup [f]
  (storage/with-session [cluster ["127.0.0.1"] "leela"]
    (storage/truncate-all cluster))
  (f))

(defmacro dbg[x] `(let [x# ~x] (println "dbg:" '~x "=" x#) x#))

(use-fixtures :each storage-cleanup)

(def b-zero (f/hexstr-to-bytes "0x00"))
(def b-one (f/hexstr-to-bytes "0x01"))
(def b-two (f/hexstr-to-bytes "0x02"))
(def b-three (f/hexstr-to-bytes "0x03"))
(def b-four (f/hexstr-to-bytes "0x04"))
(def b-null (f/hexstr-to-bytes "0x"))

(deftest test-getindex
  (storage/with-session [cluster ["127.0.0.1"] "leela"]

    (testing "getindex with no data"
      (is (= [] (storage/getindex cluster b-zero 0))))

    (testing "getindex after putindex"
      (storage/putindex cluster b-zero 0 "foobar")
      (is (= ["foobar"] (storage/getindex cluster b-zero 0))))

    (testing "getindex after multiple putindex"
      (storage/putindex cluster b-zero 1 "f")
      (storage/putindex cluster b-zero 1 "o")
      (storage/putindex cluster b-zero 1 "b")
      (storage/putindex cluster b-zero 1 "a")
      (storage/putindex cluster b-zero 1 "r")
      (is (= ["a" "b" "f" "o" "r"] (storage/getindex cluster b-zero 1))))

    (testing "getindex pagination with no finish"
      (storage/putindex cluster b-zero 2 "a0")
      (storage/putindex cluster b-zero 2 "a1")
      (storage/putindex cluster b-zero 2 "a2")
      (storage/with-limit 1
        (is (= ["a0"] (storage/getindex cluster b-zero 2 "")))
        (is (= ["a1"] (storage/getindex cluster b-zero 2 "a1")))
        (is (= ["a2"] (storage/getindex cluster b-zero 2 "a2")))))

    (testing "getindex pagination with finish"
      (storage/putindex cluster b-zero 2 "a0")
      (storage/putindex cluster b-zero 2 "a1")
      (storage/putindex cluster b-zero 2 "a2")
      (storage/with-limit 1
        (is (= ["a0"] (storage/getindex cluster b-zero 2 "" "a1")))
        (is (= ["a1"] (storage/getindex cluster b-zero 2 "a1" "a2")))))))

(deftest test-getlink
  (storage/with-session [cluster ["127.0.0.1"] "leela"]

    (testing "getlink with no data"
      (is (= [] (storage/getlink cluster b-zero "l" b-null))))

    (testing "getlink after putlink"
      (storage/putlink cluster b-zero "l" b-one)
      (storage/putlink cluster b-zero "l" b-two)
      (storage/putlink cluster b-zero "l" b-three)
      (is (= ["0x01" "0x02" "0x03"] (map f/bytes-to-hexstr (storage/getlink cluster b-zero "l" b-null)))))

    (testing "getlink pagination"
      (storage/putlink cluster b-zero "l" b-one)
      (storage/putlink cluster b-zero "l" b-two)
      (storage/putlink cluster b-zero "l" b-three)
      (storage/with-limit 1
        (is (= ["0x01"] (map f/bytes-to-hexstr (storage/getlink cluster b-zero "l" b-null))))
        (is (= ["0x01"] (map f/bytes-to-hexstr (storage/getlink cluster b-zero "l" b-one))))
        (is (= ["0x02"] (map f/bytes-to-hexstr (storage/getlink cluster b-zero "l" b-two))))
        (is (= ["0x03"] (map f/bytes-to-hexstr (storage/getlink cluster b-zero "l" b-three))))
        (is (= [] (storage/getlink cluster b-zero "l" b-four)))))

    (testing "getlink after dellink"
      (storage/putlink cluster b-zero "l" b-one)
      (storage/putlink cluster b-zero "l" b-two)
      (storage/putlink cluster b-zero "l" b-three)

      (storage/dellink cluster b-zero "l" b-one)
      (storage/dellink cluster b-zero "l" b-two)
      (storage/dellink cluster b-zero "l" b-three)
      (is (= [] (storage/getlink cluster b-zero "l" b-null))))

    (testing "getlink after dellink with no optional argument"
      (storage/putlink cluster b-zero "l" b-one)
      (storage/putlink cluster b-zero "l" b-two)
      (storage/putlink cluster b-zero "l" b-three)

      (storage/dellink cluster b-zero "l")
      (is (= [] (storage/getlink cluster b-zero "l" b-null))))))

(deftest test-gettattr
  (storage/with-session [cluster ["127.0.0.1"] "leela"]

    (testing "gettattr with no data"
      (is (= {} (storage/gettattr cluster b-zero))))

    (testing "gettattr after puttattr"
      (storage/puttattr cluster b-zero 0 b-one)
      (let [results (storage/gettattr cluster b-zero)]
        (is (= [0] (keys results)))
        (is (= ["0x01"] (map f/bytes-to-hexstr (vals results))))))

    (testing "gettattr after puttattr (overwrite)"
      (storage/puttattr cluster b-zero 0 b-one)
      (storage/puttattr cluster b-zero 0 b-two)
      (let [results (storage/gettattr cluster b-zero)]
        (is (= [0] (keys results)))
        (is (= ["0x02"] (map f/bytes-to-hexstr (vals results))))))

    (testing "gettattr after delattr"
      (storage/puttattr cluster b-two 0 b-one)
      (storage/deltattr cluster b-two 0)
      (is (= {} (storage/gettattr cluster b-two))))))

(deftest test-getkattr
  (storage/with-session [cluster ["127.0.0.1"] "leela"]

    (testing "getkattr with no data"
      (is (= nil (storage/getkattr cluster b-zero "attr"))))

    (testing "getkattr after putkattr"
      (storage/putkattr cluster b-zero "attr#0" (f/hexstr-to-bytes "0x00"))
      (is (not (= nil (storage/getkattr cluster b-zero "attr#0")))))

    (testing "getkattr after putkattr (overwrite)"
      (storage/putkattr cluster b-zero "attr#0" (f/hexstr-to-bytes "0x00"))
      (storage/putkattr cluster b-zero "attr#0" (f/hexstr-to-bytes "0x01"))
      (is (= "0x01" (f/bytes-to-hexstr (storage/getkattr cluster b-zero "attr#0")))))

    (testing "getakattr after delkattr"
      (storage/putkattr cluster b-zero "attr#0" (f/hexstr-to-bytes "0x00"))
      (storage/delkattr cluster b-zero "attr#0")
      (is (= nil (storage/getkattr cluster b-zero "attr#0"))))))
