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

(deftest test-getindex
  (storage/with-session [cluster ["127.0.0.1"] "leela"]

    (testing "getindex with no data"
      (is (= [] (storage/getindex cluster f/b-0x00 0))))

    (testing "getindex after putindex"
      (storage/putindex cluster f/b-0x00 0 "foobar")
      (is (= ["foobar"] (storage/getindex cluster f/b-0x00 0))))

    (testing "getindex after multiple putindex"
      (storage/putindex cluster f/b-0x00 1 "f")
      (storage/putindex cluster f/b-0x00 1 "o")
      (storage/putindex cluster f/b-0x00 1 "b")
      (storage/putindex cluster f/b-0x00 1 "a")
      (storage/putindex cluster f/b-0x00 1 "r")
      (is (= ["a" "b" "f" "o" "r"] (storage/getindex cluster f/b-0x00 1))))

    (testing "getindex pagination with no finish"
      (storage/putindex cluster f/b-0x00 2 "a0")
      (storage/putindex cluster f/b-0x00 2 "a1")
      (storage/putindex cluster f/b-0x00 2 "a2")
      (storage/with-limit 1
        (is (= ["a0"] (storage/getindex cluster f/b-0x00 2 "")))
        (is (= ["a1"] (storage/getindex cluster f/b-0x00 2 "a1")))
        (is (= ["a2"] (storage/getindex cluster f/b-0x00 2 "a2")))))

    (testing "getindex pagination with finish"
      (storage/putindex cluster f/b-0x00 2 "a0")
      (storage/putindex cluster f/b-0x00 2 "a1")
      (storage/putindex cluster f/b-0x00 2 "a2")
      (storage/with-limit 1
        (is (= ["a0"] (storage/getindex cluster f/b-0x00 2 "" "a1")))
        (is (= ["a1"] (storage/getindex cluster f/b-0x00 2 "a1" "a2")))))))

(deftest test-getlink
  (storage/with-session [cluster ["127.0.0.1"] "leela"]

    (testing "getlink with no data"
      (is (= [] (storage/getlink cluster f/b-0x00 "l" f/b-0x))))

    (testing "getlink after putlink"
      (storage/putlink cluster f/b-0x00 "l" f/b-0x01)
      (storage/putlink cluster f/b-0x00 "l" f/b-0x02)
      (storage/putlink cluster f/b-0x00 "l" f/b-0x03)
      (is (= ["01" "02" "03"] (map f/binary-to-hexstr (storage/getlink cluster f/b-0x00 "l" f/b-0x)))))

    (testing "getlink pagination"
      (storage/putlink cluster f/b-0x00 "l" f/b-0x01)
      (storage/putlink cluster f/b-0x00 "l" f/b-0x02)
      (storage/putlink cluster f/b-0x00 "l" f/b-0x03)
      (storage/with-limit 1
        (is (= ["01"] (map f/binary-to-hexstr (storage/getlink cluster f/b-0x00 "l" f/b-0x))))
        (is (= ["01"] (map f/binary-to-hexstr (storage/getlink cluster f/b-0x00 "l" f/b-0x01))))
        (is (= ["02"] (map f/binary-to-hexstr (storage/getlink cluster f/b-0x00 "l" f/b-0x02))))
        (is (= ["03"] (map f/binary-to-hexstr (storage/getlink cluster f/b-0x00 "l" f/b-0x03))))
        (is (= [] (storage/getlink cluster f/b-0x00 "l" f/b-0x04)))))

    (testing "getlink after dellink"
      (storage/putlink cluster f/b-0x00 "l" f/b-0x01)
      (storage/putlink cluster f/b-0x00 "l" f/b-0x02)
      (storage/putlink cluster f/b-0x00 "l" f/b-0x03)

      (storage/dellink cluster f/b-0x00 "l" f/b-0x01)
      (storage/dellink cluster f/b-0x00 "l" f/b-0x02)
      (storage/dellink cluster f/b-0x00 "l" f/b-0x03)
      (is (= [] (storage/getlink cluster f/b-0x00 "l" f/b-0x))))

    (testing "getlink after dellink with no optional argument"
      (storage/putlink cluster f/b-0x00 "l" f/b-0x01)
      (storage/putlink cluster f/b-0x00 "l" f/b-0x02)
      (storage/putlink cluster f/b-0x00 "l" f/b-0x03)

      (storage/dellink cluster f/b-0x00 "l")
      (is (= [] (storage/getlink cluster f/b-0x00 "l" f/b-0x))))))

(deftest test-get-tattr
  (storage/with-session [cluster ["127.0.0.1"] "leela"]

    (testing "get-tattr with no data"
      (is (= {} (storage/get-tattr cluster f/b-0x00))))

    (testing "get-tattr after put-tattr"
      (storage/put-tattr cluster f/b-0x00 0 f/b-0x01)
      (let [results (storage/get-tattr cluster f/b-0x00)]
        (is (= [0] (keys results)))
        (is (= ["01"] (map f/binary-to-hexstr (vals results))))))

    (testing "get-tattr after put-tattr (overwrite)"
      (storage/put-tattr cluster f/b-0x00 0 f/b-0x01)
      (storage/put-tattr cluster f/b-0x00 0 f/b-0x02)
      (let [results (storage/get-tattr cluster f/b-0x00)]
        (is (= [0] (keys results)))
        (is (= ["02"] (map f/binary-to-hexstr (vals results))))))

    (testing "get-tattr after delattr"
      (storage/put-tattr cluster f/b-0x02 0 f/b-0x01)
      (storage/del-tattr cluster f/b-0x02 0)
      (is (= {} (storage/get-tattr cluster f/b-0x02))))))

(deftest test-get-kattr
  (storage/with-session [cluster ["127.0.0.1"] "leela"]

    (testing "get-kattr with no data"
      (is (= nil (storage/get-kattr cluster f/b-0x00 "attr"))))

    (testing "get-kattr after put-kattr"
      (storage/put-kattr cluster f/b-0x00 "attr#0" f/b-0x00)
      (is (not (= nil (storage/get-kattr cluster f/b-0x00 "attr#0")))))

    (testing "get-kattr after put-kattr (overwrite)"
      (storage/put-kattr cluster f/b-0x00 "attr#0" f/b-0x00)
      (storage/put-kattr cluster f/b-0x00 "attr#0" f/b-0x01)
      (is (= "01" (f/binary-to-hexstr (storage/get-kattr cluster f/b-0x00 "attr#0")))))

    (testing "getakattr after del-kattr"
      (storage/put-kattr cluster f/b-0x00 "attr#0" f/b-0x00)
      (storage/del-kattr cluster f/b-0x00 "attr#0")
      (is (= nil (storage/get-kattr cluster f/b-0x00 "attr#0"))))))
