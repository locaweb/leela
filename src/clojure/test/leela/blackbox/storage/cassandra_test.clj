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
  (let [node (f/uuid-1)]
    (storage/with-session [cluster ["127.0.0.1"] "leela"]

      (testing "getindex with no data"
        (is (= [] (storage/getindex cluster node false))))

      (testing "getindex after putindex"
        (storage/truncate-all cluster)
        (storage/putindex cluster node false "foobar")
        (is (= ["foobar"] (storage/getindex cluster node false))))

      (testing "getindex after multiple putindex"
        (storage/truncate-all cluster)
        (storage/putindex cluster node false "f")
        (storage/putindex cluster node false "o")
        (storage/putindex cluster node false "b")
        (storage/putindex cluster node false "a")
        (storage/putindex cluster node false "r")
        (is (= ["a" "b" "f" "o" "r"] (storage/getindex cluster node false))))

      (testing "getindex pagination with no finish"
        (storage/truncate-all cluster)
        (storage/putindex cluster node false "a0")
        (storage/putindex cluster node false "a1")
        (storage/putindex cluster node false "a2")
        (storage/with-limit 1
          (is (= ["a0"] (storage/getindex cluster node false "")))
          (is (= ["a1"] (storage/getindex cluster node false "a1")))
          (is (= ["a2"] (storage/getindex cluster node false "a2")))))

      (testing "getindex pagination with finish"
        (storage/truncate-all cluster)
        (storage/putindex cluster node false "a0")
        (storage/putindex cluster node false "a1")
        (storage/putindex cluster node false "a2")
        (storage/with-limit 1
          (is (= ["a0"] (storage/getindex cluster node false "" "a1")))
          (is (= ["a1"] (storage/getindex cluster node false "a1" "a2"))))))))

(deftest test-getlink
  (let [node-a (f/uuid-from-time 1)
        node-b (f/uuid-from-time 2)
        node-c (f/uuid-from-time 3)
        node-d (f/uuid-from-time 4)]
    (storage/with-session [cluster ["127.0.0.1"] "leela"]

      (testing "getlink with no data"
        (is (= [] (storage/getlink cluster node-a "l" (f/uuid-from-time 0)))))

      (testing "getlink after putlink"
        (storage/putlink cluster node-a "l" node-b)
        (storage/putlink cluster node-a "l" node-c)
        (storage/putlink cluster node-a "l" node-d)
        (is (= [node-b node-c node-d] (storage/getlink cluster node-a "l" (f/uuid-from-time 0)))))

      (testing "getlink pagination"
        (storage/putlink cluster node-a "l" node-b)
        (storage/putlink cluster node-a "l" node-c)
        (storage/putlink cluster node-a "l" node-d)
        (storage/with-limit 2
          (is (= [node-b node-c] (storage/getlink cluster node-a "l" (f/uuid-from-time 0))))
          (is (= [node-b node-c] (storage/getlink cluster node-a "l" node-b)))
          (is (= [node-c node-d] (storage/getlink cluster node-a "l" node-c)))
          (is (= [node-d] (storage/getlink cluster node-a "l" node-d)))))

    (testing "getlink after dellink"
      (storage/putlink cluster node-a "l" node-b)
      (storage/putlink cluster node-a "l" node-c)
      (storage/putlink cluster node-a "l" node-d)
      (storage/dellink cluster node-a "l" node-b)
      (storage/dellink cluster node-a "l" node-c)
      (storage/dellink cluster node-a "l" node-d)
      (is (= [] (storage/getlink cluster node-a "l" (f/uuid-from-time 0)))))

    (testing "getlink after dellink with no optional argument"
      (storage/putlink cluster node-a "l" node-b)
      (storage/putlink cluster node-a "l" node-c)
      (storage/putlink cluster node-a "l" node-d)
      (storage/dellink cluster node-a "l")
      (is (= [] (storage/getlink cluster node-a "l" (f/uuid-from-time 0))))))))

(deftest test-get-tattr
  (let [node-a (f/uuid-from-time 1)
        node-b (f/uuid-from-time 2)
        node-c (f/uuid-from-time 3)
        node-d (f/uuid-from-time 4)]
    (storage/with-session [cluster ["127.0.0.1"] "leela"]

      (testing "get-tattr with no data"
        (is (= [] (storage/get-tattr cluster node-a "attr"))))

      (testing "get-tattr after put-tattr"
        (storage/put-tattr cluster node-a "attr" 0 (f/str-to-bytes "foobar"))
        (let [results (storage/get-tattr cluster node-a "attr")]
          (is (= [[0 "foobar"]] (for [[k v] results] [k (f/bytes-to-str v)])))))

      (testing "get-tattr after put-tattr (overwrite)"
        (storage/put-tattr cluster node-a "attr" 0 (f/str-to-bytes "foobar"))
        (storage/put-tattr cluster node-a "attr" 0 (f/str-to-bytes "foobaz"))
        (let [results (storage/get-tattr cluster node-a "attr")]
          (is (= [[0 "foobaz"]] (for [[k v] results] [k (f/bytes-to-str v)])))))

      (testing "get-tattr after delattr"
        (storage/put-tattr cluster node-c "attr" 0 (f/str-to-bytes "foobar"))
        (storage/del-tattr cluster node-c "attr" 0)
        (is (= [] (storage/get-tattr cluster node-c "attr")))))))

(deftest test-get-kattr
  (let [node-a (f/uuid-from-time 1)
        node-b (f/uuid-from-time 2)
        node-c (f/uuid-from-time 3)
        node-d (f/uuid-from-time 4)]
    (storage/with-session [cluster ["127.0.0.1"] "leela"]

      (testing "get-kattr with no data"
        (is (= nil (storage/get-kattr cluster node-a "attr"))))

      (testing "get-kattr after put-kattr"
        (storage/put-kattr cluster node-a "attr#0" (f/str-to-bytes "foobar"))
        (is (not (= nil (storage/get-kattr cluster node-a "attr#0")))))

      (testing "get-kattr after put-kattr (overwrite)"
        (storage/put-kattr cluster node-a "attr#0" (f/str-to-bytes "foobar"))
        (storage/put-kattr cluster node-a "attr#0" (f/str-to-bytes "foobaz"))
        (is (= "foobaz" (f/bytes-to-str (storage/get-kattr cluster node-a "attr#0")))))

      (testing "getakattr after del-kattr"
        (storage/put-kattr cluster node-a "attr#0" (f/str-to-bytes "foobar"))
        (storage/del-kattr cluster node-a "attr#0")
        (is (= nil (storage/get-kattr cluster node-a "attr#0")))))))

(deftest test-naming
  (storage/with-session [cluster ["127.0.0.1"] "leela"]
    (testing "putguid register a new uuid"
      (is (not (= nil (storage/putguid cluster "leela" "leela" "foobar")))))

    (testing "getguid is idempotent"
      (is (= (storage/putguid cluster "leela" "leela" "foobaz") (storage/putguid cluster "leela" "leela" "foobaz"))))

    (testing "getguid with no data"
      (is (= nil (storage/getguid cluster "leela" "leela" "foo"))))

    (testing "getguid aftter getguid"
      (is (= (storage/putguid cluster "leela" "leela" "bar") (storage/getguid cluster "leela" "leela" "bar"))))))
