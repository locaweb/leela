(ns leela.blackbox.storage.cassandra-test
  (:use     [clojure.test]
            [clojurewerkz.cassaforte.query]
            [clojurewerkz.cassaforte.embedded]
            [clojurewerkz.cassaforte.multi.cql])
  (:require [leela.blackbox.f :as f]
            [clojurewerkz.cassaforte.client :as client]
            [leela.blackbox.storage.cassandra :as storage]))

(defmacro dbg[x] `(let [x# ~x] (println "dbg:" '~x "=" x#) x#))

(defmacro truncate-n-test [cluster name & body]
  `(testing ~name
     (storage/truncate-all ~cluster)
     ~@body))

(deftest test-getindex
  (let [node (f/uuid-1)]
    (storage/with-session [cluster ["127.0.0.1"] "leela"]

      (truncate-n-test cluster "getindex with no data"
        (is (= [] (storage/getindex cluster node false))))

      (truncate-n-test cluster "getindex after putindex"
        (storage/putindex cluster [{:key node :name "foobar"}])
        (is (= ["foobar"] (storage/getindex cluster node false))))

      (truncate-n-test cluster "getindex after multiple putindex"
        (storage/putindex cluster [{:key node :name "f"}
                                   {:key node :name "o"}
                                   {:key node :name "b"}
                                   {:key node :name "a"}
                                   {:key node :name "r"}])
        (is (= ["a" "b" "f" "o" "r"] (storage/getindex cluster node false))))

      (truncate-n-test cluster "getindex pagination with only start"
        (storage/putindex cluster [{:key node :name "a0"}
                                   {:key node :name "a1"}
                                   {:key node :name "a2"}])
        (storage/with-limit 1
          (is (= ["a0"] (storage/getindex cluster node false)))
          (is (= ["a1"] (storage/getindex cluster node false "a1")))
          (is (= ["a2"] (storage/getindex cluster node false "a2")))))

      (truncate-n-test cluster "getindex pagination with start & finish"
        (storage/putindex cluster [{:key node :name "fooba0r"}
                                   {:key node :name "fooba1r"}
                                   {:key node :name "fooba2r"}])
        (is (= ["fooba0r"] (storage/getindex cluster node false "fooba0" "fooba1")))
        (is (= ["fooba1r"] (storage/getindex cluster node false "fooba1" "fooba2")))
        (is (= ["fooba2r"] (storage/getindex cluster node false "fooba2" "fooba3"))))

      (truncate-n-test cluster "getindex (reverse)"
        (storage/putindex cluster [{:key node :name "a0"}
                                   {:key node :name "a1"}])
        (is (= ["a0" "a1"] (storage/getindex cluster node true))))

      (truncate-n-test cluster "getindex (reverse) pagination with only start"
        (storage/putindex cluster [{:key node :name "a0"}
                                   {:key node :name "a1"}
                                   {:key node :name "a2"}])
        (storage/with-limit 1
          (is (= ["a0"] (storage/getindex cluster node true)))
          (is (= ["a1"] (storage/getindex cluster node true "a1")))
          (is (= ["a2"] (storage/getindex cluster node true "a2")))))

      (truncate-n-test cluster "getindex (reverse) pagination with start & finish"
        (storage/putindex cluster [{:key node :name "fooba0r"}
                                   {:key node :name "fooba1r"}
                                   {:key node :name "fooba2r"}])
        (is (= ["fooba0r"] (storage/getindex cluster node true "0r" "1r")))
        (is (= ["fooba1r"] (storage/getindex cluster node true "1r" "2r")))
        (is (= ["fooba2r"] (storage/getindex cluster node true "2r" "3r")))))))

(deftest test-getlink
  (let [node-a (f/uuid-from-time 1)
        node-b (f/uuid-from-time 2)
        node-c (f/uuid-from-time 3)
        node-d (f/uuid-from-time 4)]
    (storage/with-session [cluster ["127.0.0.1"] "leela"]

      (truncate-n-test cluster "getlink with no data"
        (is (= [] (storage/getlink cluster node-a "l" (f/uuid-from-time 0)))))

      (truncate-n-test cluster "getlink after putlink"
        (storage/putlink cluster [{:a node-a :l "l" :b node-b}
                                  {:a node-a :l "l" :b node-c}
                                  {:a node-a :l "l" :b node-d}])
        (is (= [node-b node-c node-d] (storage/getlink cluster node-a "l" (f/uuid-from-time 0)))))

      (truncate-n-test cluster "getlink pagination"
        (storage/putlink cluster [{:a node-a :l "l" :b node-b}
                                  {:a node-a :l "l" :b node-c}
                                  {:a node-a :l "l" :b node-d}])
        (storage/with-limit 2
          (is (= [node-b node-c] (storage/getlink cluster node-a "l" (f/uuid-from-time 0))))
          (is (= [node-b node-c] (storage/getlink cluster node-a "l" node-b)))
          (is (= [node-c node-d] (storage/getlink cluster node-a "l" node-c)))
          (is (= [node-d] (storage/getlink cluster node-a "l" node-d)))))

      (truncate-n-test cluster "getlink after dellink"
        (storage/putlink cluster [{:a node-a :l "l" :b node-b}
                                  {:a node-a :l "l" :b node-c}
                                  {:a node-a :l "l" :b node-d}])
        (storage/dellink cluster [[node-a "l" node-b]
                                  [node-a "l" node-c]
                                  [node-a "l" node-d]])
        (is (= [] (storage/getlink cluster node-a "l" (f/uuid-from-time 0)))))

      (truncate-n-test cluster "getlink after dellink with no optional argument"
        (storage/putlink cluster [{:a node-a :l "l" :b node-b}
                                  {:a node-a :l "l" :b node-c}
                                  {:a node-a :l "l" :b node-d}])
        (storage/dellink cluster [[node-a "l" nil]])
        (is (= [] (storage/getlink cluster node-a "l" (f/uuid-from-time 0))))))))

(deftest test-get-tattr
  (let [node-a (f/uuid-from-time 1)
        node-b (f/uuid-from-time 2)
        node-c (f/uuid-from-time 3)
        node-d (f/uuid-from-time 4)]
    (storage/with-session [cluster ["127.0.0.1"] "leela"]

      (truncate-n-test cluster "get-tattr with no data"
        (is (= [] (storage/get-tattr cluster node-a "attr"))))

      (truncate-n-test cluster "get-tattr after put-tattr"
        (storage/put-tattr cluster node-a "attr" 0 (f/str-to-bytes "foobar"))
        (let [results (storage/get-tattr cluster node-a "attr")]
          (is (= [[0 "foobar"]] (for [[k v] results] [k (f/bytes-to-str v)])))))

      (truncate-n-test cluster "get-tattr after put-tattr (overwrite)"
        (storage/put-tattr cluster node-a "attr" 0 (f/str-to-bytes "foobar"))
        (storage/put-tattr cluster node-a "attr" 0 (f/str-to-bytes "foobaz"))
        (let [results (storage/get-tattr cluster node-a "attr")]
          (is (= [[0 "foobaz"]] (for [[k v] results] [k (f/bytes-to-str v)])))))

      (truncate-n-test cluster "get-tattr after delattr"
        (storage/put-tattr cluster node-c "attr" 0 (f/str-to-bytes "foobar"))
        (storage/del-tattr cluster node-c "attr" 0)
        (is (= [] (storage/get-tattr cluster node-c "attr")))))))

(deftest test-get-kattr
  (let [node-a (f/uuid-from-time 1)
        node-b (f/uuid-from-time 2)
        node-c (f/uuid-from-time 3)
        node-d (f/uuid-from-time 4)]
    (storage/with-session [cluster ["127.0.0.1"] "leela"]

      (truncate-n-test cluster "get-kattr with no data"
        (is (= nil (storage/get-kattr cluster node-a "attr"))))

      (truncate-n-test cluster "get-kattr after put-kattr"
        (storage/put-kattr cluster node-a "attr#0" (f/str-to-bytes "foobar"))
        (is (not (= nil (storage/get-kattr cluster node-a "attr#0")))))

      (truncate-n-test cluster "get-kattr after put-kattr (overwrite)"
        (storage/put-kattr cluster node-a "attr#0" (f/str-to-bytes "foobar"))
        (storage/put-kattr cluster node-a "attr#0" (f/str-to-bytes "foobaz"))
        (is (= "foobaz" (f/bytes-to-str (storage/get-kattr cluster node-a "attr#0")))))

      (truncate-n-test cluster "getakattr after del-kattr"
        (storage/put-kattr cluster node-a "attr#0" (f/str-to-bytes "foobar"))
        (storage/del-kattr cluster node-a "attr#0")
        (is (= nil (storage/get-kattr cluster node-a "attr#0")))))))

(deftest test-naming
  (storage/with-session [cluster ["127.0.0.1"] "leela"]
    (truncate-n-test cluster "putguid register a new uuid"
      (is (not (= nil (storage/putguid cluster "leela" "leela" "foobar")))))

    (truncate-n-test cluster "getguid is idempotent"
      (is (= (storage/putguid cluster "leela" "leela" "foobaz") (storage/putguid cluster "leela" "leela" "foobaz"))))

    (truncate-n-test cluster "getguid with no data"
      (is (= nil (storage/getguid cluster "leela" "leela" "foo"))))

    (truncate-n-test cluster "getguid aftter getguid"
      (is (= (storage/putguid cluster "leela" "leela" "bar") (storage/getguid cluster "leela" "leela" "bar"))))))
