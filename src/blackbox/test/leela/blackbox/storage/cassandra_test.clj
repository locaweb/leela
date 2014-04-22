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

(deftest test-get-gindex
  (let [node (f/uuid-1)
        table (rand-nth [:g_index :k_index])]
    (storage/with-session [cluster ["127.0.0.1"] "leela"]

      (truncate-n-test cluster "get-gindex with no data"
        (is (= [] (storage/get-index cluster table node))))

      (truncate-n-test cluster "get-gindex after put-gindex"
        (storage/put-index cluster table [{:key node :name "foobar"}])
        (is (= ["foobar"] (storage/get-index cluster table node))))

      (truncate-n-test cluster "get-gindex after multiple put-gindex"
        (storage/put-index cluster table [{:key node :name "f"}
                                          {:key node :name "o"}
                                          {:key node :name "b"}
                                          {:key node :name "a"}
                                          {:key node :name "r"}])
        (is (= ["a" "b" "f" "o" "r"] (storage/get-index cluster table node))))

      (truncate-n-test cluster "get-gindex pagination with only start"
        (storage/put-index cluster table [{:key node :name "a0"}
                                          {:key node :name "a1"}
                                          {:key node :name "a2"}])
        (storage/with-limit 1
          (is (= ["a0"] (storage/get-index cluster table node)))
          (is (= ["a1"] (storage/get-index cluster table node "a1")))
          (is (= ["a2"] (storage/get-index cluster table node "a2")))))

      (truncate-n-test cluster "get-gindex pagination with start & finish"
        (storage/put-index cluster table [{:key node :name "fooba0r"}
                                          {:key node :name "fooba1r"}
                                          {:key node :name "fooba2r"}])
        (is (= ["fooba0r"] (storage/get-index cluster table node "fooba0" "fooba1")))
        (is (= ["fooba1r"] (storage/get-index cluster table node "fooba1" "fooba2")))
        (is (= ["fooba2r"] (storage/get-index cluster table node "fooba2" "fooba3")))))))

(deftest test-put-tattr
  (let [node-a (f/uuid-from-time 1)]
    (storage/with-session [cluster ["127.0.0.1"] "leela"]

      (truncate-n-test cluster "put-tattr without index"
         (storage/put-tattr cluster [[{:key node-a
                                       :name "foobar"
                                       :time 0
                                       :value (f/str-to-bytes "foobar")} {}]])
         (is (= [] (storage/get-index cluster :t_index node-a false))))

      (truncate-n-test cluster "put-tattr with index"
         (storage/put-tattr cluster [[{:key node-a
                                       :name "foobar"
                                       :time 0
                                       :value (f/str-to-bytes "foobar")} {:index true}]])
         (is (= ["foobar"] (storage/get-index cluster :t_index node-a false)))))))

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
        (storage/dellink cluster [{:a node-a :l "l" :b node-b}
                                  {:a node-a :l "l" :b node-c}
                                  {:a node-a :l "l" :b node-d}])
        (is (= [] (storage/getlink cluster node-a "l" (f/uuid-from-time 0)))))

      (truncate-n-test cluster "getlink after dellink with no optional argument"
        (storage/putlink cluster [{:a node-a :l "l" :b node-b}
                                  {:a node-a :l "l" :b node-c}
                                  {:a node-a :l "l" :b node-d}])
        (storage/dellink cluster [{:a node-a :l "l" :b nil}])
        (is (= [] (storage/getlink cluster node-a "l" (f/uuid-from-time 0))))))))

(deftest test-get-tattr
  (let [node-a (f/uuid-from-time 1)
        node-b (f/uuid-from-time 2)
        node-c (f/uuid-from-time 3)
        node-d (f/uuid-from-time 4)]
    (storage/with-session [cluster ["127.0.0.1"] "leela"]

      (truncate-n-test cluster "get-tattr with no data"
        (is (= [] (storage/get-tattr cluster node-a "attr" 0))))

      (truncate-n-test cluster "get-tattr after put-tattr"
        (storage/put-tattr cluster [[{:key node-a
                                      :name "attr"
                                      :time 0
                                      :value (f/str-to-bytes "foobar")} {}]])
        (let [results (storage/get-tattr cluster node-a "attr" 0)]
          (is (= [[0 "foobar"]] (map (partial map f/bytes-to-str) results)))))

      (truncate-n-test cluster "get-tattr after put-tattr (overwrite)"
        (storage/put-tattr cluster [[{:key node-a
                                      :name "attr"
                                      :time 0
                                      :value (f/str-to-bytes "foobar")} {}]
                                    [{:key node-a
                                      :name "attr"
                                      :time 0
                                      :value (f/str-to-bytes "foobaz")} {}]])
        (let [results (storage/get-tattr cluster node-a "attr" 0)]
          (is (= [[0 "foobaz"]] (map (partial map f/bytes-to-str) results)))))

      (truncate-n-test cluster "get-tattr after delattr"
        (storage/put-tattr cluster [[{:key node-c
                                      :name "attr"
                                      :time 0
                                      :value (f/str-to-bytes "foobar")} {}]])
        (storage/del-tattr cluster [{:key node-c
                                     :name "attr"
                                     :time 0}])
        (is (= [] (storage/get-tattr cluster node-c "attr" 0)))))))

(deftest test-put-kattr
  (let [node-a (f/uuid-from-time 1)]
    (storage/with-session [cluster ["127.0.0.1"] "leela"]

      (truncate-n-test cluster "put-kattr without index"
         (storage/put-kattr cluster [[{:key node-a
                                       :name "foobar"
                                       :value (f/str-to-bytes "foobar")} {}]])
         (is (= [] (storage/get-index cluster :k_index node-a false))))

      (truncate-n-test cluster "put-kattr with index"
         (storage/put-kattr cluster [[{:key node-a
                                       :name "foobar"
                                       :value (f/str-to-bytes "foobar")} {:index true}]])
         (is (= ["foobar"] (storage/get-index cluster :k_index node-a false)))))))

(deftest test-get-kattr
  (let [node-a (f/uuid-from-time 1)
        node-b (f/uuid-from-time 2)
        node-c (f/uuid-from-time 3)
        node-d (f/uuid-from-time 4)]
    (storage/with-session [cluster ["127.0.0.1"] "leela"]

      (truncate-n-test cluster "get-kattr with no data"
        (is (= nil (storage/get-kattr cluster node-a "attr"))))

      (truncate-n-test cluster "get-kattr after put-kattr"
        (storage/put-kattr cluster [[{:key node-a :name "attr#0" :value (f/str-to-bytes "foobar")} {}]])
        (is (not (= nil (storage/get-kattr cluster node-a "attr#0")))))

      (truncate-n-test cluster "get-kattr after put-kattr (overwrite)"
        (storage/put-kattr cluster [[{:key node-a :name "attr#0" :value (f/str-to-bytes "foobar")} {}]
                                    [{:key node-a :name "attr#0" :value (f/str-to-bytes "foobaz")} {}]])
        (is (= "foobaz" (f/bytes-to-str (storage/get-kattr cluster node-a "attr#0")))))

      (truncate-n-test cluster "getakattr after del-kattr"
        (storage/put-kattr cluster [[{:key node-a :name "attr#0" :value (f/str-to-bytes "foobar")} {}]])
        (storage/del-kattr cluster [{:key node-a :name "attr#0"}])
        (is (= nil (storage/get-kattr cluster node-a "attr#0")))))))

(deftest test-naming
  (storage/with-session [cluster ["127.0.0.1"] "leela"]
    (truncate-n-test cluster "putguid register a new uuid"
      (is (not (= nil (storage/putguid cluster "leela" "leela" "foo" "bar")))))

    (truncate-n-test cluster "getguid is idempotent"
      (is (= (storage/putguid cluster "leela" "leela" "foo" "bar") (storage/putguid cluster "leela" "leela" "foo" "bar"))))

    (truncate-n-test cluster "getguid with no data"
      (is (empty? (storage/getguid cluster "leela" "leela" "foo" "bar"))))

    (truncate-n-test cluster "getguid with no data"
      (is (= nil (storage/getguid cluster "leela" "leela" "foo" "bar"))))

    (truncate-n-test cluster "getguid aftter putguid"
      (is (= (storage/putguid cluster "leela" "leela" "foo" "bar") (storage/getguid cluster "leela" "leela" "foo" "bar"))))))
