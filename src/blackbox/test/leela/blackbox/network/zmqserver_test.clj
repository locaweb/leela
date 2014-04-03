(ns leela.blackbox.network.zmqserver-test
  (:use     [clojure.test])
  (:require [leela.blackbox.f :as f]
            [leela.blackbox.storage.cassandra :as storage]
            [leela.blackbox.network.zmqserver :as server]))

(defmacro truncate-n-test [cluster name & body]
  `(testing ~name
     (storage/truncate-all ~cluster)
     ~@body))

(deftest test-zmqserver-handle-put-name-msg
  (storage/with-session [cluster ["127.0.0.1"] "leela"]

    (truncate-n-test cluster "putname returns guid"
      (let [[msg user tree name guid] (server/handle-message cluster ["put" "name" "user" "tree" "foobar"])]
        (is (= "name" msg))
        (is (= "user" user))
        (is (= "tree" tree))
        (is (= "foobar" name))
        (is guid)))

    (truncate-n-test cluster "putname is idempotent"
      (is (= (server/handle-message cluster ["put" "name" "user" "tree" "foobar"]) (server/handle-message cluster ["put" "name" "user" "tree" "foobar"]))))

    (truncate-n-test cluster "getguid after putname"
      (is (= (server/handle-message cluster ["put" "name" "user" "tree" "foobar"]) (server/handle-message cluster ["get" "guid" "user" "tree" "foobar"]))))

    (truncate-n-test cluster "getname after putname"
      (let [[_ _ _ _ guid] (server/handle-message cluster ["put" "name" "user" "tree" "foobar"])]
        (is (= (server/handle-message cluster ["put" "name" "user" "tree" "foobar"]) (server/handle-message cluster ["get" "name" guid])))))))

(deftest test-zmqserver-handle-label-all-message
  (let [node (str (f/uuid-1))]
    (storage/with-session [cluster ["127.0.0.1"] "leela"]

      (truncate-n-test cluster "getlabel-all with no data"
        (is (= (server/msg-label []) (server/handle-message cluster ["get" "label" "all" node]))))

      (truncate-n-test cluster "getlabel-all after putlabel"
        (server/handle-message cluster ["put" "label" node "0"])
        (server/handle-message cluster ["put" "label" node "1"])
        (server/handle-message cluster ["put" "label" node "2"])
        (is (= (server/msg-label ["0" "1" "2"]) (server/handle-message cluster ["get" "label" "all" node]))))

      (truncate-n-test cluster "getlabel-all pagination"
        (server/handle-message cluster ["put" "label" node "0"])
        (server/handle-message cluster ["put" "label" node "1"])
        (server/handle-message cluster ["put" "label" node "2"])
        (is (= (server/msg-label ["0"]) (server/handle-message cluster ["get" "label" "all" node "" "1"])))
        (is (= (server/msg-label ["1"]) (server/handle-message cluster ["get" "label" "all" node "1" "1"])))
        (is (= (server/msg-label ["2"]) (server/handle-message cluster ["get" "label" "all" node "2" "1"])))))))

(deftest test-zmqserver-handle-label-prefix-message
  (let [node (str (f/uuid-1))]
    (storage/with-session [cluster ["127.0.0.1"] "leela"]

      (truncate-n-test cluster "getlabel-prefix with no data"
        (is (= (server/msg-label []) (server/handle-message cluster ["get" "label" "pre" node "1" "2"]))))

      (truncate-n-test cluster "getlabel-prefix after putlabel"
        (server/handle-message cluster ["put" "label" node "00"])
        (server/handle-message cluster ["put" "label" node "01"])
        (server/handle-message cluster ["put" "label" node "02"])
        (server/handle-message cluster ["put" "label" node "10"])
        (server/handle-message cluster ["put" "label" node "11"])
        (server/handle-message cluster ["put" "label" node "12"])
        (server/handle-message cluster ["put" "label" node "20"])
        (server/handle-message cluster ["put" "label" node "21"])
        (server/handle-message cluster ["put" "label" node "22"])
        (is (= (server/msg-label ["10" "11" "12"]) (server/handle-message cluster ["get" "label" "pre" node "1" "2"]))))

      (truncate-n-test cluster "getlabel-prefix pagination"
        (server/handle-message cluster ["put" "label" node "00"])
        (server/handle-message cluster ["put" "label" node "01"])
        (server/handle-message cluster ["put" "label" node "02"])
        (server/handle-message cluster ["put" "label" node "10"])
        (server/handle-message cluster ["put" "label" node "11"])
        (server/handle-message cluster ["put" "label" node "12"])
        (server/handle-message cluster ["put" "label" node "20"])
        (server/handle-message cluster ["put" "label" node "21"])
        (server/handle-message cluster ["put" "label" node "22"])
        (storage/with-limit 1
          (is (= (server/msg-label ["10"]) (server/handle-message cluster ["get" "label" "pre" node "1" "2"])))
          (is (= (server/msg-label ["11"]) (server/handle-message cluster ["get" "label" "pre" node "11" "2"])))
          (is (= (server/msg-label ["12"]) (server/handle-message cluster ["get" "label" "pre" node "12" "2"]))))))))

(deftest test-zmqserver-handle-label-suffix-message
  (let [node (str (f/uuid-1))]
    (storage/with-session [cluster ["127.0.0.1"] "leela"]

      (truncate-n-test cluster "getlabel-suffix with no data"
        (is (= (server/msg-label []) (server/handle-message cluster ["get" "label" "suf" node "1" "2"]))))

      (truncate-n-test cluster "getlabel-suffix after putabel"
        (server/handle-message cluster ["put" "label" node "00"])
        (server/handle-message cluster ["put" "label" node "01"])
        (server/handle-message cluster ["put" "label" node "02"])
        (server/handle-message cluster ["put" "label" node "10"])
        (server/handle-message cluster ["put" "label" node "11"])
        (server/handle-message cluster ["put" "label" node "12"])
        (server/handle-message cluster ["put" "label" node "20"])
        (server/handle-message cluster ["put" "label" node "21"])
        (server/handle-message cluster ["put" "label" node "22"])
        (is (= (server/msg-label ["01" "11" "21"]) (server/handle-message cluster ["get" "label" "suf" node "1" "2"]))))

      (truncate-n-test cluster "getlabel-suffix pagination"
        (server/handle-message cluster ["put" "label" node "00"])
        (server/handle-message cluster ["put" "label" node "01"])
        (server/handle-message cluster ["put" "label" node "02"])
        (server/handle-message cluster ["put" "label" node "10"])
        (server/handle-message cluster ["put" "label" node "11"])
        (server/handle-message cluster ["put" "label" node "12"])
        (server/handle-message cluster ["put" "label" node "20"])
        (server/handle-message cluster ["put" "label" node "21"])
        (server/handle-message cluster ["put" "label" node "22"])
        (is (= (server/msg-label ["01"]) (server/handle-message cluster ["get" "label" "suf" node "1" "2" "1"])))
        (is (= (server/msg-label ["11"]) (server/handle-message cluster ["get" "label" "suf" node "11" "2" "1"])))
        (is (= (server/msg-label ["21"]) (server/handle-message cluster ["get" "label" "suf" node "21" "2" "1"])))))))

(deftest test-zmqserver-handle-label-exact-message
  (let [node (str (f/uuid-1))]
    (storage/with-session [cluster ["127.0.0.1"] "leela"]

      (truncate-n-test cluster "getlabel-exact with no data"
        (is (= (server/msg-label []) (server/handle-message cluster ["get" "label" "ext" node "foobar"]))))

      (truncate-n-test cluster "getlabel-exact after putlabel"
        (server/handle-message cluster ["put" "label" node "foobar"])
        (is (= (server/msg-label ["foobar"]) (server/handle-message cluster ["get" "label" "ext" node "foobar"])))))))

(deftest test-zmqserver-handle-getlink-message
  (let [node-a (str (f/uuid-from-time 1))
        node-b (str (f/uuid-from-time 2))
        node-c (str (f/uuid-from-time 3))
        node-d (str (f/uuid-from-time 4))
        node-e (str (f/uuid-from-time 5))]
    (storage/with-session [cluster ["127.0.0.1"] "leela"]

      (truncate-n-test cluster "getlink with no data"
      (is (= (server/msg-link []) (server/handle-message cluster ["get" "link" node-a "l" ""]))))

    (truncate-n-test cluster "getlink after putlink"
      (server/handle-message cluster ["put" "link" node-a "l" node-b node-a "l" node-c node-a "l" node-d])
      (is (= (map seq (server/msg-link [node-b node-c node-d])) (map seq (server/handle-message cluster ["get" "link" node-a "l" ""])))))

    (truncate-n-test cluster "getlink after dellink"
      (server/handle-message cluster ["put" "link" node-a "l" node-b])
      (server/handle-message cluster ["del" "link" node-a "l" node-b node-a "l" node-c node-a "l" node-d])
      (is (= (server/msg-link []) (server/handle-message cluster ["get" "link" node-a "l" ""]))))

    (truncate-n-test cluster "getlink after dellink"
      (server/handle-message cluster ["put" "link" node-a "l" node-b])
      (server/handle-message cluster ["del" "link" node-a "l" ""])
      (is (= (server/msg-link []) (server/handle-message cluster ["get" "link" node-a "l" ""]))))

    (truncate-n-test cluster "getlink pagination"
      (server/handle-message cluster ["put" "link" node-a "l" node-b node-a "l" node-c node-a "l" node-d])
      (is (= (server/msg-link [node-b node-c]) (server/handle-message cluster ["get" "link" node-a "l" "" "2"])))
      (is (= (server/msg-link [node-b node-c]) (server/handle-message cluster ["get" "link" node-a "l" node-b "2"])))
      (is (= (server/msg-link [node-c node-d]) (server/handle-message cluster ["get" "link" node-a "l" node-c "2"])))
      (is (= (server/msg-link [node-d]) (server/handle-message cluster ["get" "link" node-a "l" node-d "2"])))
      (is (= (server/msg-link []) (server/handle-message cluster ["get" "link" node-a "l" node-e "2"])))))))

(deftest test-zmqserver-handle-t-attr-message
  (let [node (str (f/uuid-1))
        value (f/str-to-bytes "foobar")]
    (storage/with-session [cluster ["127.0.0.1"] "leela"]

      (truncate-n-test cluster "get-tattr with no data"
        (is (= (server/msg-tattr []) (server/handle-message cluster ["get" "t-attr" node "attr" "0"]))))

      (truncate-n-test cluster "get-tattr after put-tattr"
        (server/handle-message cluster ["put" "t-attr" node "attr" "0" value ""])
        (is (= (server/msg-tattr [[0 "foobar"]]) (map f/bytes-to-str (server/handle-message cluster ["get" "t-attr" node "attr" "0"])))))

      (truncate-n-test cluster "get-tattr after del-tattr"
        (server/handle-message cluster ["put" "t-attr" node "name" "0" value ""])
        (is (= (server/msg-done) (server/handle-message cluster ["del" "t-attr" node "name" "0"])))
        (is (= (server/msg-tattr []) (server/handle-message cluster ["get" "t-attr" node "name" "0"]))))

      (truncate-n-test cluster "get-tattr after del-tattr (selective)"
        (server/handle-message cluster ["put" "t-attr"
                                        node "name" "0" value ""
                                        node "name" "1" value ""])
        (is (= (server/msg-done) (server/handle-message cluster ["del" "t-attr" node "name" "0"])))
        (is (= (server/msg-tattr [[1 "foobar"]]) (map f/bytes-to-str (server/handle-message cluster ["get" "t-attr" node "name" "0"]))))))))

(deftest test-zmqserver-handle-k-attr-message
  (let [node (str (f/uuid-1))
        value (f/str-to-bytes "foobar")]
    (storage/with-session [cluster ["127.0.0.1"] "leela"]

      (truncate-n-test cluster "get-kattr with no data"
        (is (= (server/msg-fail 404) (map f/bytes-to-str (server/handle-message cluster ["get" "k-attr" node "attr"])))))

      (truncate-n-test cluster "get-kattr after put-kattr with ttl"
        (is (= (server/msg-done) (server/handle-message cluster ["put" "k-attr" node "attr" value "ttl:3600"])))
        (is (= (server/msg-kattr "foobar") (map f/bytes-to-str (server/handle-message cluster ["get" "k-attr" node "attr"])))))

      (truncate-n-test cluster "get-kattr after put-kattr"
        (is (= (server/msg-done) (server/handle-message cluster ["put" "k-attr" node "attr" value ""])))
        (is (= (server/msg-kattr "foobar") (map f/bytes-to-str (server/handle-message cluster ["get" "k-attr" node "attr"])))))

      (truncate-n-test cluster "del-kattr after put-kattr"
        (server/handle-message cluster ["put" "k-attr" node "attr" value ""])
        (is (= (server/msg-done) (server/handle-message cluster ["del" "k-attr" node "attr"])))
        (is (= (server/msg-fail 404) (map f/bytes-to-str (server/handle-message cluster ["get" "k-attr" node "attr"]))))))))

(deftest test-zmqserver-handle-attr-message
  (let [node (str (f/uuid-1))
        value (f/str-to-bytes "foobar")]
    (storage/with-session [cluster ["127.0.0.1"] "leela"]

      (truncate-n-test cluster "get-attr with no data"
        (is (= (server/msg-nattr []) (map f/bytes-to-str (server/handle-message cluster ["get" "attr" "k-attr" "all" node ""])))))

      (truncate-n-test cluster "get-attr after put-attr with ttl"
        (is (= (server/msg-done) (server/handle-message cluster ["put" "k-attr" node "attr#1" value "ttl:3600, index:true" node "attr#2" value "ttl:3600, index:true"])))
        (is (= (server/msg-nattr ["attr#1" "attr#2"]) (map f/bytes-to-str (server/handle-message cluster ["get" "attr" "k-attr" "all" node ""])))))

      (truncate-n-test cluster "get-attr after put-kattr (index:false)"
        (is (= (server/msg-done) (server/handle-message cluster ["put" "k-attr" node "attr#1" value "" node "attr#2" value ""])))
        (is (= (server/msg-nattr []) (map f/bytes-to-str (server/handle-message cluster ["get" "attr" "k-attr" "all" node ""])))))

      (truncate-n-test cluster "get-attr after put-kattr (index:true)"
        (is (= (server/msg-done) (server/handle-message cluster ["put" "k-attr" node "attr#1" value "index:true" node "attr#2" value "index:true"])))
        (is (= (server/msg-nattr ["attr#1" "attr#2"]) (map f/bytes-to-str (server/handle-message cluster ["get" "attr" "k-attr" "all" node ""])))))

      (truncate-n-test cluster "get-attr after del-kattr"
        (server/handle-message cluster ["put" "k-attr" node "attr" value "index:true"])
        (is (= (server/msg-done) (server/handle-message cluster ["del" "k-attr" node "attr"])))
        (is (= (server/msg-nattr []) (map f/bytes-to-str (server/handle-message cluster ["get" "attr" "k-attr" "all" node ""]))))))))

(deftest test-zmqserver-zmqworker-interface
  (testing "just checks the interface"
    (is (:onjob (server/zmqworker nil)))
    (is (:onerr (server/zmqworker nil)))))
