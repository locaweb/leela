(ns leela.blackbox.network.zmqserver-test
  (:use     [clojure.test])
  (:require [leela.blackbox.f :as f]
            [leela.blackbox.storage.cassandra :as storage]
            [leela.blackbox.network.zmqserver :as server]))

(defn storage-cleanup [f]
  (storage/with-session [cluster ["127.0.0.1"] "leela"]
    (storage/truncate-all cluster))
  (f))

(use-fixtures :each storage-cleanup)

(deftest test-zmqserver-handle-put-name-msg
  (storage/with-session [cluster ["127.0.0.1"] "leela"]

    (testing "putname returns guid"
      (let [[msg user tree name guid] (server/handle-message cluster ["put" "name" "user" "tree" "foobar"])]
        (is (= "name" msg))
        (is (= "user" user))
        (is (= "tree" tree))
        (is (= "foobar" name))
        (is guid)))

    (testing "putname is idempotent"
      (is (= (server/handle-message cluster ["put" "name" "user" "tree" "foobar"]) (server/handle-message cluster ["put" "name" "user" "tree" "foobar"]))))

    (testing "getguid after putname"
      (is (= (server/handle-message cluster ["put" "name" "user" "tree" "foobar"]) (server/handle-message cluster ["get" "guid" "user" "tree" "foobar"]))))

    (testing "getname after putname"
      (let [[_ _ _ _ guid] (server/handle-message cluster ["put" "name" "user" "tree" "foobar"])]
        (is (= (server/handle-message cluster ["put" "name" "user" "tree" "foobar"]) (server/handle-message cluster ["get" "name" guid])))))))

(deftest test-zmqserver-handle-label-all-message
  (let [node (str (f/uuid-1))]
    (storage/with-session [cluster ["127.0.0.1"] "leela"]

      (testing "getlabel-all with no data"
        (is (= (server/msg-label []) (server/handle-message cluster ["get" "label" "all" node]))))

      (testing "getlabel-all after putlabel"
        (server/handle-message cluster ["put" "label" node "0"])
        (server/handle-message cluster ["put" "label" node "1"])
        (server/handle-message cluster ["put" "label" node "2"])
        (is (= (server/msg-label ["0" "1" "2"]) (server/handle-message cluster ["get" "label" "all" node]))))

      (testing "getlabel-all pagination"
        (server/handle-message cluster ["put" "label" node "0"])
        (server/handle-message cluster ["put" "label" node "1"])
        (server/handle-message cluster ["put" "label" node "2"])
        (is (= (server/msg-label ["0"]) (server/handle-message cluster ["get" "label" "all" node "" "1"])))
        (is (= (server/msg-label ["1"]) (server/handle-message cluster ["get" "label" "all" node "1" "1"])))
        (is (= (server/msg-label ["2"]) (server/handle-message cluster ["get" "label" "all" node "2" "1"])))))))

(deftest test-zmqserver-handle-label-prefix-message
  (let [node (str (f/uuid-1))]
    (storage/with-session [cluster ["127.0.0.1"] "leela"]

      (testing "getlabel-prefix with no data"
        (is (= (server/msg-label []) (server/handle-message cluster ["get" "label" "pre" node "1" "2"]))))

      (testing "getlabel-prefix after putlabel"
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

      (testing "getlabel-prefix pagination"
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

      (testing "getlabel-suffix with no data"
        (is (= (server/msg-label []) (server/handle-message cluster ["get" "label" "suf" node "1" "2"]))))

      (testing "getlabel-suffix after putabel"
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

      (testing "getlabel-suffix pagination"
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
          (is (= (server/msg-label ["01"]) (server/handle-message cluster ["get" "label" "suf" node "1" "2"])))
          (is (= (server/msg-label ["11"]) (server/handle-message cluster ["get" "label" "suf" node "11" "2"])))
          (is (= (server/msg-label ["21"]) (server/handle-message cluster ["get" "label" "suf" node "21" "2"]))))))))

(deftest test-zmqserver-handle-label-exact-message
  (let [node (str (f/uuid-1))]
    (storage/with-session [cluster ["127.0.0.1"] "leela"]

      (testing "getlabel-exact with no data"
        (is (= (server/msg-label []) (server/handle-message cluster ["get" "label" "ext" node "foobar"]))))

      (testing "getlabel-exact after putlabel"
        (server/handle-message cluster ["put" "label" node "foobar"])
        (is (= (server/msg-label ["foobar"]) (server/handle-message cluster ["get" "label" "ext" node "foobar"])))))))

(deftest test-zmqserver-handle-getlink-message
  (let [node-a (str (f/uuid-from-time 1))
        node-b (str (f/uuid-from-time 2))
        node-c (str (f/uuid-from-time 3))
        node-d (str (f/uuid-from-time 4))
        node-e (str (f/uuid-from-time 5))]
    (storage/with-session [cluster ["127.0.0.1"] "leela"]

      (testing "getlink with no data"
      (is (= (server/msg-link []) (server/handle-message cluster ["get" "link" node-a "l" ""]))))

    (testing "getlink after putlink"
      (server/handle-message cluster ["put" "link" node-a "l" node-b node-a "l" node-c node-a "l" node-d])
      (is (= (map seq (server/msg-link [node-b node-c node-d])) (map seq (server/handle-message cluster ["get" "link" node-a "l" ""])))))

    (testing "getlink after dellink"
      (server/handle-message cluster ["put" "link" node-a "l" node-b])
      (server/handle-message cluster ["del" "link" node-a "l" node-b node-a "l" node-c node-a "l" node-d])
      (is (= (server/msg-link []) (server/handle-message cluster ["get" "link" node-a "l" ""]))))

    (testing "getlink after dellink"
      (server/handle-message cluster ["put" "link" node-a "l" node-b])
      (server/handle-message cluster ["del" "link" node-a "l" ""])
      (is (= (server/msg-link []) (server/handle-message cluster ["get" "link" node-a "l" ""]))))

    (testing "getlink pagination"
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

      (testing "get-tattr with no data"
        (is (= (server/msg-tattr []) (server/handle-message cluster ["get" "t-attr" node "attr"]))))

      (testing "get-tattr after put-tattr"
        (server/handle-message cluster ["put" "t-attr" node "attr" "0" value])
        (is (= (server/msg-tattr [[0 "foobar"]]) (map #(f/bytes-to-str %) (server/handle-message cluster ["get" "t-attr" node "attr"])))))

      (testing "get-tattr after del-tattr"
        (server/handle-message cluster ["put" "t-attr" node "name" "0" value])
        (is (= (server/msg-done) (server/handle-message cluster ["del" "t-attr" node "name" "0"])))
        (is (= (server/msg-tattr []) (server/handle-message cluster ["get" "t-attr" node "name"]))))

      (testing "get-tattr limit"
        (server/handle-message cluster ["put" "t-attr" node "name" "0" value])
        (server/handle-message cluster ["put" "t-attr" node "name" "1" value])
        (server/handle-message cluster ["put" "t-attr" node "name" "2" value])
        (is (= 3 (count (server/handle-message cluster ["get" "t-attr" node "name" "1"]))))
        (is (= 5 (count (server/handle-message cluster ["get" "t-attr" node "name" "2"]))))
        (is (= 7 (count (server/handle-message cluster ["get" "t-attr" node "name" "3"]))))))))

(deftest test-zmqserver-handle-k-attr-message
  (let [node (str (f/uuid-1))
        value (f/str-to-bytes "foobar")]
    (storage/with-session [cluster ["127.0.0.1"] "leela"]

      (testing "get-kattr with no data"
        (is (= (server/msg-kattr "") (map #(f/bytes-to-str %) (server/handle-message cluster ["get" "k-attr" node "attr"])))))

      (testing "get-kattr after put-kattr"
        (is (= (server/msg-done) (server/handle-message cluster ["put" "k-attr" node "attr" value])))
        (is (= (server/msg-kattr "foobar") (map #(f/bytes-to-str %) (server/handle-message cluster ["get" "k-attr" node "attr"])))))

      (testing "del-kattr after put-kattr"
        (server/handle-message cluster ["put" "k-attr" node "attr" value])
        (is (= (server/msg-done) (server/handle-message cluster ["del" "k-attr" node "attr"])))
        (is (= (server/msg-kattr "") (map #(f/bytes-to-str %) (server/handle-message cluster ["get" "k-attr" node "attr"]))))))))

(deftest test-zmqserver-zmqworker-interface
  (testing "just checks the interface"
    (is (:onjob (server/zmqworker nil)))
    (is (:onerr (server/zmqworker nil)))))
