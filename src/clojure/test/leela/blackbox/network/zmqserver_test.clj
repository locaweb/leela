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

(deftest test-zmqserver-handle-name-message
  (storage/with-session [cluster ["127.0.0.1"] "leela"]

    (testing "getname with no data"
      (is (= (server/msg-fail 404) (server/handle-message cluster ["get" "name" "00"]))))

    (testing "getname after putname"
      (server/handle-message cluster ["put" "name" "00" "/system/leela" "clojure"])
      (is (= (server/msg-name ["/system/leela" "clojure"]) (server/handle-message cluster ["get" "name" "00"]))))))

(deftest test-zmqserver-handle-label-all-message
  (storage/with-session [cluster ["127.0.0.1"] "leela"]

    (testing "getlabel-all with no data"
      (is (= (server/msg-label []) (server/handle-message cluster ["get" "label" "all" "00"]))))

    (testing "getlabel-all after putlabel"
      (server/handle-message cluster ["put" "label" "00" "0"])
      (server/handle-message cluster ["put" "label" "00" "1"])
      (server/handle-message cluster ["put" "label" "00" "2"])
      (is (= (server/msg-label ["0" "1" "2"]) (server/handle-message cluster ["get" "label" "all" "00"]))))

    (testing "getlabel-all pagination"
      (server/handle-message cluster ["put" "label" "00" "0"])
      (server/handle-message cluster ["put" "label" "00" "1"])
      (server/handle-message cluster ["put" "label" "00" "2"])
      (storage/with-limit 1
        (is (= (server/msg-label ["0"]) (server/handle-message cluster ["get" "label" "all" "00"])))
        (is (= (server/msg-label ["1"]) (server/handle-message cluster ["get" "label" "all" "00" "1"])))
        (is (= (server/msg-label ["2"]) (server/handle-message cluster ["get" "label" "all" "00" "2"])))))))

(deftest test-zmqserver-handle-label-prefix-message
  (storage/with-session [cluster ["127.0.0.1"] "leela"]

    (testing "getlabel-prefix with no data"
      (is (= (server/msg-label []) (server/handle-message cluster ["get" "label" "pre" "01" "1" "2"]))))

    (testing "getlabel-prefix after putlabel"
      (server/handle-message cluster ["put" "label" "01" "00"])
      (server/handle-message cluster ["put" "label" "01" "01"])
      (server/handle-message cluster ["put" "label" "01" "02"])
      (server/handle-message cluster ["put" "label" "01" "10"])
      (server/handle-message cluster ["put" "label" "01" "11"])
      (server/handle-message cluster ["put" "label" "01" "12"])
      (server/handle-message cluster ["put" "label" "01" "20"])
      (server/handle-message cluster ["put" "label" "01" "21"])
      (server/handle-message cluster ["put" "label" "01" "22"])
      (is (= (server/msg-label ["10" "11" "12"]) (server/handle-message cluster ["get" "label" "pre" "01" "1" "2"]))))

    (testing "getlabel-prefix pagination"
      (server/handle-message cluster ["put" "label" "01" "00"])
      (server/handle-message cluster ["put" "label" "01" "01"])
      (server/handle-message cluster ["put" "label" "01" "02"])
      (server/handle-message cluster ["put" "label" "01" "10"])
      (server/handle-message cluster ["put" "label" "01" "11"])
      (server/handle-message cluster ["put" "label" "01" "12"])
      (server/handle-message cluster ["put" "label" "01" "20"])
      (server/handle-message cluster ["put" "label" "01" "21"])
      (server/handle-message cluster ["put" "label" "01" "22"])
      (storage/with-limit 1
        (is (= (server/msg-label ["10"]) (server/handle-message cluster ["get" "label" "pre" "01" "1" "2"])))
        (is (= (server/msg-label ["11"]) (server/handle-message cluster ["get" "label" "pre" "01" "11" "2"])))
        (is (= (server/msg-label ["12"]) (server/handle-message cluster ["get" "label" "pre" "01" "12" "2"])))))))

(deftest test-zmqserver-handle-label-suffix-message
  (storage/with-session [cluster ["127.0.0.1"] "leela"]

    (testing "getlabel-suffix with no data"
      (is (= (server/msg-label []) (server/handle-message cluster ["get" "label" "suf" "02" "1" "2"]))))

    (testing "getlabel-suffix after putabel"
      (server/handle-message cluster ["put" "label" "02" "00"])
      (server/handle-message cluster ["put" "label" "02" "01"])
      (server/handle-message cluster ["put" "label" "02" "02"])
      (server/handle-message cluster ["put" "label" "02" "10"])
      (server/handle-message cluster ["put" "label" "02" "11"])
      (server/handle-message cluster ["put" "label" "02" "12"])
      (server/handle-message cluster ["put" "label" "02" "20"])
      (server/handle-message cluster ["put" "label" "02" "21"])
      (server/handle-message cluster ["put" "label" "02" "22"])
      (is (= (server/msg-label ["01" "11" "21"]) (server/handle-message cluster ["get" "label" "suf" "02" "1" "2"]))))

    (testing "getlabel-suffix pagination"
      (server/handle-message cluster ["put" "label" "02" "00"])
      (server/handle-message cluster ["put" "label" "02" "01"])
      (server/handle-message cluster ["put" "label" "02" "02"])
      (server/handle-message cluster ["put" "label" "02" "10"])
      (server/handle-message cluster ["put" "label" "02" "11"])
      (server/handle-message cluster ["put" "label" "02" "12"])
      (server/handle-message cluster ["put" "label" "02" "20"])
      (server/handle-message cluster ["put" "label" "02" "21"])
      (server/handle-message cluster ["put" "label" "02" "22"])
      (storage/with-limit 1
        (is (= (server/msg-label ["01"]) (server/handle-message cluster ["get" "label" "suf" "02" "1" "2"])))
        (is (= (server/msg-label ["11"]) (server/handle-message cluster ["get" "label" "suf" "02" "11" "2"])))
        (is (= (server/msg-label ["21"]) (server/handle-message cluster ["get" "label" "suf" "02" "21" "2"])))))))

(deftest test-zmqserver-handle-label-exact-message
  (storage/with-session [cluster ["127.0.0.1"] "leela"]

    (testing "getlabel-exact with no data"
      (is (= (server/msg-label []) (server/handle-message cluster ["get" "label" "ext" "03" "foobar"]))))

    (testing "getlabel-exact after putlabel"
      (server/handle-message cluster ["put" "label" "03" "foobar"])
      (is (= (server/msg-label ["foobar"]) (server/handle-message cluster ["get" "label" "ext" "03" "foobar"]))))))

(deftest test-zmqserver-handle-getlink-message
  (storage/with-session [cluster ["127.0.0.1"] "leela"]

    (testing "getlink with no data"
      (is (= (server/msg-link []) (server/handle-message cluster ["get" "link" "00" "l" ""]))))

    (testing "getlink after putlink"
      (server/handle-message cluster ["put" "link" "00" "l" "01"])
      (server/handle-message cluster ["put" "link" "00" "l" "02"])
      (server/handle-message cluster ["put" "link" "00" "l" "03"])
      (is (= (map seq (server/msg-link [(f/bin-0x01) (f/bin-0x02) (f/bin-0x03)])) (map seq (server/handle-message cluster ["get" "link" "00" "l" ""])))))

    (testing "getlink pagination"
      (server/handle-message cluster ["put" "link" "00" "l" "01"])
      (server/handle-message cluster ["put" "link" "00" "l" "02"])
      (server/handle-message cluster ["put" "link" "00" "l" "03"])
      (storage/with-limit 1
        (is (= (map seq (server/msg-link [(f/bin-0x01)])) (map seq (server/handle-message cluster ["get" "link" "00" "l" ""]))))
        (is (= (map seq (server/msg-link [(f/bin-0x01)])) (map seq (server/handle-message cluster ["get" "link" "00" "l" "01"]))))
        (is (= (map seq (server/msg-link [(f/bin-0x02)])) (map seq (server/handle-message cluster ["get" "link" "00" "l" "02"]))))
        (is (= (map seq (server/msg-link [(f/bin-0x03)])) (map seq (server/handle-message cluster ["get" "link" "00" "l" "03"]))))
        (is (= (map seq (server/msg-link [])) (map seq (server/handle-message cluster ["get" "link" "00" "l" "04"]))))))))

(deftest test-zmqserver-handle-t-attr-message
  (storage/with-session [cluster ["127.0.0.1"] "leela"]

    (testing "get-tattr with no data"
      (is (= (server/msg-tattr []) (server/handle-message cluster ["get" "t-attr" "00" "attr"]))))

    (testing "get-tattr after put-tattr"
      (server/handle-message cluster ["put" "t-attr" "00" "attr" "0" "00"])
      (is (= (server/msg-tattr [[0 (f/bin-0x00)]]) (server/handle-message cluster ["get" "t-attr" "00" "attr"]))))

    (testing "get-tattr after del-tattr"
      (server/handle-message cluster ["put" "t-attr" "00" "name" "0" "00"])
      (is (= (server/msg-done) (server/handle-message cluster ["del" "t-attr" "00" "name" "0"])))
      (is (= (server/msg-tattr []) (server/handle-message cluster ["get" "t-attr" "00" "name"]))))

    (testing "get-tattr limit"
      (server/handle-message cluster ["put" "t-attr" "00" "name" "0" "00"])
      (server/handle-message cluster ["put" "t-attr" "00" "name" "1" "00"])
      (server/handle-message cluster ["put" "t-attr" "00" "name" "2" "00"])
      (is (= 3 (count (server/handle-message cluster ["get" "t-attr" "00" "name" "1"]))))
      (is (= 5 (count (server/handle-message cluster ["get" "t-attr" "00" "name" "2"]))))
      (is (= 7 (count (server/handle-message cluster ["get" "t-attr" "00" "name" "3"])))))))

(deftest test-zmqserver-handle-k-attr-message
  (storage/with-session [cluster ["127.0.0.1"] "leela"]

    (testing "get-kattr with no data"
      (is (= (server/msg-kattr nil) (server/handle-message cluster ["get" "k-attr" "00" "0"]))))

    (testing "get-kattr after put-kattr"
      (is (= (server/msg-done) (server/handle-message cluster ["put" "k-attr" "00" "attr" "00"])))
      (is (= (server/msg-kattr (f/bin-0x00)) (server/handle-message cluster ["get" "k-attr" "00" "attr"]))))

    (testing "del-kattr after put-kattr"
      (server/handle-message cluster ["put" "k-attr" "00" "0" "00"])
      (is (= (server/msg-done) (server/handle-message cluster ["del" "k-attr" "00" "0"])))
      (is (= (server/msg-kattr nil) (server/handle-message cluster ["get" "k-attr" "00" "0"]))))))

(deftest test-zmqserver-zmqworker-interface
  (testing "just checks the interface"
    (is (:onjob (server/zmqworker nil)))
    (is (:onerr (server/zmqworker nil)))))
