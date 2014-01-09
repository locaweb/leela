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

(deftest test-zmqserver-handle-message
  (storage/with-session [cluster ["127.0.0.1"] "leela"]

    (testing "getname with no data"
      (is (= (server/msg-fail 404) (server/handle-message cluster ["get" "name" "0x00"]))))

    (testing "getname after putname"
      (server/handle-message cluster ["put" "name" "0x00" "/system/leela" "clojure"])
      (is (= (server/msg-name ["/system/leela" "clojure"]) (server/handle-message cluster ["get" "name" "0x00"]))))

    (testing "getlabel-all with no data"
      (is (= (server/msg-label []) (server/handle-message cluster ["get" "label" "all" "0x00"]))))

    (testing "getlabel-all after putlabel"
      (server/handle-message cluster ["put" "label" "0x00" "0"])
      (server/handle-message cluster ["put" "label" "0x00" "1"])
      (server/handle-message cluster ["put" "label" "0x00" "2"])
      (is (= (server/msg-label ["0" "1" "2"]) (server/handle-message cluster ["get" "label" "all" "0x00"]))))

    (testing "getlabel-all pagination"
      (server/handle-message cluster ["put" "label" "0x00" "0"])
      (server/handle-message cluster ["put" "label" "0x00" "1"])
      (server/handle-message cluster ["put" "label" "0x00" "2"])
      (storage/with-limit 1
        (is (= (server/msg-label ["0"]) (server/handle-message cluster ["get" "label" "all" "0x00"])))
        (is (= (server/msg-label ["1"]) (server/handle-message cluster ["get" "label" "all" "0x00" "1"])))
        (is (= (server/msg-label ["2"]) (server/handle-message cluster ["get" "label" "all" "0x00" "2"])))))

    (testing "getlabel-prefix with no data"
      (is (= (server/msg-label []) (server/handle-message cluster ["get" "label" "pre" "0x01" "1" "2"]))))

    (testing "getlabel-prefix after putlabel"
      (server/handle-message cluster ["put" "label" "0x01" "00"])
      (server/handle-message cluster ["put" "label" "0x01" "01"])
      (server/handle-message cluster ["put" "label" "0x01" "02"])
      (server/handle-message cluster ["put" "label" "0x01" "10"])
      (server/handle-message cluster ["put" "label" "0x01" "11"])
      (server/handle-message cluster ["put" "label" "0x01" "12"])
      (server/handle-message cluster ["put" "label" "0x01" "20"])
      (server/handle-message cluster ["put" "label" "0x01" "21"])
      (server/handle-message cluster ["put" "label" "0x01" "22"])
      (is (= (server/msg-label ["10" "11" "12"]) (server/handle-message cluster ["get" "label" "pre" "0x01" "1" "2"]))))

    (testing "getlabel-prefix pagination"
      (server/handle-message cluster ["put" "label" "0x01" "00"])
      (server/handle-message cluster ["put" "label" "0x01" "01"])
      (server/handle-message cluster ["put" "label" "0x01" "02"])
      (server/handle-message cluster ["put" "label" "0x01" "10"])
      (server/handle-message cluster ["put" "label" "0x01" "11"])
      (server/handle-message cluster ["put" "label" "0x01" "12"])
      (server/handle-message cluster ["put" "label" "0x01" "20"])
      (server/handle-message cluster ["put" "label" "0x01" "21"])
      (server/handle-message cluster ["put" "label" "0x01" "22"])
      (storage/with-limit 1
        (is (= (server/msg-label ["10"]) (server/handle-message cluster ["get" "label" "pre" "0x01" "1" "2"])))
        (is (= (server/msg-label ["11"]) (server/handle-message cluster ["get" "label" "pre" "0x01" "11" "2"])))
        (is (= (server/msg-label ["12"]) (server/handle-message cluster ["get" "label" "pre" "0x01" "12" "2"])))))

    (testing "getlabel-suffix with no data"
      (is (= (server/msg-label []) (server/handle-message cluster ["get" "label" "suf" "0x02" "1" "2"]))))

    (testing "getlabel-suffix after putabel"
      (server/handle-message cluster ["put" "label" "0x02" "00"])
      (server/handle-message cluster ["put" "label" "0x02" "01"])
      (server/handle-message cluster ["put" "label" "0x02" "02"])
      (server/handle-message cluster ["put" "label" "0x02" "10"])
      (server/handle-message cluster ["put" "label" "0x02" "11"])
      (server/handle-message cluster ["put" "label" "0x02" "12"])
      (server/handle-message cluster ["put" "label" "0x02" "20"])
      (server/handle-message cluster ["put" "label" "0x02" "21"])
      (server/handle-message cluster ["put" "label" "0x02" "22"])
      (is (= (server/msg-label ["01" "11" "21"]) (server/handle-message cluster ["get" "label" "suf" "0x02" "1" "2"]))))

    (testing "getlabel-suffix pagination"
      (server/handle-message cluster ["put" "label" "0x02" "00"])
      (server/handle-message cluster ["put" "label" "0x02" "01"])
      (server/handle-message cluster ["put" "label" "0x02" "02"])
      (server/handle-message cluster ["put" "label" "0x02" "10"])
      (server/handle-message cluster ["put" "label" "0x02" "11"])
      (server/handle-message cluster ["put" "label" "0x02" "12"])
      (server/handle-message cluster ["put" "label" "0x02" "20"])
      (server/handle-message cluster ["put" "label" "0x02" "21"])
      (server/handle-message cluster ["put" "label" "0x02" "22"])
      (storage/with-limit 1
        (is (= (server/msg-label ["01"]) (server/handle-message cluster ["get" "label" "suf" "0x02" "1" "2"])))
        (is (= (server/msg-label ["11"]) (server/handle-message cluster ["get" "label" "suf" "0x02" "11" "2"])))
        (is (= (server/msg-label ["21"]) (server/handle-message cluster ["get" "label" "suf" "0x02" "21" "2"])))))

    (testing "getlabel-exact with no data"
      (is (= (server/msg-label []) (server/handle-message cluster ["get" "label" "ext" "0x03" "foobar"]))))

    (testing "getlabel-exact after putlabel"
      (server/handle-message cluster ["put" "label" "0x03" "foobar"])
      (is (= (server/msg-label ["foobar"]) (server/handle-message cluster ["get" "label" "ext" "0x03" "foobar"]))))

    (testing "dellink with no data"
      (is (= (server/msg-done) (server/handle-message cluster ["del" "link" "0x00" "0x"]))))

    (testing "dellink after putlink"
      (server/handle-message cluster ["put" "link" "0x00" "0x01"])
      (server/handle-message cluster ["put" "link" "0x00" "0x02"])
      (server/handle-message cluster ["put" "link" "0x00" "0x03"])
      (is (= (server/msg-done) (server/handle-message cluster ["del" "link" "0x00" "0x01"])))
      (is (= (server/msg-link ["0x02" "0x03"]) (server/handle-message cluster ["get" "link" "0x00"]))))

    (testing "dellink after putlink (all)"
      (server/handle-message cluster ["put" "link" "0x00" "0x01"])
      (server/handle-message cluster ["put" "link" "0x00" "0x02"])
      (server/handle-message cluster ["put" "link" "0x00" "0x03"])
      (is (= (server/msg-done) (server/handle-message cluster ["del" "link" "0x00"])))
      (is (= (server/msg-link []) (server/handle-message cluster ["get" "link" "0x00"]))))

    (testing "getlink with no data"
      (is (= (server/msg-link []) (server/handle-message cluster ["get" "link" "0x00" "0x"]))))

    (testing "getlink after putlink"
      (server/handle-message cluster ["put" "link" "0x00" "0x01"])
      (server/handle-message cluster ["put" "link" "0x00" "0x02"])
      (server/handle-message cluster ["put" "link" "0x00" "0x03"])
      (is (= (server/msg-link ["0x01" "0x02" "0x03"]) (server/handle-message cluster ["get" "link" "0x00" "0x"]))))

    (testing "getlink pagination"
      (server/handle-message cluster ["put" "link" "0x00" "0x01"])
      (server/handle-message cluster ["put" "link" "0x00" "0x02"])
      (server/handle-message cluster ["put" "link" "0x00" "0x03"])
      (storage/with-limit 1
        (is (= (server/msg-link ["0x01"]) (server/handle-message cluster ["get" "link" "0x00" "0x"])))
        (is (= (server/msg-link ["0x01"]) (server/handle-message cluster ["get" "link" "0x00" "0x01"])))
        (is (= (server/msg-link ["0x02"]) (server/handle-message cluster ["get" "link" "0x00" "0x02"])))
        (is (= (server/msg-link ["0x03"]) (server/handle-message cluster ["get" "link" "0x00" "0x03"])))
        (is (= (server/msg-link []) (server/handle-message cluster ["get" "link" "0x00" "0x04"])))))

    (testing "deltattr with no data"
      (is (= (server/msg-done) (server/handle-message cluster ["del" "tattr" "0x00"]))))

    (testing "gettattr after puttattr"
      (server/handle-message cluster ["put" "tattr" "0x00" "0" "0x00"])
      (server/handle-message cluster ["put" "tattr" "0x00" "1" "0x01"])
      (is (= (server/msg-tattr [0 "0x00" 1 "0x01"]) (server/handle-message cluster ["get" "tattr" "0x00"]))))

    (testing "deltattr after puttattr"
      (server/handle-message cluster ["put" "tattr" "0x00" "0" "0x00"])
      (is (= (server/msg-done) (server/handle-message cluster ["del" "tattr" "0x00"])))
      (is (= (server/msg-tattr []) (server/handle-message cluster ["get" "tattr" "0x00"]))))

    (testing "delkattr with no data"
      (is (= (server/msg-done) (server/handle-message cluster ["del" "kattr" "0x00"]))))

    (testing "getkattr after putkattr"
      (server/handle-message cluster ["put" "kattr" "0x00" "0" "0x00"])
      (server/handle-message cluster ["put" "kattr" "0x00" "1" "0x01"])
      (is (= (server/msg-kattr ["0x00"]) (server/handle-message cluster ["get" "kattr" "0x00" "0"])))
      (is (= (server/msg-kattr ["0x01"]) (server/handle-message cluster ["get" "kattr" "0x00" "1"]))))

    (testing "delkattr after putkattr"
      (server/handle-message cluster ["put" "kattr" "0x00" "0" "0x00"])
      (is (= (server/msg-done) (server/handle-message cluster ["del" "kattr" "0x00"])))
      (is (= (server/msg-kattr []) (server/handle-message cluster ["get" "kattr" "0x00" "0"]))))
    ))

(deftest test-zmqserver-zmqworker-interface
  (testing "just checks the interface"
    (is (:onjob (server/zmqworker nil)))
    (is (:onerr (server/zmqworker nil)))))
