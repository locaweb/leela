(ns leela.blackbox.network.zmqserver-test
  (:use     [clojure.test])
  (:require [leela.blackbox.f :as f]
            [leela.blackbox.config :as cfg]
            [leela.blackbox.storage.cassandra :as storage]
            [leela.blackbox.network.zmqserver :as server]))

(defn storage-cleanup [f]
  (storage/with-session [cluster "127.0.0.1" "leela"]
    (storage/truncate-all cluster))
  (f))

(use-fixtures :each storage-cleanup)

(deftest test-zmqserver-handle-message
  (storage/with-session [cluster "127.0.0.1" "leela"]

    (testing "getname with no data"
      (is (= (server/msg-fail 404) (server/handle-message cluster {"code" 0 "data" "0x00"}))))

    (testing "getname after putname"
      (server/handle-message cluster {"code" 1 "data" ["/system/leela" "clojure" "0x00"]})
      (is (= (server/msg-name "/system/leela" "clojure") (server/handle-message cluster {"code" 0 "data" "0x00"}))))

    (testing "getlabel with no data"
      (is (= (server/msg-fail 404) (server/handle-message cluster {"code" 2 "data" "0x00"}))))

    (testing "getlabel after putlabel"
      (server/handle-message cluster {"code" 3 "data" ["0x00" ["l0" "l1" "l2"]]})
      (is (= (server/msg-label ["l0" "l1" "l2"]) (server/handle-message cluster {"code" 2 "data" "0x00"}))))

    (testing "getlink with no data"
      (is (= (server/msg-fail 404)) (server/handle-message cluster {"code" 4 "data" "0x00"})))

    (testing "getlink after putlink"
      (server/handle-message cluster {"code" 5 "data" ["0x00" ["0x01" "0x02" "0x03"]]})
      (is (= (server/msg-link ["0x01" "0x02" "0x03"])) (server/handle-message cluster {"code" 4 "data" "0x00"})))))

(deftest test-zmqserver-zmqworker-interface
  (testing "just checks the interface"
    (is (:onjob (server/zmqworker nil)))
    (is (:onerr (server/zmqworker nil)))))
