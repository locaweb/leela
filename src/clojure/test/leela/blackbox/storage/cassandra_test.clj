(ns leela.blackbox.storage.cassandra-test
  (:use     [clojure.test]
            [clojurewerkz.cassaforte.query]
            [clojurewerkz.cassaforte.embedded]
            [clojurewerkz.cassaforte.multi.cql])
  (:require [leela.blackbox.f :as f]
            [leela.blackbox.config :as cfg]
            [clojurewerkz.cassaforte.client :as client]
            [leela.blackbox.storage.cassandra :as storage]))

(defn storage-cleanup [f]
  (storage/with-session [cluster]
    (storage/truncate-all cluster))
  (f))

(use-fixtures :each storage-cleanup)

(deftest test-cassandra-backend
  (storage/with-session [cluster]

    (testing "getname with no data"
      (is (= nil (storage/getname cluster "0x00"))))

    (testing "getname after putname"
      (storage/putname cluster "foobar" "0x00")
      (is (= "foobar" (storage/getname cluster "0x00"))))
    
    (testing "getlabel with no data"
      (is (= [] (storage/getlabel cluster "0x00"))))

    (testing "getlabel after putlabel"
      (storage/putlabel cluster "0x00" ["l0" "l1" "l2"])
      (is (= ["l0" "l1" "l2"] (storage/getlabel cluster "0x00"))))

    (testing "getlink with no data"
      (is (= [] (storage/getlink cluster "0x00"))))

    (testing "getlink after putlink"
      (storage/putlink cluster "0x00" ["0x01" "0x02" "0x03"])
      (is (= ["0x01" "0x02" "0x03"] (storage/getlink cluster "0x00"))))))
