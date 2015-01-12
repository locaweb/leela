(ns leela.blackbox.network.zmqserver-test
  (:use     [clojure.test]
            [clojure.tools.logging :only [trace debug info error]])
  (:require [leela.blackbox.f :as f]
            [leela.blackbox.storage.cassandra :as storage]
            [leela.blackbox.storage.s3 :as s3]
            [leela.blackbox.network.zmqserver :as server]))

(defn maybe-getenv [u]
  (if (.startsWith u "$")
    (System/getenv (subs u 1))
    u))

(def cred {:access-key (maybe-getenv "$S3_ACCESS_KEY")
           :secret-key (maybe-getenv "$S3_SECRET_KEY")
           :endpoint (maybe-getenv "$S3_URL")})

(defmacro truncate-n-test [cluster name & body]
  `(testing ~name
     (storage/truncate-all ~cluster)
     ~@body))

(defn encode-str [data]
  (map #(f/str-to-bytes %) data))

(defmacro handle-message-macro [cluster msg]
 `(server/handle-message ~cluster ~cluster cred (encode-str ~msg)))

(deftest test-zmqserver-handle-put-name-msg
  (storage/with-connection [cluster ["127.0.0.1"] {}]
   (storage/use-attr-schema cluster "leela")
   (storage/use-graph-schema cluster "leela")
     (truncate-n-test cluster "putname returns guid"
      (let [[msg user tree kind name guid] (handle-message-macro cluster ["put" "name" "user" "tree" "kind" "foobar"])]
        (is (= "name" msg))
        (is (= "user" user))
        (is (= "tree" tree))
        (is (= "kind" kind))
        (is (= "foobar" name))
        (is guid)))

    (truncate-n-test cluster "putname is idempotent"
      (is (= (handle-message-macro cluster ["put" "name" "user" "tree" "kind" "foobar"]) (handle-message-macro cluster ["put" "name" "user" "tree" "kind" "foobar"]))))

    (truncate-n-test cluster "getguid after putname"
      (is (= (handle-message-macro cluster ["put" "name" "user" "tree" "kind" "foobar"]) (handle-message-macro cluster ["get" "guid" "user" "tree" "kind" "foobar"]))))

    (truncate-n-test cluster "getname after putname"
      (let [[_ _ _ _ _ guid] (handle-message-macro cluster ["put" "name" "user" "tree" "kind" "foobar"])]
        (is (= (handle-message-macro cluster ["get" "guid" "user" "tree" "kind" "foobar"]) (handle-message-macro cluster ["get" "name" guid])))))
))

(deftest test-zmqserver-handle-label-all-message
  (let [node (str (f/uuid-1))]
  (storage/with-connection [cluster ["127.0.0.1"] {}]
   (storage/use-attr-schema cluster "leela")
   (storage/use-graph-schema cluster "leela")

      (truncate-n-test cluster "getlabel-all with no data"
        (is (= (server/msg-label []) (handle-message-macro cluster ["get" "label" "all" node]))))

      (truncate-n-test cluster "getlabel-all after putlabel"
        (handle-message-macro cluster ["put"
                                        "label"
                                        node "0"
                                        node "1"
                                        node "2"])
        (is (= (server/msg-label ["0" "1" "2"]) (handle-message-macro cluster ["get" "label" "all" node]))))

      (truncate-n-test cluster "getlabel-all pagination"
        (handle-message-macro cluster ["put"
                                        "label"
                                        node "0"
                                        node "1"
                                        node "2"])
        (is (= (server/msg-label ["0"]) (handle-message-macro cluster ["get" "label" "all" node "" "1"])))
        (is (= (server/msg-label ["1"]) (handle-message-macro cluster ["get" "label" "all" node "1" "1"])))
        (is (= (server/msg-label ["2"]) (handle-message-macro cluster ["get" "label" "all" node "2" "1"])))))))

(deftest test-zmqserver-handle-label-prefix-message
  (let [node (str (f/uuid-1))]
  (storage/with-connection [cluster ["127.0.0.1"] {}]
   (storage/use-attr-schema cluster "leela")
   (storage/use-graph-schema cluster "leela")

      (truncate-n-test cluster "getlabel-prefix with no data"
        (is (= (server/msg-label []) (handle-message-macro cluster ["get" "label" "pre" node "1" "2"]))))

      (truncate-n-test cluster "getlabel-prefix after putlabel"
        (handle-message-macro cluster ["put"
                                        "label"
                                        node "00"
                                        node "01"
                                        node "02"
                                        node "10"
                                        node "11"
                                        node "12"
                                        node "20"
                                        node "21"
                                        node "22"])
        (is (= (server/msg-label ["10" "11" "12"]) (handle-message-macro cluster ["get" "label" "pre" node "1" "2"]))))

      (truncate-n-test cluster "getlabel-prefix pagination"
        (handle-message-macro cluster ["put"
                                        "label"
                                        node "00"
                                        node "01"
                                        node "02"
                                        node "10"
                                        node "11"
                                        node "12"
                                        node "20"
                                        node "21"
                                        node "22"])
        (storage/with-limit 1
          (is (= (server/msg-label ["10"]) (handle-message-macro cluster ["get" "label" "pre" node "1" "2"])))
          (is (= (server/msg-label ["11"]) (handle-message-macro cluster ["get" "label" "pre" node "11" "2"])))
          (is (= (server/msg-label ["12"]) (handle-message-macro cluster ["get" "label" "pre" node "12" "2"]))))))))

(deftest test-zmqserver-handle-label-exact-message
  (let [node (str (f/uuid-1))]
  (storage/with-connection [cluster ["127.0.0.1"] {}]
   (storage/use-attr-schema cluster "leela")
   (storage/use-graph-schema cluster "leela")

      (truncate-n-test cluster "getlabel-exact with no data"
        (is (= (server/msg-label []) (handle-message-macro cluster ["get" "label" "ext" node "foobar"]))))

      (truncate-n-test cluster "getlabel-exact after putlabel"
        (handle-message-macro cluster ["put" "label" node "foobar"])
        (is (= (server/msg-label ["foobar"]) (handle-message-macro cluster ["get" "label" "ext" node "foobar"])))))))

(deftest test-zmqserver-handle-getlink-message
  (let [node-a (str (f/uuid-from-time 1))
        node-b (str (f/uuid-from-time 2))
        node-c (str (f/uuid-from-time 3))
        node-d (str (f/uuid-from-time 4))
        node-e (str (f/uuid-from-time 5))]

   (storage/with-connection [cluster ["127.0.0.1"] {}]
    (storage/use-attr-schema cluster "leela")
    (storage/use-graph-schema cluster "leela")

      (truncate-n-test cluster "getlink with no data"
      (is (= (server/msg-link []) (handle-message-macro cluster ["get" "link" node-a "l" ""]))))

    (truncate-n-test cluster "getlink after putlink"
      (handle-message-macro cluster ["put" "link" node-a "l" node-b node-a "l" node-c node-a "l" node-d])
      (is (= (map seq (server/msg-link [node-b node-c node-d])) (map seq (handle-message-macro cluster ["get" "link" node-a "l" ""])))))

    (truncate-n-test cluster "getlink after dellink"
      (handle-message-macro cluster ["put" "link" node-a "l" node-b])
      (handle-message-macro cluster ["del" "link" node-a "l" node-b node-a "l" node-c node-a "l" node-d])
      (is (= (server/msg-link []) (handle-message-macro cluster ["get" "link" node-a "l" ""]))))

    (truncate-n-test cluster "getlink after dellink"
      (handle-message-macro cluster ["put" "link" node-a "l" node-b])
      (handle-message-macro cluster ["del" "link" node-a "l" ""])
      (is (= (server/msg-link []) (handle-message-macro cluster ["get" "link" node-a "l" ""]))))

    (truncate-n-test cluster "getlink pagination"
      (handle-message-macro cluster ["put" "link" node-a "l" node-b node-a "l" node-c node-a "l" node-d])
      (is (= (server/msg-link [node-b node-c]) (handle-message-macro cluster ["get" "link" node-a "l" "" "2"])))
      (is (= (server/msg-link [node-b node-c]) (handle-message-macro cluster ["get" "link" node-a "l" node-b "2"])))
      (is (= (server/msg-link [node-c node-d]) (handle-message-macro cluster ["get" "link" node-a "l" node-c "2"])))
      (is (= (server/msg-link [node-d]) (handle-message-macro cluster ["get" "link" node-a "l" node-d "2"])))
      (is (= (server/msg-link []) (handle-message-macro cluster ["get" "link" node-a "l" node-e "2"])))))))

(deftest test-zmqserver-handle-t-attr-message
  (let [node (str (f/uuid-1))
        value (f/str-to-bytes "foobar")]
    (storage/with-connection [cluster ["127.0.0.1"] {}]
     (storage/use-attr-schema cluster "leela")
     (storage/use-graph-schema cluster "leela")

      (truncate-n-test cluster "get-tattr with no data"
        (is (= (server/msg-tattr []) (handle-message-macro cluster ["get" "t-attr" node "attr" "0"]))))

      (truncate-n-test cluster "get-tattr after put-tattr"
        (handle-message-macro cluster ["put" "t-attr" node "attr" "0" value ""])
        (is (= (server/msg-tattr [[0 "foobar"]]) (map f/bytes-to-str (handle-message-macro cluster ["get" "t-attr" node "attr" "0"])))))

      (truncate-n-test cluster "get-tattr after del-tattr"
        (handle-message-macro cluster ["put" "t-attr" node "name" "0" value ""])
        (is (= (server/msg-done) (handle-message-macro cluster ["del" "t-attr" node "name" "0"])))
        (is (= (server/msg-tattr []) (handle-message-macro cluster ["get" "t-attr" node "name" "0"]))))

      (truncate-n-test cluster "get-tattr after del-tattr (selective)"
        (handle-message-macro cluster ["put" "t-attr"
                                        node "name" "0" value ""
                                        node "name" "1" value ""])
        (is (= (server/msg-done) (handle-message-macro cluster ["del" "t-attr" node "name" "0"])))
        (is (= (server/msg-tattr [[1 "foobar"]]) (map f/bytes-to-str (handle-message-macro cluster ["get" "t-attr" node "name" "0"]))))))))

(deftest test-zmqserver-handle-at-attr-message
  (let [attr-a (str (f/uuid-1) "/CPU")
        value (f/str-to-bytes "foobar2222")]
        (is (= (server/msg-done) (handle-message-macro nil ["put" "at-bucket" "201411"])))
        (is (= (server/msg-done) (handle-message-macro nil ["put" "at-attr" "201411" attr-a value])))
        (is (= (server/msg-fail 404) (handle-message-macro nil ["put" "at-attr" "20141" attr-a value])))
        (is (= "foobar2222" (f/bytes-to-str (handle-message-macro nil ["get" "at-attr" "201411" attr-a]))))
        (is (= nil (handle-message-macro nil ["get" "at-attr" "000000" attr-a])))))

(deftest test-zmqserver-handle-k-attr-message
  (let [node (str (f/uuid-1))
        value (f/str-to-bytes "foobar")]
    (storage/with-connection [cluster ["127.0.0.1"] {}]
     (storage/use-attr-schema cluster "leela")
     (storage/use-graph-schema cluster "leela")

      (truncate-n-test cluster "get-kattr with no data"
        (is (= (server/msg-fail 404) (map f/bytes-to-str (handle-message-macro cluster ["get" "k-attr" node "attr"])))))

      (truncate-n-test cluster "get-kattr after put-kattr with ttl"
        (is (= (server/msg-done) (handle-message-macro cluster ["put" "k-attr" node "attr" value "ttl:3600"])))
        (is (= (server/msg-kattr "foobar") (map f/bytes-to-str (handle-message-macro cluster ["get" "k-attr" node "attr"])))))

      (truncate-n-test cluster "get-kattr after put-kattr"
        (is (= (server/msg-done) (handle-message-macro cluster ["put" "k-attr" node "attr" value ""])))
        (is (= (server/msg-kattr "foobar") (map f/bytes-to-str (handle-message-macro cluster ["get" "k-attr" node "attr"])))))

      (truncate-n-test cluster "del-kattr after put-kattr"
        (handle-message-macro cluster ["put" "k-attr" node "attr" value ""])
        (is (= (server/msg-done) (handle-message-macro cluster ["del" "k-attr" node "attr"])))
        (is (= (server/msg-fail 404) (map f/bytes-to-str (handle-message-macro cluster ["get" "k-attr" node "attr"]))))))))

(deftest test-zmqserver-handle-attr-message
  (let [node (str (f/uuid-1))
        value (f/str-to-bytes "foobar")]
    (storage/with-connection [cluster ["127.0.0.1"] {}]
     (storage/use-attr-schema cluster "leela")
     (storage/use-graph-schema cluster "leela")

      (truncate-n-test cluster "get-attr with no data"
        (is (= (server/msg-nattr []) (map f/bytes-to-str (handle-message-macro cluster ["get" "attr" "k-attr" "all" node ""])))))

      (truncate-n-test cluster "get-attr after put-attr with ttl"
        (is (= (server/msg-done) (handle-message-macro cluster ["put" "k-attr" node "attr#1" value "ttl:3600, index:true" node "attr#2" value "ttl:3600, index:true"])))
        (is (= (server/msg-nattr ["attr#1" "attr#2"]) (map f/bytes-to-str (handle-message-macro cluster ["get" "attr" "k-attr" "all" node ""])))))

      (truncate-n-test cluster "get-attr after put-kattr (index:false)"
        (is (= (server/msg-done) (handle-message-macro cluster ["put" "k-attr" node "attr#1" value "" node "attr#2" value ""])))
        (is (= (server/msg-nattr []) (map f/bytes-to-str (handle-message-macro cluster ["get" "attr" "k-attr" "all" node ""])))))

      (truncate-n-test cluster "get-attr after put-kattr (index:true)"
        (is (= (server/msg-done) (handle-message-macro cluster ["put" "k-attr" node "attr#1" value "index:true" node "attr#2" value "index:true"])))
        (is (= (server/msg-nattr ["attr#1" "attr#2"]) (map f/bytes-to-str (handle-message-macro cluster ["get" "attr" "k-attr" "all" node ""])))))

      (truncate-n-test cluster "get-attr after del-kattr"
        (handle-message-macro cluster ["put" "k-attr" node "attr" value "index:true"])
        (is (= (server/msg-done) (handle-message-macro cluster ["del" "k-attr" node "attr"])))
        (is (= (server/msg-nattr []) (map f/bytes-to-str (handle-message-macro cluster ["get" "attr" "k-attr" "all" node ""]))))))))

(deftest test-zmqserver-zmqworker-interface
  (testing "just checks the interface"
    (is (:onjob (server/zmqworker nil nil nil)))
    (is (:onerr (server/zmqworker nil nil nil)))))
