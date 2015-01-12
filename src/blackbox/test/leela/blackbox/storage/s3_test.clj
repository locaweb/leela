(ns leela.blackbox.storage.s3-test
  (:use     [clojure.test]
            [clojure.tools.logging :only [info warn]]
  )
  (:require [leela.blackbox.f :as f]
            [leela.blackbox.storage.s3 :as storage]
  )
)

(defmacro dbg[x] `(let [x# ~x] (println "dbg:" '~x "=" x#) x#))

(defn maybe-getenv [u]
  (if (.startsWith u "$")
    (System/getenv (subs u 1))
    u))

(def cred {:access-key (maybe-getenv "$S3_ACCESS_KEY")
           :secret-key (maybe-getenv "$S3_SECRET_KEY")
           :endpoint (maybe-getenv "$S3_URL")})

(deftest test-archived-tattr-create-raw
  (is (= "foo" (:name (storage/create-bucket cred "foo"))))
  (is (= "foo" (:bucket (storage/list-bucket cred "foo"))))
  (is (= nil (storage/delete-bucket cred "foo"))))

(deftest test-archived-tattr-create
  (is (= "201401" (:name (storage/create-bucket cred "201401"))))
  (is (= "201401" (:bucket (storage/list-bucket cred "201401"))))
  (is (= nil (storage/delete-bucket cred "201401"))))

(deftest test-archived-tattr-simple
  (let [attr-a (str (f/uuid-from-time 1) "/CPU")
        node-b (f/uuid-from-time 2)]
   (is (= "201412" (:name (storage/create-bucket cred "201412"))))
   (storage/put-archived-tattr cred "201412" attr-a (f/str-to-bytes "alecfooz"))
   (is (= "alecfooz" (slurp (storage/get-archived-tattr cred "201412" attr-a))))
   (is (= nil (storage/del-archived-tattr cred "201412" attr-a)))
   (is (= nil (storage/get-archived-tattr cred "201412" attr-a)))
   (is (= nil (storage/delete-bucket cred "201412")))
))

(deftest test-archived-tattr
  (let [attr-a (str (f/uuid-from-time 1) "/CPU")
        attr-b (str (f/uuid-from-time 2) "/CPU2")]
   (is (= "201412" (:name (storage/create-bucket cred "201412"))))
   (is (= nil (storage/get-archived-tattr cred "201412" attr-a)))
   (storage/put-archived-tattr cred "201412" attr-a  (f/str-to-bytes "alecfoo"))
   (is (= "alecfoo" (slurp (storage/get-archived-tattr cred "201412" attr-a))))
   (storage/put-archived-tattr cred "201412" attr-b (f/str-to-bytes "alecfoo"))
   (is (= "alecfoo" (slurp (storage/get-archived-tattr cred "201412" attr-b))))
   (is (= nil (storage/del-archived-tattr cred "201412" attr-a)))
   (is (= nil (storage/get-archived-tattr cred "201412" attr-a)))
   (is (= "BucketNotEmpty" (:errorcode (storage/delete-bucket cred "201412"))))
   (is (= nil (storage/delete-bucket cred "201412" true))))
)
