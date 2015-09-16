(ns leela.storaged.network.protocol-test
  (:require
   [clojure.test :refer :all]
   [leela.tests.helpers :refer :all]
   [leela.storaged.bytes :as bytes]
   [leela.storaged.network.protocol :refer :all]))

(deftest test-frame-unframe-is-idempotent
  (let [k (keyword (random-name 100))
        v (random-name 100)
        z (random-name 100)
        w (rand-int 100)]
    (is (= [z w {k v}] (unframe (frame [z w {k v}]))))))

(deftest test-encode-query-properties
  (let [query (encode-query [:x :y :z])]
    (is (= "query-1.0" (kind query)))
    (is (= :query (kind-nover query)))
    (is (nil? (status query)))
    (is (= [:x :y :z] (resource query)))
    (is (= {} (headers query)))
    (is (nil? (payload query)))
    (is (query? query))))

(deftest test-encode-reply-properties
  (let [reply (encode-reply 200)]
    (is (= "reply-1.0" (kind reply)))
    (is (= :reply (kind-nover reply)))
    (is (= 200 (status reply)))
    (is (nil? (resource reply)))
    (is (= {} (headers reply)))
    (is (nil? (payload reply)))
    (is (reply? reply))))

(deftest test-encode-q-o-r-payload
  (let [query (encode-query [] {} "foobar")
        reply (encode-reply 200 {} "foobar")]
    (is (= "foobar" (payload query)))
    (is (= "foobar" (payload reply)))))

(deftest test-header-function
  (let [query (encode-query [] {:foo "bar" :bar "foo"})]
    (is (nil? (header query :baz)))
    (is (= "bar" (header query :foo)))
    (is (= "foo" (header query :bar)))
    (is (= :baz (header query :baz :baz)))))

(deftest test-header-functions-on-query
  (let [query-wh (encode-query [] {:content-type ["foobar"] :accept ["foobar"]})
        query-nh (encode-query 200)]
    (is (= ["foobar"] (header-content-type query-wh)))
    (is (= ["foobar"] (header-accept query-wh)))
    (is (= ["application/x-msgpack"] (header-content-type query-nh)))
    (is (= ["application/x-msgpack"] (header-accept query-nh)))))

(deftest test-header-functions-on-reply
  (let [reply-wh (encode-reply 200 {:content-type ["foobar"] :accept ["foobar"]})
        reply-nh (encode-reply 200)]
    (is (= ["foobar"] (header-content-type reply-wh)))
    (is (= ["foobar"] (header-accept reply-wh)))
    (is (= ["application/x-msgpack"] (header-content-type reply-nh)))
    (is (= ["application/x-msgpack"] (header-accept reply-nh)))))

(deftest test-payload-fmap
  (let [reply (encode-reply 200 {} [:foo :bar])
        query (encode-query [] {} [:foo :bar])]
    (is (= (encode-reply 200 {} '(:bar)) (payload-fmap rest reply)))
    (is (= (encode-query [] {} '(:bar)) (payload-fmap rest query)))))

(deftest test-headers-fmap
  (let [reply (encode-reply 200 {:foo :bar})
        query (encode-query [] {:foo :bar})]
    (is (= (encode-reply 200 {}) (headers-fmap #(dissoc % :foo) reply)))
    (is (= (encode-query [] {}) (headers-fmap #(dissoc % :foo) query)))))

(deftest test-resource-fmap
  (let [query (encode-query [:foo :bar])]
    (is (= (encode-query '(:bar)) (resource-fmap rest query)))))

(deftest test-status-fmap
  (let [reply (encode-reply 200)]
    (is (= (encode-reply 201) (status-fmap inc reply)))))
