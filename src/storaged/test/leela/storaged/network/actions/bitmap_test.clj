(ns leela.storaged.network.actions.bitmap-test
  (:require
   [clojure.test :refer :all]
   [leela.tests.helpers :refer :all]
   [leela.storaged.bytes :as bytes]
   [leela.tests.pagination :refer :all]
   [leela.storaged.network.actions.bitmap :refer :all]
   [leela.storaged.cassandra.bitmap :as bitmap]
   ))

(install-keyspace-fixtures [bitmap/create-schema])

(deftest test-fetch-chunk-empty
	(is (nil? (fetch-chunk {"hash" "0"}))))

(deftest test-fetch-chunk-not-empty
	(let [hash (random-name 10)
		  data (byte-array (rand-int 100))]
	    (bitmap/store-chunk hash data)
	    (is (= (count data) (count (fetch-chunk {"hash" hash}))))))

(deftest test-fetch-index-empty
	(is (empty? (fetch-index {"plane" 0 "varname" "0" "content" "0"}))))

(deftest test-fetch-index-not-empty
	(let [plane (rand-int 100)
		  varname (random-name 15)
		  content (random-name 15)
		  version (rand-int 100)
		  blocks (map #(random-name (min 100 %)) (range 1 (rand-int 1000)))]
		  (bitmap/store-index plane varname content version blocks)
		  (is (= [{:version version :blocks blocks}] (fetch-index {"plane" plane "varname" varname "content" content})))
		  )
	)
