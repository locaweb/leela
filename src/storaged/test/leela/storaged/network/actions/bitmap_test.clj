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
	(is (nil? (get-fetch-chunk-handler {:hash "0"}))))

(deftest test-fetch-chunk-not-empty
	(let [hash (random-name 10)
		  data (bytes/chars-from-bytes (bytes/base64-from-bytes (bytes/bytes-from-chars (random-name 100))))]
        (put-store-chunk-handler {:hash hash :data data})
	    (is (= data (get-fetch-chunk-handler {:hash hash})))))

(deftest test-fetch-index-empty
	(is (empty? (get-fetch-index-handler {:plane 0 :varname "0" :content "0"}))))

(deftest test-fetch-index-not-empty
	(let [plane (rand-int 100)
		  varname (random-name 15)
		  content (random-name 15)
		  version (rand-int 100)
		  blocks (map #(random-name (min 100 %)) (range 1 (rand-int 1000)))]
		  (bitmap/put-store-index-handler plane varname content version blocks)
		  (is (= [{:version version :blocks blocks}] (get-fetch-index-handler {:plane plane :varname varname :content content})))
		  )
	)

