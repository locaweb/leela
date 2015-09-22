
(ns leela.storaged.network.actions.bitmap
  (:require
   [leela.storaged.bytes :as bytes]
   [leela.storaged.cassandra.bitmap :as c*]
   [leela.storaged.cassandra.connection :refer [with-limit]]
   [leela.storaged.network.actions.common :refer :all]))

(defn get-fetch-chunk-handler [params]
	(when-map {:hash string?} params (c*/fetch-chunk (:hash params))))

(defn get-fetch-index-handler [params]
	(when-map 
		{:plane integer? :varname string? :content string? :version (nil-or integer?)} params 
		(if-let [version (:version params)]
			(c*/fetch-index (:plane params)
				         	(:varname params)
					     	(:content params)
						 	version)
			(c*/fetch-index (:plane params)
				         	(:varname params)
					     	(:content params)))))

(defn put-store-chunk-handler [params]
	(when-map 
		{:hash string? :data bytes/base64?} params 
			(c*/store-chunk (:hash params)
							(:data params))))

(defn put-store-index-handler [params]
    (when-map
        {:plane integer? :varname string? :content string? :version (nil-or integer?) :blocks (all string?) } params 
            (c*/store-index (:plane params)
                           (:varname params)
                           (:version params)
                           (:blocks params))))







