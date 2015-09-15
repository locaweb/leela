
(ns leela.storaged.network.actions.bitmap
  (:require
   [leela.storaged.bytes :as bytes]
   [leela.storaged.cassandra.bitmap :as c*]
   [leela.storaged.cassandra.connection :refer [with-limit]]
   [leela.storaged.network.actions.common :refer :all]))



(defn fetch-chunk [params]
	(when-map {"hash" string?} params (c*/fetch-chunk (get params "hash"))))

(defn fetch-index [params]
	(when-map 
		{"plane" integer? "varname" string? "content" string? "version" (nil-or integer?)} params 
		(if-let [version (get params "version")]
			(c*/fetch-index (get params "plane")
				         	(get params "varname")
					     	(get params "content")
						 	version)
			(c*/fetch-index (get params "plane")
				         	(get params "varname")
					     	(get params "content")))))




