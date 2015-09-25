(ns leela.storaged.network.actions.sequence
  (:require
   [leela.storaged.bytes :as bytes]
   [leela.storaged.cassandra.sequence :as c*]
   [leela.storaged.cassandra.connection :refer [with-limit]]
   [leela.storaged.network.actions.common :refer :all]))


(defn get-fetch-block-handler [params]
    (when-map
        {:plane integer?} params
            (c*/fetch-block (:plane params))))

(defn put-store-block-handler [params]
    (when-map
        {:plane integer? :block integer?} params
            (c*/store-block (:plane params)
                            (:block params))))

(defn post-fetch-next-block-handler [params]
    (when-map 
        {:plane integer?} params
            (c*/alloc-block (:plane params))))

(defn get-fetch-sequence-handler [params]
    (when-map 
        {:plane integer? :object bytes/base64?} params
            (if-let [object(:object params)]
                 (c*/fetch-sequence (:plane params) 
                                    object)
                 (c*/fetch-sequence (:plane params)
                                    ))))

(defn put-store-sequence-handler [params]
    (when-map
         {:plane integer? :seqid integer? :object bytes/base64?} params
            (c*/store-obj (:plane params)
                          (:seqid params)
                          (:object params))))



(defn get-fetch-seqid-handler [params]
    (when-map
        {:plane integer? :object bytes/base64?} params
            (c*/fetch-seqid (:plane params)
                            (:object params))))

;(defn post-alloc-block-handler [params]
;(when-map
;   {:plane integer?} params
;       (c*/alloc-block (:plane params))))