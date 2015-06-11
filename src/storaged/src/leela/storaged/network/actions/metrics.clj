;; Copyright 2015 (c) Diego Souza <dsouza@c0d3.xxx>
;;  
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;  
;;     http://www.apache.org/licenses/LICENSE-2.0
;;  
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

(ns leela.storaged.network.actions.metrics
  (:require
   [cheshire.core :refer [generate-cbor parse-cbor]]
   [leela.storaged.bytes :as bytes]
   [leela.storaged.cassandra.metrics :as c*]
   [leela.storaged.cassandra.connection :refer [with-limit]]
   [leela.storaged.network.actions.common :refer :all]))

(defn get-metrics-handler [params]
  (when-map {:plane integer?
             :metric integer?
             :bucket integer?
             :limit (nil-or integer?)
             :offset (nil-or integer?)} params
             (with-limit (get params :limit default-limit)
               (if-let [offset (:offset params)]
                 (c*/fetch-metric (:plane params)
                                  (:metric params)
                                  (:bucket params)
                                  offset)
                 (c*/fetch-metric (:plane params)
                                  (:metric params)
                                  (:bucket params))))))

(defn get-index-handler [params]
  (when-map {:plane integer?
             :metric integer?
             :limit (nil-or integer?)
             :bucket (nil-or integer?)} params
             (with-limit (get params :limit default-limit)
               (if-let [bucket (:bucket params)]
                 (c*/fetch-index (:plane params)
                                 (:metric params)
                                 bucket)
                 (c*/fetch-index (:plane params)
                                 (:metric params))))))

(defn put-metrics-handler [params]
  (when-map {:plane integer?
             :metric integer?
             :bucket integer?
             :offset integer?
             :datum bytes/byte-array?} params
             (c*/store-metric (:plane params)
                              (:metric params)
                              (:bucket params)
                              (:offset params)
                              (:datum params))))
