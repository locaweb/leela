;; Copyright (c) 2015 <Diego Souza>

;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

(ns leela.storaged.network.actions.metrics
  (:require
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
