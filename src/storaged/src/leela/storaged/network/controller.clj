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

(ns leela.storaged.network.controller
  (:import
   [java.nio ByteOrder ByteBuffer])
  (:require
   [leela.storaged.time :refer :all]
   [slingshot.slingshot :refer [try+]]
   [clojure.tools.logging :refer [info warn error]]
   [leela.storaged.control :refer [try-fn]]
   [leela.storaged.network.protocol :refer :all]
   [leela.storaged.network.actions.metrics :refer :all]))

(defn- fmt-request [r]
  (pr-str r))

(defn- fmt-error [e]
  (pr-str e))

(defn- fmt-reply [r]
  (pr-str r))

(defn- fmt-duration [t]
  (pr-str t))

(defn- between? [n [a b]]
  (and (<= a n) (> b n)))

(defn m->logger [fun]
  (fn [payload]
    (let [[duration reply] (time-it (fun payload))
          code             (status reply)]
      (if (between? code [200, 500])
        (info (format "%s - %s [%s]"
                      (fmt-request payload)
                      (fmt-reply reply)
                      (fmt-duration duration)))
        (info (format "%s - %s [%s]"
                      (fmt-request payload)
                      (fmt-error reply)
                      (human-string duration))))
      reply)))

(defn- e-trace [^Throwable e]
  (map str (.getStackTrace e)))

(defn- e-message [^Throwable e]
  (.getMessage e))

(defn w->error [fun]
  (fn [payload]
    (try+
     (fun payload)
     (catch [:type :leela.storaged/user-error] info
       (let [e ^Throwable (:cause info)]
         (frame
          (encode-reply 500 {:trace (e-trace e)} (e-message e)))))
     (catch Exception e
       (error e "unexpected error serving request")
       (frame
        (encode-reply 500 {:trace (e-trace e)} (e-message e)))))))

(defn m->proto [fun]
  (fn [payload]
    (frame
     (if-let [query (unframe-when payload query?)]
       (fun query)
       (encode-reply 400 {} "bad frame")))))

(defn- handle-get [query]
  (case (first (payload query))
    "metrics" (let [params (rest (payload query))]
                (encode-reply 200 {} (get-metrics-handler (first params))))
    (encode-reply 404 {} "unknown table")))

(defn- handle-put [payload]
  (case (first (payload payload))
    "metrics" (let [params (rest (payload payload))]
                (encode-reply 201 {} (put-metrics-handler (first params))))
    (encode-reply 404 {} "unknown table")))

(defn controller [query]
  (case (first (payload query))
    "get" (handle-get (payload-fmap rest query))
    "put" (handle-put (payload-fmap rest query))
    (encode-reply 405 {:allow [:get :put]} "unknown verb")))

(defn make-controller [ctrl & middlewares]
  (w->error (reduce (fn [f g] (g f)) ctrl middlewares)))

(defn make-controller-default []
  (make-controller controller m->logger m->proto))
