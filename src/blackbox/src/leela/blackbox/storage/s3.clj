;; Copyright 2014 (c) Diego Souza <dsouza@c0d3.xxx>
;; Copyright 2014 (c) Alexandre Baaklini <abaaklini@gmail.com>
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

(ns leela.blackbox.storage.s3
  (:use     [clj-time.core :only [date-time year month day]]
            [clj-time.coerce]
            [clojure.tools.logging :only [info warn]]
            [clojurewerkz.cassaforte.query]
  )
  (:import [java.io PrintWriter InputStreamReader ByteArrayInputStream ByteArrayOutputStream])
  (:require [clojure.string :as s]
            [leela.blackbox.f :as f]
            [aws.sdk.s3 :as s3]
  )
)

(defstruct failed-oper :statuscode :errorcode)

(defn list-bucket
  ([with-client bucket]
    (s3/list-objects with-client bucket))
  ([with-client bucket node?]
   (s3/list-objects with-client bucket {:prefix (str node?)})))

(defn del-archived-tattr [with-client bucket k]
    (s3/delete-object with-client bucket k))

(defn get-archived-tattr [with-client bucket attr]
  (try
    (let [data (s3/get-object with-client bucket attr)]
      (:content data))
    (catch Exception e
      (if-not (= 404 (.getStatusCode e))
        (throw e)))
  )
)

(defn create-bucket [with-client bucket]
    (s3/create-bucket with-client bucket))

(defn delete-bucket
  ([with-client bucket]
   (try
    (s3/delete-bucket with-client bucket)
   (catch Exception e
    (if (= 409 (.getStatusCode e))
     (struct failed-oper (.getStatusCode e) (.getErrorCode e))
     (throw e)))))
  ([with-client bucket flag?]
    (if (= flag? true)
     (do
      (doall (map (fn [b] (del-archived-tattr with-client bucket (:key b))) (:objects (list-bucket with-client bucket))))
      (s3/delete-bucket with-client bucket))
     (s3/delete-bucket with-client bucket))))

(defn put-archived-tattr [with-client bucket attr data]
  (try
    (s3/put-object with-client bucket attr (ByteArrayInputStream. data) {:content-length (count data)})
  (catch Exception e
    (if (= "NoSuchBucket" (.getErrorCode e))
      (struct failed-oper (.getStatusCode e) (.getErrorCode e))
      (throw e)))))
