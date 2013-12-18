;; Copyright 2013 (c) Diego Souza <dsouza@c0d3.xxx>
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

(ns leela.blackbox.czmq.zhelpers
  (:import [org.zeromq ZMQ ZMQ$PollItem ZMQ$Socket]))

(defn recvmulti [fh]
  (loop [acc [(.recv fh)]]
    (if-not (.hasReceiveMore fh)
      acc
      (recur (conj acc (.recv fh))))))

(defn sendmulti [fh frame]
  (loop [msg  (first frame)
         rest (next frame)]
    (if (nil? rest)
      (.send fh msg 0)
      (do
        (.send fh msg ZMQ/SNDMORE)
        (recur (first rest) (next rest))))))

(defn setup-socket [fh]
  (.setLinger fh 0)
  (.setReconnectIVL fh 1000)
  (.setMaxMsgSize fh (* 1024 1024))
  fh)

(defn make-pollitem [^ZMQ$Socket fh events]
  (ZMQ$PollItem. fh (apply bit-or (conj events 0))))

(defn make-pollitems [watch]
  (let [events (map #(apply make-pollitem %) (partition 2 watch))]
    (into-array ZMQ$PollItem events)))

(defn poll [timeout watch]
  (let [pollitems (make-pollitems watch)]
    (ZMQ/poll pollitems timeout)
    pollitems))
