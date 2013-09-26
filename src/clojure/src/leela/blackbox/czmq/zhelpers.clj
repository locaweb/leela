;; This file is part of Leela.
;;
;; Leela is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; Leela is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Leela.  If not, see <http://www.gnu.org/licenses/>.

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
