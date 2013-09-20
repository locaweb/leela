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
  (.setLinger fh 1)
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
