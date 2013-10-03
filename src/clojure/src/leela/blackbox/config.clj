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

(ns leela.blackbox.config
  (:use     [clojure.tools.logging :only [info warn error]])
  (:import  [org.apache.zookeeper Watcher
                                  ZooKeeper
                                  CreateMode
                                  ZooDefs$Ids
                                  ZooKeeper$States
                                  Watcher$Event$EventType
                                  Watcher$Event$KeeperState
                                  KeeperException$NoNodeException
                                  KeeperException$NodeExistsException]
            [org.apache.zookeeper.data Stat])
  (:require [leela.blackbox.f :as f]))

(def +z-value+ {:announce  nil
                :cassandra {}
                :zmqrouter {:endpoint     "tcp://*:50021"
                            :queue-size   128
                            :capabilities 64}})

(def state (atom +z-value+))

(defn watch-state [version]
  (= version (:version @state)))

(defn wait-config []
  (loop []
    (when-not (:version @state)
      (Thread/sleep 100)
      (recur))))

(defn get-state [k]
  (let [obj @state
        ver (:version obj)
        ret (get obj k {})]
    (if (map? ret)
      [(assoc ret :version ver) #(watch-state ver)]
      [{:version ver :data ret} #(watch-state ver)])))

(defn read-state [k]
  (let [[data _] (get-state k)]
    data))

(defn put-state [v s]
  (warn (format "new node state: %s/v%s" s v))
  (swap! state (fn [_ s1] s1) {:version   v
                               :announce  (:announce s {})
                               :cassandra (conj (:cassandra +z-value+) (:cassandra s {}))
                               :zmqrouter (conj (:zmqrouter +z-value+) (:zmqrouter s {}))}))

(defn zk-make-watcher [f & args]
  (proxy [Watcher] [] (process [e] (apply f (cons e args)))))

(defn zk-confpath [node]
  (format "/conf/blackbox/%s" node))

(defn zk-livepath [node]
  (format "/live/blackbox/%s" node))

(defn zk-read [zkref]
  (let [[zk node & _] @zkref
        path         (zk-confpath node)
        stat         (Stat.)]
    (loop []
      (if-let [data (try
                      (.getData zk path true stat)
                      (catch KeeperException$NoNodeException _))]
        [(.getVersion stat) (f/bytes-to-json data)]
        (if (.exists zk path true)
          (recur)
          [0 {}])))))

(defn zk-announce-state [zkref]
  (f/retry-on-error 10
    (let [[zk node & _] @zkref
          acls          (java.util.ArrayList.)
          path          (zk-livepath node)
          state         (read-state :announce)
          data          (f/json-to-bytes state)]
      (warn (format "announcing new state: %s" state))
      (.addAll acls ZooDefs$Ids/CREATOR_ALL_ACL)
      (.addAll acls ZooDefs$Ids/READ_ACL_UNSAFE)
      (doseq [ppath ["/live" "/live/blackbox"]]
        (when-not (.exists zk ppath false)
          (.create zk ppath nil acls CreateMode/PERSISTENT)))
      (if-let [stat (.exists zk path false)]
        (when-not (= (.getEphemeralOwner stat) (.getSessionId zk))
          (warn (format "purging stale node: %s" path))
          (.delete zk path (.getVersion stat))))
      (try
        (.create zk path data acls CreateMode/EPHEMERAL)
        (catch KeeperException$NodeExistsException _
          (.setData zk path data -1))))))

(defn reload [zkref]
  (apply put-state (zk-read zkref))
  (zk-announce-state zkref))

(defn zk-conn-watcher [e reconnect zkref]
  (let [state         (.getState e)
        [zk _ _ auth] @zkref]
    (condp = state
      Watcher$Event$KeeperState/SyncConnected (do
                                                (warn "connection established")
                                                (.addAuthInfo zk "digest" (f/str-to-bytes auth))
                                                (reload zkref))
      Watcher$Event$KeeperState/Expired (do
                                          (warn "session has expired, reconnecting")
                                          (.close zk)
                                          (reconnect zkref))
      Watcher$Event$KeeperState/Disconnected (do
                                               (warn "disconnected from zookeeper, reconnecting")
                                               (.close zk)
                                               (reconnect zkref))
      (warn (format "unknown zk state: %s" state)))))

(defn zk-watcher [e reconnect zkref]
  (let [event (.getType e)]
    (info (format "zookeeper event: %s" e))
    (condp = event
      Watcher$Event$EventType/None (zk-conn-watcher e reconnect zkref)
      Watcher$Event$EventType/NodeCreated (reload zkref)
      Watcher$Event$EventType/NodeDataChanged (reload zkref)
      Watcher$Event$EventType/NodeDeleted (reload zkref)
      (warn (format "ignoring unknown event type: %s" e)))))

(defn zk-connect [zkref]
  (let [watcher             (zk-make-watcher zk-watcher zk-connect zkref)
        [_ _ endpoint auth] @zkref]
    (swap! zkref (fn [[_ & args] zk] (cons zk args)) (ZooKeeper. endpoint 5000 watcher))
    zkref))

(defn zk-attach [node endpoint auth]
  (let [zkref (atom [nil node endpoint auth])]
    (zk-connect zkref)
    (wait-config)))
