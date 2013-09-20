(ns leela.blackbox.config)

(def state (atom {:router {:capabilities 32 :queue-size 128 :endpoint "tcp://*:50021"}
                  :cassandra {:seed ["127.0.0.1"] :keyspace "leela" :rows-limit 10000}}))

(defn watch-state [k ver]
  (= ver (:version (@state k))))

(defn get-state [k & default]
  (let [data (@state k)]
    [data (watch-state k (:version data))]))

(defn read-state [k & default]
  (let [[data _] (get-state k default)]
    data))

(defn put-state [k v]
  (swap! state assoc k v))
