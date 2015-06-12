(ns leela.tests.helpers
  (:require
   [clojure.test :refer :all]
   [clojure.string :refer [join]]
   [clojurewerkz.cassaforte.cql :as cql]
   [clojurewerkz.cassaforte.query :as stmt]
   [clojurewerkz.cassaforte.policies :refer [without-prepared-statements]]
   [leela.storaged.cassandra.connection :as conn :refer [*cluster* *keyspace*]]))

(defmacro install-keyspace-fixtures [create-schema-coll]
  `(letfn [(create-keyspace-fixture# [f#]
             (let [keyspace# (join ["fixture_" (random-name 5)])]
               (conn/with-connection [cluster# ["127.0.0.1"] {}]
                 (conn/with-cluster cluster#
                   (cql/create-keyspace cluster# keyspace#
                                        (stmt/if-not-exists)
                                        (stmt/with {:replication {:class "SimpleStrategy" :replication_factor 1}}))
                   (try
                     (conn/with-keyspace keyspace#
                       (doseq [fun# ~create-schema-coll] (fun#))
                       (without-prepared-statements (f#)))
                     (finally
                       (cql/drop-keyspace cluster# keyspace#)))))))

           (truncate-keyspace-fixture# [f#]
             (doseq [table# (conn/desc-tables)]
               (cql/truncate *cluster* (conn/fqn table#)))
             (f#))]

     (use-fixtures :once create-keyspace-fixture#)
     (use-fixtures :each truncate-keyspace-fixture#)))

(defn trace
  ([k x] (println (format "%s: %s" k (pr-str x))) x)
  ([x] (println (pr-str x)) x))

(defn a-z-range []
  (map char (range (int \a) (inc (int \z)))))

(defn random-name [n]
  (join (repeatedly n #(rand-nth (a-z-range)))))
