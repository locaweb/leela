(ns leela.tests.helpers
  (:require
   [clojure.test :refer :all]
   [clojure.string :refer [join]]
   [clojurewerkz.cassaforte.cql :as cql]
   [clojurewerkz.cassaforte.query :as stmt]
   [leela.storaged.cassandra.connection :as conn :refer [+keyspace+]]))

(def ^:dynamic +cluster+)



(defmacro install-keyspace-fixtures [create-schema-coll]
  `(letfn [(create-keyspace-fixture# [f#]
             (let [keyspace# (join ["fixture_" (random-name 5)])]
               (conn/with-connection [cluster# ["127.0.0.1"] {}]
                 (binding [+cluster+ cluster#]
                   (cql/create-keyspace cluster# keyspace#
                                        (stmt/if-not-exists)
                                        (stmt/with {:replication {:class "SimpleStrategy" :replication_factor 1}}))
                   (try
                     (conn/with-keyspace keyspace#
                       (doseq [fun# ~create-schema-coll]
                         (fun# cluster#))
                       (f#))
                     (finally
                       (cql/drop-keyspace cluster# keyspace#)))))))

           (truncate-keyspace-fixture# [f#]
             (doseq [table# (conn/desc-tables +cluster+ +keyspace+)]
               (cql/truncate +cluster+ (conn/fqn table#)))
             (f#))]

     (use-fixtures :once create-keyspace-fixture#)
     (use-fixtures :each truncate-keyspace-fixture#)))

(defn a-z-range []
  (map char (range (int \a) (inc (int \z)))))

(defn random-name [n]
  (join (repeatedly n #(rand-nth (a-z-range)))))
