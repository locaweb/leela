(defproject storaged "6.3.0"
  :url          "http://leela.rtfd.org"
  :license      {:name "MIT"
                 :url  "http://opensource.org/licenses/MIT"}
  :description  "Leela Storage Daemon: read and write access to the underlying storage"
  ;:dependencies [; [pandect "0.3.0"]
                 ; [clj-time "0.6.0"]
                 ; [clojure-msgpack "0.1.0-SNAPSHOT"]
                 ; [org.zeromq/jzmq "3.1.0"]
                 
                 ; [clojurewerkz/cassaforte "2.0.0"]
  :profiles     {:dev      {:dependencies [[org.clojure/tools.nrepl "0.2.0"]]}
                 :storaged {:main         leela.storaged.main
                            :aot          [leela.storaged.main]
                            :jar-name     "storaged-%s.jar"
                            :dependencies [[cheshire "5.5.0"]
                                           [org.clojure/clojure "1.6.0"]
                                           [clojurewerkz/cassaforte "2.0.0"]
                                           [org.clojure/tools.logging "0.3.1"]
                                           [org.apache.cassandra/cassandra-all "2.1.5"]]}
                 :triggers {:jar-name     "triggers-%s.jar"
                            :dependencies [[org.clojure/clojure "1.6.0"]
                                           [org.apache.cassandra/cassandra-all "2.1.5"]]
                            :aot          [leela.storaged.cassandra.triggers]}}
  :global-vars  {*warn-on-reflection* true}
)
