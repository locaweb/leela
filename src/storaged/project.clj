(defproject storaged "6.3.0"
  :url          "http://leela.rtfd.org"
  :license      {:name "MIT"
                 :url  "http://opensource.org/licenses/MIT"}
  :description  "Leela Storage Daemon: read and write access to the underlying storage"
  :dependencies [; [pandect "0.3.0"]
                 ; [clj-time "0.6.0"]
                 ; [clojure-msgpack "0.1.0-SNAPSHOT"]
                 ; [org.zeromq/jzmq "3.1.0"]
                 [org.clojure/clojure "1.6.0"]
                 [org.clojure/tools.cli "0.3.1"]
                 ; [clojurewerkz/cassaforte "2.0.0"]
                 [org.clojure/tools.logging "0.3.1"]
                 [org.apache.cassandra/cassandra-all "2.1.5"]]
  :profiles     {:storaged {:main         leela.storaged.main
                            :aot          [leela.storaged.main]
                            :uberjar-name "storaged-uberjar.jar"
                            :dependencies [[clojurewerkz/cassaforte "2.0.0"]]}
                 :triggers {:name         "triggers"
                            :aot          [leela.storaged.cassandra.triggers]}}
  :global-vars  {*warn-on-reflection* true}
)
