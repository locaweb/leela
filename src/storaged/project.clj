(defproject storaged "6.3.0"
  :url          "http://leela.rtfd.org"
  :license      {:name "MIT"
                 :url  "http://opensource.org/licenses/MIT"}
  :description  "Leela Storage Daemon: read and write access to the underlying storage"
  :profiles     {:repl     {:dependencies [[org.clojure/clojure "1.6.0"]
                                           [org.clojure/tools.nrepl "0.2.10"]]
                            :plugins      [[cider/cider-nrepl "0.10.0-SNAPSHOT"]]}
                 :test     {:jvm-opts ["-Dlogback.configurationFile=etc/logback.xml"]}
                 :storaged {:main         leela.storaged.main
                            :aot          [leela.storaged.main]
                            :jar-name     "storaged-%s.jar"
                            :dependencies [[pandect "0.5.2"]
                                           [clj-time "0.10.0"]
                                           [slingshot "0.12.2"]
                                           [clojure-msgpack "1.1.1"]
                                           [org.zeromq/jeromq "0.3.5"]
                                           [org.clojure/clojure "1.6.0"]
                                           [org.clojure/tools.cli "0.2.4"]
                                           [clojurewerkz/cassaforte "2.0.0"]
                                           [org.clojure/tools.logging "0.3.1"]
                                           [commons-codec/commons-codec "1.10"]
                                           [org.apache.cassandra/cassandra-all "2.1.5"]]}
                 :triggers {:jar-name     "triggers-%s.jar"
                            :dependencies [[org.clojure/clojure "1.6.0"]
                                           [org.apache.cassandra/cassandra-all "2.1.5"]]
                            :aot          [leela.storaged.cassandra.triggers]}}
  :global-vars  {*warn-on-reflection* true}
)
