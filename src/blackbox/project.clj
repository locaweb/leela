(defproject blackbox "6.2.1"
  :url          "http://leela.rtfd.org"
  :aot          [leela.blackbox.blackbox]
  :main         leela.blackbox.blackbox
  :license      {:name "APACHE-2.0"
                 :url  "http://www.apache.org/licenses/LICENSE-2.0"}
  :description  "Leela: scalable metrics monitoring engine [storage component]"
  :dependencies [[pandect "0.3.0"]
                 [clj-time "0.6.0"]
                 [clojure-msgpack "0.1.0-SNAPSHOT"]
                 [org.zeromq/jzmq "3.1.0"]
                 [clj-aws-s3 "0.3.10"]
                 [org.clojure/clojure "1.6.0"]
                 [org.clojure/tools.cli "0.2.4"]
                 [clojurewerkz/cassaforte "2.0.0"]
                 [org.clojure/tools.logging "0.2.6"]
                 [org.apache.cassandra/cassandra-all "2.1.2"]]
)
