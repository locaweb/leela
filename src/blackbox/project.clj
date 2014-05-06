(defproject blackbox "4.2.1"
  :url          "http://leela.rtfd.org"
  :aot          [leela.blackbox.blackbox]
  :main         leela.blackbox.blackbox
  :license      {:name "APACHE-2.0"
                 :url  "http://www.apache.org/licenses/LICENSE-2.0"}
  :description  "Leela: scalable metrics monitoring engine [storage component]"
  :dependencies [[pandect "0.3.0"]
                 [clj-time "0.6.0"]
                 [org.zeromq/jzmq "2.2.2"]
                 [org.clojure/clojure "1.5.1"]
                 [org.clojure/tools.cli "0.2.4"]
                 [clojurewerkz/cassaforte "1.3.0-beta8"]
                 [org.clojure/tools.logging "0.2.6"]
                 [org.apache.cassandra/cassandra-all "2.0.2"]])
