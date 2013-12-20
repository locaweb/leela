(defproject blackbox "5.0.0"
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
                 [org.clojure/data.json "0.2.3"]
                 [org.clojure/tools.cli "0.2.4"]
                 [clojurewerkz/cassaforte "1.2.0"]
                 [org.clojure/tools.logging "0.2.6"]
                 [commons-codec/commons-codec "1.8"]])
