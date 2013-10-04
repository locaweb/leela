(defproject blackbox "4.0.0"
  :url          "http://leela.rtfd.org"
  :aot          [leela.blackbox.blackbox]
  :main         leela.blackbox.blackbox
  :license      {:name "APACHE-2.0"
                 :url  "http://www.apache.org/licenses/LICENSE-2.0"}
  :description  "Leela: scalable metrics monitoring engine [storage component]"
  :dependencies [[org.zeromq/jzmq "2.2.2"]
                 [org.clojure/clojure "1.5.1"]
                 [org.clojure/data.json "0.2.3"]
                 [org.clojure/tools.cli "0.2.4"]
                 [clojurewerkz/cassaforte "1.2.0"]
                 [org.clojure/tools.logging "0.2.6"]
                 [org.apache.zookeeper/zookeeper "3.4.5" :exclusions [javax.jms/jms
                                                                      com.sun.jmx/jmxri
                                                                      com.sun.jdmk/jmxtools]]])
