(defproject fit "0.1.0-SNAPSHOT"
  :description "API Fit para controle de usuários, alimentos e exercícios"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [ring "1.9.6"]
                 [compojure "1.6.2"]
                 [cheshire "5.11.0"]
                 [ring/ring-json "0.5.1"]
                 [clj-time "0.15.2"]
                 [clj-http "3.12.3"]
                 [org.clojure/data.json "2.4.0"]]
  :repl-options {:init-ns fit.core}
  :main fit.api
  :plugins [[lein-ring "0.12.5"]]
  :ring {:handler fit.api/app
         :init    fit.api/-main}
  :profiles {:dev {:dependencies [[javax.servlet/servlet-api "2.5"]]
                   :plugins      [[lein-ring "0.12.5"]]}})
