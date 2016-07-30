(defproject turtle-geometry "0.0.1-SNAPSHOT"
  :description "Turtle Geometry"
  :url "http://wbabic.github.io/turtle-geometry"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/clojurescript "1.8.51"]
                 [org.clojure/tools.analyzer.jvm "0.6.9"]

                 [devcards "0.2.1-7" :exclusions [org.clojure/tools.reader
                                                  cljsjs/react
                                                  cljsjs/react-dom
                                                  cljsjs/react-dom-server]]
                 [sablono "0.7.1"]
                 [cljsjs/react "15.0.2-0"]
                 [cljsjs/react-dom "15.0.2-0"]
                 [cljsjs/react-dom-server "15.0.2-0"]
                 [reagent "0.6.0-rc" :exclusions [cljsjs/react
                                                  cljsjs/react-dom
                                                  cljsjs/react-dom-server]]

                 [org.clojure/core.match "0.3.0-alpha4"]
                 [org.clojure/core.async "0.2.374"]

                 [ring/ring-core "1.4.0"]
                 [clj-time "0.9.0"]

                 [fipp "0.6.4"]
                 [com.gfredericks/exact "0.1.8"]
                 [prismatic/schema "1.1.0"]]

  :profiles
  {:dev
   {:dependencies [[org.clojure/test.check "0.9.0"]]}}

  :plugins [[lein-cljsbuild "1.1.3"]
            [lein-figwheel "0.5.3-2"
             :exclusions [org.clojure/clojure
                          ring/ring-core joda-time
                          org.clojure/tools.reader]]]

  :clean-targets ^{:protect false} ["resources/public/js/compiled"
                                    "resources/public/js/pages"
                                    "target"]

  :source-paths ["src"]

  :cljsbuild {:builds [{:id "devcards"
                        :source-paths ["src"]
                        :figwheel {:devcards true } ;; <- note this
                        :compiler {:main       "turtle-geometry.core"
                                   :asset-path "js/compiled/devcards_out"
                                   :output-to  "resources/public/js/compiled/turtle-geometry.js"
                                   :output-dir "resources/public/js/compiled/devcards_out"
                                   :source-map-timestamp true }}
                       {:id "pages"
                        :source-paths ["src" "pages-src"]
                        :compiler {:main       "pages.core"
                                   :devcards true
                                   :asset-path "js/pages/out"
                                   :output-to  "resources/public/js/pages/turtle-geometry.js"
                                   :optimizations :advanced}}]}

  :figwheel { :css-dirs ["resources/public/css"]})
