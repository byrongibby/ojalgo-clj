(defproject ojalgo-clj "0.1.0-SNAPSHOT"
  :description "A core.matrix implementation of oj! Algorithms"
  :url "https://github.com/byrongibby/ojalgo-clj"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[net.mikera/core.matrix "0.62.0"]
                 [org.clojure/clojure "1.10.1"]
                 [org.ojalgo/ojalgo "48.4.1"]]
  :source-paths ["src/clojure"]
  :java-source-paths ["src/java"]
  :target-path "target/%s"
  :aot [ojalgo-clj.function]
  :repl-options {:init-ns ojalgo-clj.core-test}
  :profiles {:dev {:dependencies [[net.mikera/vectorz-clj "0.48.0"]
                                  [net.mikera/core.matrix "0.62.0" :classifier "tests"]]}
             :test {:dependencies [[net.mikera/vectorz-clj "0.48.0"]
                                   [net.mikera/core.matrix "0.62.0" :classifier "tests"]]}
             :uberjar {:aot :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}})
