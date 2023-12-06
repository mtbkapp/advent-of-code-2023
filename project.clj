(defproject advent-of-code-2023 "0.1.0-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [instaparse "1.4.12"]]
  :main ^:skip-aot advent-of-code-2023.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}})
