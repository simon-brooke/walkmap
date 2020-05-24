(defproject walkmap "0.1.0-SNAPSHOT"
  :cloverage {:output "docs/cloverage"}
  :codox {:metadata {:doc "**TODO**: write docs"
                     :doc/format :markdown}
          :output-path "docs/codox"
          :source-uri "https://github.com/simon-brooke/walkmap/blob/master/{filepath}#L{line}"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/math.numeric-tower "0.0.4"]
                 [com.taoensso/timbre "4.10.0"]
                 [dali "0.7.4"]
                 [hiccup "1.0.5"]
                 [me.raynes/fs "1.4.6"]
                 [smee/binary "0.5.5"]]
  :description "A Clojure library designed to assist in computing walkmaps for games."
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :plugins [[lein-cloverage "1.1.1"]
            [lein-codox "0.10.7"]]
  :repl-options {:init-ns walkmap.core}
  :url "http://example.com/FIXME")
