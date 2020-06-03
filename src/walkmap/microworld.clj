(ns walkmap.microworld
  "An interface between walkmap and microworld, to allow use of microworld
  functionality to model things like rainfall, soil fertility, settlement
  and so on."
  (:require [clojure.edn :as edn :only [read]]
            [clojure.java.io :as io]
            [clojure.string :as s]
            [mw-cli.core :refer [process]]
            [mw-engine.core :refer [run-world]]
            [mw-engine.heightmap :as h]
            [mw-engine.drainage :as d]
            [mw-parser.bulk :as parser]
            [taoensso.timbre :as l]
            [walkmap.edge :as e]
            [walkmap.polygon :as p :only [check-polygon polygon? rectangle]]
            [walkmap.superstructure :refer [store]]
            [walkmap.tag :as t :only [tag tags]]
            [walkmap.utils :as u :only [check-kind-type check-kind-type-seq kind-type truncate]]
            [walkmap.vertex :as v :only [vertex vertex?]]))

;; (def settlement-rules (parser/compile-file "resources/rules/settlement_rules.txt"))

;; (def w0 (h/apply-heightmap "../the-great-game/resources/maps/heightmap.png"))
;; (def w1 (d/rain-world (d/flood-hollows w0)))
;; (def w2 (drainage/flow-world-nr w1))

;; (def w3 (run-world w2 nil settlement-rules 100))

;; (with-open [w (clojure.java.io/writer "settlement_1.edn")]
;;   (binding [*out* w]
;;     (pr
;;       (run-world w0 nil settlement-rules 100))))

;; (process
;;   (h/apply-heightmap "resources/small_hill.png")
;;   (parser/compile-file "resources/rules/settlement_rules.txt")
;;   100
;;   "small_hill.edn"
;;   "small_hill.html")

(defn cell->polygon
  ([cell]
   (cell->polygon cell (v/vertex 1 1 1)))
  ([cell scale-vector]
   (t/tag
     (assoc
       (merge
         cell
         (let [w (* (:x cell) (:x scale-vector))
               s (* (:y cell) (:y scale-vector))
               e (+ w (:x scale-vector))
               n (+ s (:y scale-vector))
               z (* (:altitude cell) (:z scale-vector))]
         (p/rectangle
           (v/vertex s w z)
           (v/vertex n e z))))
       :walkmap.id/id
       (keyword (gensym "mw-cell")))
     (:state cell))))

(defn load-microworld-edn
  "While it would be possible to call MicroWorld functions directly from
  Walkmap, the fact is that running MicroWorld is so phenomenally
  compute-heavy that it's much more sensible to do it in batch mode. So the
  better plan is to be able to pull the output from MicroWorld - as an EDN
  structure - into a walkmap superstructure."
  ([filename]
   (load-microworld-edn filename :mw))
  ([filename map-kind]
   (when-not
     (keyword? map-kind)
     (throw (IllegalArgumentException.
              (u/truncate
                (str "Must be a keyword: " (or map-kind "nil")) 80))))
   (load-microworld-edn filename map-kind nil))
  ([filename mapkind superstucture]
   (load-microworld-edn filename mapkind superstucture (v/vertex 1 1 1)))
  ([filename map-kind superstructure scale-vertex]
   (let [mw (try
              (with-open [r (io/reader filename)]
                (edn/read (java.io.PushbackReader. r)))
              (catch RuntimeException e
                (l/error "Error parsing edn file '%s': %s\n"
                         filename (.getMessage e))))
         polys (reduce
                 concat
                 (map (fn [row] (map cell->polygon row)) mw))]
     (if (map? superstructure)
       (reduce
         #(store %2 %1)
         superstructure
         polys)
       polys))))

;; (load-microworld-edn "../MicroWorld/mw-cli/isle_of_man.edn" nil {})
