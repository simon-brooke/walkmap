(ns walkmap.microworld
  "An interface between walkmap and microworld, to allow use of microworld
  functionality to model things like rainfall, soil fertility, settlement
  and so on."
  (:require [clojure.edn :as edn :only [read]]
            [clojure.java.io :as io]
            [clojure.string :as s]
            [taoensso.timbre :as l]
            [walkmap.edge :as e]
            [walkmap.polygon :as p :only [rectangle]]
            [walkmap.superstructure :refer [store]]
            [walkmap.tag :as t :only [tag]]
            [walkmap.vertex :as v :only [check-vertex vertex vertex?]]))

(defn cell->polygon
  "From this MicroWorld `cell`, construct a walkmap polygon (specifically,
  a rectangle. If `scale-vector` passed and is a vertex, scale all the vertices
  in the cell by that vector."
  ([cell]
   (cell->polygon cell (v/vertex 1 1 1)))
  ([cell scale-vector]
   (t/tag
     (assoc
       (merge
         cell
         (let [w (* (:x cell) (:x (check-vertex scale-vector)))
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




