(ns cc.journeyman.walkmap.microworld
  "An interface between walkmap and microworld, to allow use of microworld
  functionality to model things like rainfall, soil fertility, settlement
  and so on."
  (:require [cc.journeyman.walkmap.polygon :refer [rectangle]]
            [cc.journeyman.walkmap.superstructure :refer [store]]
            [cc.journeyman.walkmap.tag :refer [tag]]
            [cc.journeyman.walkmap.vertex :refer [check-vertex vertex]]
            [cc.journeyman.walkmap.utils :refer [truncate]]
            [clojure.edn :as edn :only [read]]
            [clojure.java.io :refer [reader]]
            [taoensso.timbre :refer [error]])
  (:import [clojure.lang Keyword IPersistentMap]
           [java.io PushbackReader]))

(defn cell->polygon
  "From this MicroWorld `cell`, construct a walkmap polygon (specifically,
  a rectangle). If `scale-vector` passed and is a vertex, scale all the vertices
  in the cell by that vector."
  ([cell]
   (cell->polygon cell (vertex 1 1 1)))
  ([cell scale-vector]
   (tag
    (assoc
     (merge
      cell
      (let [w (* (:x cell) (:x (check-vertex scale-vector)))
            s (* (:y cell) (:y scale-vector))
            e (+ w (:x scale-vector))
            n (+ s (:y scale-vector))
            z (* (:altitude cell) (:z scale-vector))]
        (rectangle
         (vertex s w z)
         (vertex n e z))))
     :walkmap.id/id
     (keyword (gensym "mw-cell")))
    (:state cell))))

(defn load-microworld-edn
  "While it would be possible to call MicroWorld functions directly from
  Walkmap, the fact is that running MicroWorld is so phenomenally
  compute-heavy that it's much more sensible to do it in batch mode. So the
  better plan is to be able to pull the output from MicroWorld - as an EDN
  structure - into a walkmap superstructure."
  ([^String filename]
   (load-microworld-edn filename :mw))
  ([^String filename ^Keyword map-kind]
   (load-microworld-edn filename map-kind nil))
  ([^String filename ^Keyword mapkind ^IPersistentMap superstucture]
   (load-microworld-edn filename mapkind superstucture (vertex 1 1 1)))
  ([^String filename ^Keyword map-kind ^IPersistentMap superstructure ^IPersistentMap _scale-vertex]
   (when-not
    (keyword? map-kind)
     (throw (IllegalArgumentException.
             (truncate
              (format "Must be a keyword: %s." (or map-kind "nil")) 80))))
   (let [mw (try
              (with-open [r (reader filename)]
                (edn/read (PushbackReader. r)))
              (catch RuntimeException e
                (error "Error parsing edn file '%s': %s\n"
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




