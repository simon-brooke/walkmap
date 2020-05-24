(ns walkmap.core
  "At this stage, primarily utility functions dealing with stereolithography
  (STL) files. Not a stable API yet!"
  (:require [clojure.java.io :as io :refer [file output-stream input-stream]]
            [clojure.string :as s]
            [hiccup.core :refer [html]]
            [me.raynes.fs :as fs]
            [taoensso.timbre :as l :refer [info error spy]]
            [walkmap.stl :refer [decode-binary-stl]]
            [walkmap.svg :refer [stl->svg]]))

(def ^:dynamic *sea-level*
  "The sea level on heightmaps we're currently handling. If characters are to
  be able to swin in the sea, we must model the sea bottom, so we need
  heightmaps which cover at least the continental shelf. However, the sea
  bottom is not walkable territory and can be culled from walkmaps.

  **Note** must be a floating point number. `(= 0 0.0)` returns `false`!"
  0.0)

(defn ocean?
  "Of a `facet`, is the altitude of every vertice equal to `*sea-level*`?"
  [facet]
  (every?
    #(= % *sea-level*)
    (map :z (:vertices facet))))

(defn cull-ocean-facets
  "Ye cannae walk on water. Remove all facets from this `stl` structure which
  are at sea level."
  [stl]
  (assoc stl :facets (remove ocean? (:facets stl))))

(defn binary-stl-file->svg
  "Given only an `in-filename`, parse the indicated file, expected to be
  binary STL, and return an equivalent SVG structure. Given both `in-filename`
  and `out-filename`, as side-effect write the SVG to the indicated output file."
  ([in-filename]
   (stl->svg (cull-ocean-facets (decode-binary-stl in-filename))))
  ([in-filename out-filename]
   (let [s (binary-stl-file->svg in-filename)]
     ;; (svg/render-svg s out-filename)
     (spit out-filename (html s))
     s)))

