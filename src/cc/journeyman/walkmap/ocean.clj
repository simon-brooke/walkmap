(ns cc.journeyman.walkmap.ocean
  "Deal with (specifically, at this stage, cull) ocean areas"
  (:require [cc.journeyman.walkmap.polygon :refer [triangle?]]
            [cc.journeyman.walkmap.stl :refer [stl?]]
            [cc.journeyman.walkmap.utils :refer [=ish]]))

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
    #(=ish % *sea-level*)
    (map :z (:vertices facet))))

(defn cull-ocean-facets
  "Ye cannae walk on water. Remove all facets from this `stl` structure which
  are at sea level."
  [stl-or-seq]
  (cond
    (and (seq? stl-or-seq) 
         (every? triangle? stl-or-seq)) (remove ocean? stl-or-seq)
    (stl? stl-or-seq)(assoc stl-or-seq :facets 
                            (cull-ocean-facets (:facets stl-or-seq)))
    :else (throw (IllegalArgumentException. 
                  "Expected STL structure, or sequence of triangles."))))
