(ns cc.journeyman.walkmap.ocean
  "Deal with (specifically, at this stage, cull) ocean areas"
  (:require [cc.journeyman.walkmap.utils :refer [=ish]]))

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
  [stl]
  (assoc stl :facets (remove ocean? (:facets stl))))
