(ns walkmap.polygon
  "Essentially the specification for things we shall consider to be polygons."
  (:require [walkmap.vertex :refer [vertex?]]))

(defn polygon?
  "True if `o` satisfies the conditions for a polygon. A polygon shall be a
  map which has a value for the key `:vertices`, where that value is a sequence
  of vertices."
  [o]
  (let
    [v (:vertices o)]
    (and
      (coll? v)
      (> (count v) 2)
      (every? vertex? v)
      (:id o)
      (or (nil? (:kind o)) (= (:kind o) :polygon)))))


