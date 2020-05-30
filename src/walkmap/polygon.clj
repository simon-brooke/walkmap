(ns walkmap.polygon
  "Essentially the specification for things we shall consider to be polygons."
  (:require [clojure.string :as s]
            [walkmap.utils :refer [kind-type]]
            [walkmap.vertex :refer [vertex?]]))

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
      (:walkmap.id/id o)
      (or (nil? (:kind o)) (= (:kind o) :polygon)))))

(defn polygon
  [vertices]
  (when-not (every? vertex? vertices)
    (throw (IllegalArgumentException.
             (str
               "Each item on path must be a vertex: "
               (s/join " " (map kind-type (remove vertex? vertices)))))))
  {:vertices vertices :walkmap.id/id (keyword (gensym "poly")) :kind :polygon})
