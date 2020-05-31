(ns walkmap.polygon
  "Essentially the specification for things we shall consider to be polygons."
  (:require [clojure.string :as s]
            [walkmap.edge :as e]
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

(defn triangle?
  [o]
  (and
    (coll? o)
    (= (count (:vertices o)) 3)))

(defn polygon
  [vertices]
  (when-not (every? vertex? vertices)
    (throw (IllegalArgumentException.
             (str
               "Each item on path must be a vertex: "
               (s/join " " (map kind-type (remove vertex? vertices)))))))
  {:vertices vertices :walkmap.id/id (keyword (gensym "poly")) :kind :polygon})

(defn gradient
  "Return a unit vector representing the gradient across `triangle`."
  [triangle]
  (when-not (triangle? triangle)
    (throw (IllegalArgumentException.
             (s/join " " ["Must be a triangle:" (kind-type triangle)]))))
  (let [order (sort #(max (:z %1) (:z %2)) (:vertices triangle))
        highest (first order)
        lowest (last order)]
     (e/unit-vector (e/edge lowest highest))))

