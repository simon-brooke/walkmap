(ns cc.journeyman.walkmap.polygon
  "Essentially the specification for things we shall consider to be polygons."
  (:require [clojure.string :as s]
            [cc.journeyman.walkmap.edge :as e]
            [cc.journeyman.walkmap.tag :as t]
            [cc.journeyman.walkmap.utils :refer [check-kind-type
                                   check-kind-type-seq
                                   kind-type
                                   not-yet-implemented]]
            [cc.journeyman.walkmap.vertex :refer [check-vertex check-vertices vertex vertex?]]))

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

(defmacro check-polygon
  "If `o` is not a polygon, throw an `IllegalArgumentException` with an
  appropriate message; otherwise, returns `o`. Macro, so exception is thrown
  from the calling function."
  [o]
  `(check-kind-type ~o polygon? :polygon))

(defmacro check-polygons
  "If `o` is not a sequence of polygons, throw an `IllegalArgumentException` with an
  appropriate message; otherwise, returns `o`. Macro, so exception is thrown
  from the calling function."
  [o]
  `(check-kind-type-seq ~o polygon? :polygon))

(defn triangle?
  "True if `o` satisfies the conditions for a triangle. A triangle shall be a
  polygon with exactly three vertices."
  [o]
  (and
    (coll? o)
    (= (count (:vertices o)) 3)))

(defmacro check-triangle
  "If `o` is not a triangle, throw an `IllegalArgumentException` with an
  appropriate message; otherwise, returns `o`. Macro, so exception is thrown
  from the calling function."
  [o]
  `(check-kind-type ~o triangle? :triangle))

(defn polygon
  "Return a polygon constructed from these `vertices`."
  [& vertices]
  (if
    (> (count vertices) 2)
    {:vertices (check-vertices vertices)
     :walkmap.id/id (keyword (gensym "poly"))
     :kind :polygon}
    (throw (IllegalArgumentException.
             "A polygon must have at least 3 vertices."))))

(defn rectangle
  "Return a rectangle, with edges aligned east-west and north-south, whose
  south-west corner is the vertex `vsw` and whose north-east corner is the
  vertex `vne`."
  [vsw vne]
  ;; we can actually create any rectangle in the xy plane based on two opposite
  ;; corners, but the maths are a bit to advanced for me today. TODO: do it!
  (let [vnw (vertex (:x (check-vertex vsw))
                    (:y (check-vertex vne))
                    (/ (reduce + (map #(or (:z %) 0) [vsw vne])) 2))
        vse (vertex (:x vne)
                    (:y vsw)
                    (/ (reduce + (map #(or (:z %) 0) [vsw vne])) 2))
        height-order (sort-by :z [vsw vne])]
    (t/tag
      (assoc
        (polygon vsw vnw vne vse)
        :gradient
        (e/unit-vector (e/edge (first height-order) (last height-order)))
        :centre
        (vertex (+ (:x vsw) (/ (- (:x vne) (:x vsw)) 2))
                (+ (:x vsw) (/ (- (:y vne) (:y vsw)) 2))
                (:z vse)))
      :rectangle)))

;; (rectangle (vertex 1 2 3) (vertex 7 9 4))

(defn gradient
  "Return a polygon like `triangle` but with a key `:gradient` whose value is a
  unit vector representing the gradient across `triangle`."
  [triangle]
  (let [order (sort #(max (:z %1) (:z %2))
                    (:vertices (check-triangle triangle)))
        highest (first order)
        lowest (last order)]
     (assoc triangle :gradient (e/unit-vector (e/edge lowest highest)))))

(defn triangle-centre
  "Return a canonicalised `facet` (i.e. a triangular polygon) with an added
  key `:centre` whose value represents the centre of this facet in 3
  dimensions. This only works for triangles, so is here not in
  `walkmap.polygon`. It is an error (although no exception is currently
  thrown) if the object past is not a triangular polygon."
  [facet]
  (let [vs (:vertices (check-triangle facet))
        v1 (first vs)
        opposite (e/edge (nth vs 1) (nth vs 2))
        oc (e/centre opposite)]
      (assoc
      facet
      :centre
      (vertex
        (+ (:x v1) (* (- (:x oc) (:x v1)) 2/3))
        (+ (:y v1) (* (- (:y oc) (:y v1)) 2/3))
        (+ (:z v1) (* (- (:z oc) (:z v1)) 2/3))))))

(defn centre
  [poly]
  (case (count (:vertices (check-polygon poly)))
    3 (triangle-centre poly)
    ;; else
    (throw
      (UnsupportedOperationException.
        "The general case of centre for polygons is not yet implemented."))))

(defmacro on2dtriangle?
  "Is the projection of this `vertex` on the x, y plane within the
  projection of this triangle on that plane?"
  [vertex poly]
  `(not-yet-implemented "on2d? for triangles."))

(defn on2drectangle?
  "Is the projection of this `vertex` on the x, y plane within the
  projection of this rectangle on that plane?"
  [vertex rectangle]
  (let [xo (sort-by :x (:vertices rectangle))
        yo (sort-by :x (:vertices rectangle))]
    (and
      (< (:x (first xo)) (:x vertex) (:x (last xo)))
      (< (:y (first yo)) (:y vertex) (:y (last yo))))))

(defmacro on2d?
  "Is the projection of this `vertex` on the x, y plane within the
  projection of this polygon `poly` on that plane?"
  [vertex poly]
  `(cond
    (rectangle? ~poly) (on2drectangle? ~vertex ~poly)
    (triangle? ~poly) (on2dtriangle? ~vertex ~poly)
    :else
    (not-yet-implemented "general case of on2d? for polygons.")))
