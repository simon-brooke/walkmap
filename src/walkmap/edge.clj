(ns walkmap.edge
  "Essentially the specification for things we shall consider to be an edge.
  An edge is a line segment having just a start and an end, with no intervening
  nodes."
  (:require [clojure.math.numeric-tower :as m]
            [walkmap.polygon :refer [polygon?]]
            [walkmap.vertex :refer [ensure3d vertex?]]))

(defn edge
  "Return an edge between vertices `v1` and `v2`."
  [v1 v2]
  (if
    (and (vertex? v1) (vertex? v2))
    {:kind :edge :id (keyword (gensym "edge")) :start v1 :end v2}
    (throw (IllegalArgumentException. "Must be vertices."))))

(defn edge?
  "True if `o` satisfies the conditions for a edge. An edge shall be a map
  having the keys `:start` and `:end`, such that the values of each of those
  keys shall be a vertex."
  [o]
  (and
    (map? o)
    (vertex? (:start o))
    (vertex? (:end o))))

(defn length
  "Return the length of the edge `e`."
  [e]
  (let [start (ensure3d (:start e))
        end (ensure3d (:end e))]
    (m/sqrt
      (reduce
        +
        (map
          #(m/expt (- (% end) (% start)) 2)
          [:x :y :z])))))

(defn unit-vector
  "Return an vertex parallel to `e` starting from the coordinate origin. Two
  edges which are parallel will have the same unit vector."
  [e]
  (let [e' {:start (ensure3d (:start e)) :end (ensure3d (:end e))}
        l (length e')]
    (reduce
      merge
      {}
      (map
        (fn [k]
          {k (/ (- (k (:end e')) (k (:start e'))) l)})
        [:x :y :z]))))

(defn parallel?
  "True if all `edges` passed are parallel with one another."
  ;; TODO: this bears being wary about, dealing with floating point arithmetic.
  ;; Keep an eye out for spurious errors.
  [& edges]
  (let [uvs (map unit-vector edges)]
    (every?
      #(= % (first uvs))
      (rest uvs))))

(defn collinear?
  "True if edges `e1` and `e2` are collinear with one another."
  [e1 e2]
  (parallel?
    e1
    e2
    {:start (:start e1) :end (:start e2)}))

