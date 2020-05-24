(ns walkmap.edge
  "Essentially the specification for things we shall consider to be an edge.
  An edge is a line segment having just a start and an end, with no intervening
  nodes."
  (:require [clojure.math.numeric-tower :as m]
            [walkmap.path :refer [path? polygon->path]]
            [walkmap.polygon :refer [polygon?]]
            [walkmap.vertex :refer [ensure3d vertex?]]))

(defn edge?
  "True if `o` satisfies the conditions for a path. A path shall be a map
  having the keys `:start` and `:end`, such that the values of each of those
  keys shall be a vertex."
  [o]
  (and
    (map? o)
    (vertex? (:start o))
    (vertex? (:end o))))

(defn path->edges
  "if `o` is a path, a polygon, or a sequence of vertices, return a sequence of
  edges representing that path, polygon or sequence."
  [o]
  (cond
    (seq? o)
    (when
      (and
        (vertex? (first o))
        (vertex? (first (rest o))))
      (cons
        {:start (first o)
         :end (first (rest o))}
        (path->edges (rest o))))
    (path? o)
    (path->edges (:nodes o))
    (polygon? o)
    (path->edges (polygon->path o))))

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

