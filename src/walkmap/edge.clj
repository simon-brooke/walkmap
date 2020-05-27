(ns walkmap.edge
  "Essentially the specification for things we shall consider to be an edge.
  An edge is a line segment having just a start and an end, with no intervening
  nodes."
  (:require [clojure.math.numeric-tower :as m]
            [walkmap.polygon :refer [polygon?]]
            [walkmap.vertex :refer [ensure2d ensure3d vertex vertex= vertex?]]))

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

(defn centre
  "Return the vertex that represents the centre of this `edge`."
  [edge]
  (let [s (ensure3d (:start edge))
        e (ensure3d (:end edge))]
    (vertex
      (+ (:x s) (/ (- (:x e) (:x s)) 2))
      (+ (:y s) (/ (- (:y e) (:y s)) 2))
      (+ (:z s) (/ (- (:z e) (:z s)) 2)))))

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
  [& edges]
  (let [uvs (map unit-vector edges)]
    (every?
      #(vertex= % (first uvs))
      (rest uvs))))

(defn collinear?
  "True if edges `e1` and `e2` are collinear with one another."
  [e1 e2]
  (parallel?
    e1
    e2
    (if (vertex= (:start e1) (:start e2))
      {:start (:start e1) :end (:end e2)}
      {:start (:start e1) :end (:start e2)})))

(defn collinear2d?
  "True if the projections of edges `e1`, `e2` onto the x, y plane are
  collinear."
  [e1 e2]
  (collinear? {:start (ensure2d (:start e1)) :end (ensure2d (:end e1))}
              {:start (ensure2d (:start e2)) :end (ensure2d (:end e2))}))

(defn minimaxd
  "Apply function `f` to `coord` of the vertices at start and end of `edge`
  and return the result. Intended use case is `f` = `min` or `max`, `coord`
  is `:x`, `:y` or `:z`. No checks are made for sane arguments."
  [edge coord f]
  (apply f (list (coord (:start edge)) (coord (:end edge)))))

(defn on?
  "True if the vertex `v` is on the edge `e`."
  [e v]
  (let [p (ensure3d (:start e))
        q (ensure3d v)
        r (ensure3d (:end e))]
    (and
      (collinear? (edge p q) (edge q r))
      (<= (:x q) (max (:x p) (:x r)))
      (>= (:x q) (min (:x p) (:x r)))
      (<= (:y q) (max (:y p) (:y r)))
      (>= (:y q) (min (:y p) (:y r)))
      (<= (:z q) (max (:z p) (:z r)))
      (>= (:z q) (min (:z p) (:z r))))))

(defn on2d?
  "True if vertex `v` is on edge `e` when projected onto the x, y plane."
  [e v]
  (on? (edge (ensure2d (:start e)) (ensure2d (:end e))) v))

(defn overlaps2d?
  "True if the recangle in the x,y plane bisected by edge `e1` overlaps that
  bisected by edge `e2`. It is an error if either `e1` or `e2` is not an edge."
  [e1 e2]
  (when (and (edge? e1) (edge? e2))
    (and
      (> (minimaxd e1 :x max) (minimaxd e2 :x min))
      (< (minimaxd e1 :x min) (minimaxd e2 :x max))
      (> (minimaxd e1 :y max) (minimaxd e2 :y min))
      (< (minimaxd e1 :y min) (minimaxd e2 :y max)))))

;; Don't think I need this.
;; (defn orientation
;;   "Determine whether the ordered sequence of vertices `p`, `q` and `r` run
;;   clockwise, collinear or anticlockwise in the x,y plane."
;;   [p q r]
;;   (let [v (- (* (- (:y q) (:y p)) (- (:x r) (:x q)))
;;             (* (- (:x q) (:x p)) (- (:y r) (:y q))))]
;;     (cond
;;       (zero? v) :collinear
;;       (pos? v) :clockwise
;;       :else
;;       :anticlockwise)))

(defn intersection2d
  "The probability of two lines intersecting in 3d space is low, and actually
  that is mostly not something we're interested in. We're interested in
  intersection in the `x,y` plane. This function returns a vertex representing
  a point vertically over the intersection of edges `e1`, `e2` in the `x,y`
  plane, whose `z` coordinate is

  * 0 if both edges are 2d (i.e. have missing or zero `z` coordinates);
  * if one edge is 2d, then the point on the other edge over the intersection;
  * otherwise, the average of the z coordinates of the points on the two
  edges over the intersection.

  If no such intersection exists, `nil` is returned.

  It is an error, and an exception will be thrown, if either `e1` or `e2` is
  not an edge."
  [e1 e2]
  (if (and (edge? e1) (edge? e2))
    (when
      (overlaps2d? e1 e2) ;; relatively cheap check
      (if
        (collinear2d? e1 e2)
        ;; any point within the overlap will do, but we'll pick the end of e1
        ;; which is on e2
        (if (on2d? e2 (:start e1)) (:start e1) (:end e1))
        ;; blatantly stolen from
        ;; https://gist.github.com/cassiel/3e725b49670356a9b936
        (let [x1 (:x (:start e1))
              x2 (:x (:end e1))
              x3 (:x (:start e2))
              x4 (:x (:end e2))
              y1 (:y (:start e1))
              y2 (:y (:end e1))
              y3 (:y (:start e2))
              y4 (:y (:end e2))
              denom (- (* (- x1 x2) (- y3 y4))
                       (* (- y1 y2) (- x3 x4)))
              x1y2-y1x2 (- (* x1 y2) (* y1 x2))
              x3y4-y3x4 (- (* x3 y4) (* y3 x4))
              px-num (- (* x1y2-y1x2 (- x3 x4))
                        (* (- x1 x2) x3y4-y3x4))
              py-num (- (* x1y2-y1x2 (- y3 y4))
                        (* (- y1 y2) x3y4-y3x4))
              result (when-not (zero? denom)
                       (vertex (/ px-num denom) (/ py-num denom)))]
          (when (and result (on2d? e1 result) (on2d? e2 result)) result))))
    (throw (IllegalArgumentException.
             (str
               "Both `e1` and `e2` must be edges."
               (map #(or (:kind %) (type %)) [e1 e2]))))))

