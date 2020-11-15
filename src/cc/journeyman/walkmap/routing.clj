(ns cc.journeyman.walkmap.routing
  "Finding optimal routes to traverse a map."
  (:require [clojure.math.numeric-tower :as m :only [expt]]
            [clojure.set :refer [intersection]]
            [clojure.string :as cs :only [join]]
            [search.core :refer [a*]]
            [cc.journeyman.walkmap.edge :as e]
            [cc.journeyman.walkmap.path :as p]
            [cc.journeyman.walkmap.polygon :as q]
            [cc.journeyman.walkmap.superstructure :as s]
            [cc.journeyman.walkmap.tag :as t]
            [cc.journeyman.walkmap.utils :as u]
            [cc.journeyman.walkmap.vertex :as v]))

;; Breadth first search is a good algorithm for terrain in which all steps have
;; equal, but in our world (like the real world), they don't.

;; Reading list:
;;
;; https://en.wikipedia.org/wiki/A*_search_algorithm
;; https://www.redblobgames.com/pathfinding/a-star/introduction.html
;; https://faculty.nps.edu/ncrowe/opmpaper2.htm
;;
;; See https://simon-brooke.github.io/the-great-game/codox/Pathmaking.html

(def ^:dynamic *gradient-exponent*
  "The exponent to be applied to `(inc (:z (unit-vector from to)))`
  of a path segment to calculate the gradient-related part of the
  cost of traversal. Dynamic, because we will want to tune this."
  2)

(def ^:dynamic *traversals-exponent*
  "The (expected to be negative) exponent to be applied to the number
  of traversals of a path to compute the road bonus. Paths more travelled by
  should have larger bonuses, but not dramatically so - so the increase in
  bonus needs to scale significantly less than linearly with the number
  of traversals. Dynamic, because we will want to tune this."
  -2)

(defn traversable?
  "True if this `object` is a polygon which can be considered as part of
  the walkmap."
  [object]
  (and
    (q/polygon? object)
    (:centre object)
    (not (t/tagged? object :no-traversal))))

(declare traversal-cost)

(defn vertices-traversal-cost
  [vertices s]
  (reduce
    +
    (map
      #(traversal-cost %1 %2 s)
      (v/check-vertices vertices)
      (rest vertices))))

(defn path-traversal-cost
  [path s]
  (vertices-traversal-cost (:vertices (p/check-path path)) s))

(defn barriers-crossed
  "Search superstructure `s` and return a sequence of barriers, if any, which
  obstruct traversal from vertex `from` to vertex `to`."
  [from to s]
  ;; TODO: implement
  '())

(defn crossing-penalty
  "TODO: should return the cost of crossing this `barrier`, initially mainly
  a watercourse, on the axis from vertex `from` to vertex `to`. in the context
  of superstructure `s`. If there's a bridge, ferry or other crossing mechanism
  in `s` at the intersection of the vertex and the barrier, then the penalty
  should be substantially less than it would otherwise be."
  [barrier from to s]
  ;; TODO: implement
  0)

(defn gradient-cost
  "Compute the per-unit-distance cost of traversing this `edge`."
  [edge]
  (let [g (:z (e/unit-vector edge))]
    (if (pos? g)
      (m/expt (inc g) *gradient-exponent*)
      1)))

;; (gradient-cost (e/edge (v/vertex 0 0 0) (v/vertex 0 1 0)))
;; (gradient-cost (e/edge (v/vertex 0 0 0) (v/vertex 0 2 0)))
;; (gradient-cost (e/edge (v/vertex 0 0 0) (v/vertex 0 1 1)))
;; (gradient-cost (e/edge (v/vertex 0 0 0) (v/vertex 0 2 1)))
;; (gradient-cost (e/edge (v/vertex 0 0 0) (v/vertex 0 1 0.0001)))

(defn best-road
  "Find the best traversable path which links the vertices `from` and `to`
  in this superstructure `s`, or nil if there are none."
  [from to s]
  (let [f (fn [v] (set (s/touching v p/path? s)))]
    (first
      (sort-by
        ;;; I... chose the path more travelled by.
        #(or (:traversals %) 0)
        (filter traversable? (intersection (f from) (f to)))))))

(defn road-bonus
  "Calculate the road bonus of the edge represented by the vertices `from`,
  `to`, in the context of the superstructure `s`. Obviously there only is
  such a bonus if there actually is an existing thoroughfare to use. Road
  bonuses scale with some fractional exponent of the number of traversals
  which have been made of the road segment in question."
  [from to s]
  (let [best (best-road from to s)]
    (when (:traversals best)
      (m/expt (:traversals best) *traversals-exponent*))))

(defn traversal-cost
  "Return the traversal cost of the edge represented by the vertices `from`,
  `to`, in the context of the superstructure `s`. It is legitimate to pass
  `nil` as the `to` argument, in which case the result will be zero, in order
  to allow `reduce` to be used to compute total path costs."
  [from to s]
  (if (nil? to)
    0
    (let [edge (e/edge from to)
          distance (e/length edge)]
      (/
        (+
          (* distance
             (gradient-cost edge))
          (reduce +
                  (map
                    #(crossing-penalty [% from to s])
                    (barriers-crossed from to s))))
        (or (road-bonus from to s) 1)))))

;; (def p '({:x 1.40625, :y 0, :kind :vertex, :walkmap.id/id :vert_1-40625_0}
;;        {:x 1.40625, :y -10.703125, :kind :vertex, :walkmap.id/id :vert_1-40625_-10-703125}
;;        {:x 7.578125, :y -10.703125, :kind :vertex, :walkmap.id/id :vert_7-578125_-10-703125}
;;        {:x 7.578125, :y 0, :kind :vertex, :walkmap.id/id :vert_7-578125_0}
;;        {:x 2.171875, :y -0.765625, :kind :vertex, :walkmap.id/id :vert_2-171875_-0-765625}
;;        {:x 6.8125, :y -0.765625, :kind :vertex, :walkmap.id/id :vert_6-8125_-0-765625}))
;; (v/check-vertices p)
;; (def p' (p/path p))

;; (traversal-cost (first p) (nth p 1) {})
;; (vertices-traversal-cost p {})
;; (path-traversal-cost (p/path p))

(defn extend-frontier
  "Return a sequence like `frontier` with all of these `candidates` which are
  not already members either of `frontier` or of `rejects` appended. Assumes
  candidates are traversable."
  ([frontier candidates]
   (extend-frontier frontier candidates nil))
  ([frontier candidates rejects]
  (if
    (empty? frontier)
    candidates
    (let [fs (set (concat frontier rejects))]
      (concat frontier (remove fs candidates))))))

;; (extend-frontier '(1 2 3 4 5) '(7 3 6 2 9 8) '(6 8))
;; (extend-frontier '(1 2 3 4 5) '(7 3 6 2 9 8))
;; (extend-frontier '(1 2 3 4 5) '())
;; (extend-frontier '(1 2 3 4 5) nil)
;; (extend-frontier nil '(1 2 3 4 5))

(def ^:dynamic *route-goal*
  "The goal of the route currently sought."
  nil)

(defn find-traversable-facet
  "Return the nearest traversable walkmap facet within `search-radius` of
  `target`, or throw an exception if none is found."
  [target search-radius s]
  (let [r (s/nearest s target traversable? search-radius)]
    (when-not r (throw
                  (Exception.
                    (cs/join " " ["Unable to find walkable facet within"
                                  search-radius
                                  "of"
                                  target]))))
    r))

(defn route
  ;; architectural problem: needs to return not the route, but a modified
  ;; superstructure with the new route stored in it.
  ([from to s]
   (route from to s traversal-cost 5))
  ([from to s cost-fn search-radius]
   (let [from' (find-traversable-facet from search-radius s)
         to' (find-traversable-facet to search-radius s)]
     (a* from'
         #(v/vertex= % (:centre to')) ;; goal?-fn - 'have we got there yet?'
         #(cost-fn %1 %2 s)           ;; distance-fn - what is the distance/cost
                                      ;; between these vertices?
         #(e/length (e/edge (:centre %) to))
                                      ;; heuristic: how far to the end goal
         #(s/neighbours % traversable? s)
                                      ;; neighbours-fn - return the traversable
                                      ;; neighbours of the current facet
         (int (* search-radius (e/length (e/edge from to))))
                                      ;; how long a path we'll accept
         ))))