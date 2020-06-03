(ns walkmap.routing
  "Finding optimal routes to traverse a map."
  (:require [walkman.edge :as e]
            [walkmap.path :as p]
            [walkmap.polygon :as q]
            [walkmap.superstructure :as s]
            [walkmap.tag :as t]
            [walkmap.utils :as u]
            [walkmap.vertex :as v]))

;; Breadth first search is a good algorithm for terrain in which all steps have
;; equal, but in our world (like the real world), they don't.

;; Reading list:
;;
;; https://en.wikipedia.org/wiki/A*_search_algorithm
;; https://www.redblobgames.com/pathfinding/a-star/introduction.html
;; https://faculty.nps.edu/ncrowe/opmpaper2.htm
;;
;; See https://simon-brooke.github.io/the-great-game/codox/Pathmaking.html

(defn traversable?
  "True if this object can be considered as part of the walkmap."
  [object]
  (and
    (or
      (and
        (q/polygon? object)
        (:centre object))
      (p/path? object))
    (not (t/tagged? object :no-traversal))))

(defn extend-frontier
  "Return a sequence like `frontier` with all of these `candidates` which are
  not already members either of `frontier` or of `rejects` appended."
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

(defn route
  ;; NOT YET GOOD ENOUGH! Simple breadth first, and although it will
  ;; reach the goal
  ([from to s search-radius]
   (loop [f from
          t to
          frontier (extend-frontier
                     nil
                     (s/neighbour-ids
                       (s/nearest s from :centre search-radius)
                       traversable?
                       s))
          visited nil]
     (let [here (s/retrieve (first frontier) s)]
       (cond
         (< (e/length (e/edge (:centre here)) to) search-radius)
         ;; close enough
         (apply p/path (cons (:centre here) track))
         (empty? (rest frontier))
         ;; failed
         nil
         :else
         (recur
           f
           t
           (extend-frontier
             (rest frontier)
             (neighbour-ids here traversable? s)
             visited)
           (cons here visited)))))))

