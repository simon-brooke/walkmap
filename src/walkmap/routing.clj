(ns walkmap.core
  "Finding optimal routes to traverse a map."
    (:require [walkmap.path :as p]
            [walkmap.polygon :as q]
            [walkmap.stl :as s]
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
