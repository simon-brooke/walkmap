(ns walkmap.geometry
  (:require [clojure.math.combinatorics :as combo]
            [clojure.math.numeric-tower :as m]
            [walkmap.edge :as e]
            [walkmap.path :refer [path? polygon->path]]
            [walkmap.polygon :refer [polygon?]]
            [walkmap.vertex :as v]))

(defn on?
  "True if the vertex `v` is on the edge `e`."
  [e v]
  (let [p (v/ensure3d (:start e))
        q (v/ensure3d v)
        r (v/ensure3d (:end e))]
    (and
      (e/collinear? p q r)
      (<= (:x q) (max (:x p) (:x r)))
      (>= (:x q) (min (:x p) (:x r)))
      (<= (:y q) (max (:y p) (:y r)))
      (>= (:y q) (min (:y p) (:y r)))
      (<= (:z q) (max (:z p) (:z r)))
      (>= (:z q) (min (:z p) (:z r))))))


