(ns walkmap.geometry
  (:require [clojure.math.combinatorics :as combo]
            [clojure.math.numeric-tower :as m]
            [walkmap.edge :as edge]
            [walkmap.path :refer [path? polygon->path]]
            [walkmap.polygon :refer [polygon?]]
            [walkmap.vertex :as vertex]))

(defn collinear?
  "True if these vertices `v1`, `v2`, `v3` are colinear; false otherwise."
  ;; This is failing...
  ;; see http://www.ambrsoft.com/TrigoCalc/Line3D/LineColinear.htm
  [v1 v2 v3]
  (let [a (m/sqrt (+ (- (:x v2) (:x v1)) (- (:y v2) (:y v1)) (- (:z v2) (:z v1))))
        b (m/sqrt (+ (- (:x v3) (:x v1)) (- (:y v3) (:y v1)) (- (:z v3) (:z v1))))
        c (m/sqrt (+ (- (:x v3) (:x v2)) (- (:y v3) (:y v2)) (- (:z v3) (:z v2))))]
    (not
      (and
        (> (+ a b) c)
        (> (+ a c) b)
        (> (+ b c) a)))))

;; (collinear? {:x 0 :y 0 :z 0} {:x 1 :y 1 :z 1} {:x 7 :y 7 :z 7})
;; (collinear? {:x 0 :y 0 :z 0} {:x 1 :y 2 :z 1} {:x 7 :y 7 :z 7})
;; (collinear? {:x 0 :y 0 :z 0} {:x 0 :y 2 :z 0} {:x 0 :y 3 :z 0})

;; (def v1 {:x 0 :y 0 :z 0})
;; (def v2  {:x 0 :y 2 :z 0})
;; (def v3 {:x 0 :y 7 :z 0})

;; (def a (m/sqrt (+ (- (:x v2) (:x v1)) (- (:y v2) (:y v1)) (- (:z v2) (:z v1)))))
;; a
;; (def b (m/sqrt (+ (- (:x v3) (:x v1)) (- (:y v3) (:y v1)) (- (:z v3) (:z v1)))))
;; b
;; (def c (m/sqrt (+ (- (:x v3) (:x v2)) (- (:y v3) (:y v2)) (- (:z v3) (:z v2)))))
;; c

;; (> (+ b c) a)
;; (> (+ a c) b)
;; (> (+ a c) c)

(defn on?
  "True if the vertex `v` is on the edge `e`."
  [e v]
  (let [p (vertex/ensure3d (:start e))
        q (vertex/ensure3d v)
        r (vertex/ensure3d (:end e))]
    (and
      (collinear? p q r)
      (<= (:x q) (max (:x p) (:x r)))
      (>= (:x q) (min (:x p) (:x r)))
      (<= (:y q) (max (:y p) (:y r)))
      (>= (:y q) (min (:y p) (:y r)))
      (<= (:z q) (max (:z p) (:z r)))
      (>= (:z q) (min (:z p) (:z r))))))


