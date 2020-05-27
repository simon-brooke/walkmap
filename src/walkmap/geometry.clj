(ns walkmap.geometry
  (:require [clojure.math.combinatorics :as combo]
            [clojure.math.numeric-tower :as m]))

(defn =ish
  "True if numbers `n1`, `n2` are roughly equal; that is to say, equal to
  within `tolerance` (defaults to one part in a million)."
  ([n1 n2]
   (if (and (number? n1) (number? n2))
     (let [m (m/abs (min n1 n2))
           t (if (zero? m) 0.000001 (* 0.000001 m))]
       (=ish n1 n2 t))
     (= n1 n2)))
  ([n1 n2 tolerance]
   (if (and (number? n1) (number? n2))
     (< (m/abs (- n1 n2)) tolerance)
     (= n1 n2))))
