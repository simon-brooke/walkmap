(ns walkmap.utils
  "Miscellaneous utility functions."
  (:require [clojure.math.numeric-tower :as m]
            [walkmap.path :as p]
            [walkmap.polygon :as q]
            [walkmap.vertex :as v]))

(defn deep-merge
  "Recursively merges maps. If vals are not maps, the last value wins."
  ;; TODO: not my implementation, not sure I entirely trust it.
  [& vals]
  (if (every? map? vals)
    (apply merge-with deep-merge vals)
    (last vals)))

(defn vertices
  "If `o` is an object with vertices, return those vertices, else nil."
  [o]
  (cond
    (v/vertex? o) (list o)
    (q/polygon? o) (:vertices o)
    (p/path? o) (:vertices o)))

