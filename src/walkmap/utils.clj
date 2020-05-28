(ns walkmap.utils
  "Miscellaneous utility functions."
  (:require [clojure.math.numeric-tower :as m]))

(defn deep-merge
  "Recursively merges maps. If vals are not maps, the last value wins."
  ;; TODO: not my implementation, not sure I entirely trust it.
  [& vals]
  (if (every? map? vals)
    (apply merge-with deep-merge vals)
    (last vals)))

;; (defn vertices
;;   "If `o` is an object with vertices, return those vertices, else nil."
;;   [o]
;;   (cond
;;     (v/vertex? o) (list o)
;;     (q/polygon? o) (:vertices o)
;;     (p/path? o) (:vertices o)))

(defn truncate
  "If string `s` is more than `n` characters long, return the first `n`
  characters; otherwise, return `s`."
  [s n]
  (if (and (string? s) (number? n) (> (count s) n))
    (subs s 0 n)
    s))


