(ns walkmap.utils
  "Miscellaneous utility functions."
  (:require [walkmap.path :as p]
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
  ;; TODO: it's possibly a design mistake that I'm currently distinguishing
  ;; between polygons and paths on the basis that one has `:vertices` and
  ;; the other has `:nodes`. Possibly it would be better to have a key
  ;; `:closed` which was `true` for polygons, `false` (or missing) for
  ;; paths.
  [o]
  (cond
    (v/vertex? o) (list o)
    (q/polygon? o) (:vertices o)
    (p/path? o) (:nodes o)))
