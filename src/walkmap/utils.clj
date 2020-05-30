(ns walkmap.utils
  "Miscellaneous utility functions."
  (:require [clojure.math.numeric-tower :as m]))

(defn deep-merge
  "Recursively merges maps. If vals are not maps, the last value wins."
  ;; TODO: not my implementation, not sure I entirely trust it.
  ;; TODO TODO: if we are to successfully merge walkmap objects, we must
  ;; return, on each object, the union of its tags if any.
  [& vals]
  (if (every? map? vals)
    (apply merge-with deep-merge vals)
    (last vals)))

(defn truncate
  "If string `s` is more than `n` characters long, return the first `n`
  characters; otherwise, return `s`."
  [s n]
  (if (and (string? s) (number? n) (> (count s) n))
    (subs s 0 n)
    s))

(defn kind-type
  "Identify the type of an `object`, e.g. for logging. If it has a `:kind` key,
  it's one of ours, and that's what we want. Otherwise, we want its type; but
  the type of `nil` is `nil`, which doesn't get printed when assembling error
  ,essages, so return \"nil\"."
  [object]
  (or (:kind object) (type object) "nil"))
