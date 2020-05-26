(ns walkmap.tag
  "Code for tagging, untagging, and finding tags on objects. Note the use of
  the namespaced keyword, `:walkmap.tag/tags`, denoted in this file `::tags`.
  This is in an attempt to avoid name clashes with other uses of this key."
  (:require [clojure.set :refer [difference union]]))

(defn tagged?
  "True if this `object` is tagged with each of these `tags`."
  [object & tags]
  (if
    (map? object)
    (if
      (every? keyword? tags)
      (let [ot (::tags object)]
        (and
          (set? ot)
          (every? ot tags)
          true))
      (throw (IllegalArgumentException.
               (str "Must be keyword(s): " (map type tags)))))
    (throw (IllegalArgumentException.
             (str "Must be a map: " (type object))))))

(defn tag
  "Return an object like this `object` but with these `tags` added to its tags,
  if they are not already present."
  [object & tags]
  (if
    (map? object)
    (if
      (every? keyword? tags)
      (assoc object ::tags (union (set tags) (::tags object)))
      (throw (IllegalArgumentException.
               (str "Must be keyword(s): " (map type tags)))))
    (throw (IllegalArgumentException.
             (str "Must be a map: " (type object))))))

(defn untag
  "Return an object like this `object` but with these `tags` removed from its
  tags, if present."
  [object & tags]
  (if
    (map? object)
    (if
      (every? keyword? tags)
      (assoc object ::tags (difference (::tags object) (set tags)))
      (throw (IllegalArgumentException.
               (str "Must be keywords: " (map type tags)))))
    (throw (IllegalArgumentException.
             (str "Must be a map: " (type object))))))
