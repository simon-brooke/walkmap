(ns walkmap.tag
  "Code for tagging, untagging, and finding tags on objects. Note the use of
  the namespaced keyword, `:walkmap.tag/tags`, denoted in this file `::tags`.
  This is in an attempt to avoid name clashes with other uses of this key."
  (:require [clojure.set :refer [difference union]]
            [taoensso.timbre :as l]
            [walkmap.utils :refer [kind-type]]))

(defn tagged?
  "True if this `object` is tagged with each of these `tags`. It is an error
  (and an exception will be thrown) if

  1. `object` is not a map;
  2. any of `tags` is not a keyword."
  [object & tags]
  (when-not (map? object)
    (throw (IllegalArgumentException.
             (str "Must be a map: " (kind-type object)))))
  (let [tags' (flatten tags)]
    (when-not (every? keyword? tags')
      (throw (IllegalArgumentException.
               (str "Must be keywords: " (map kind-type tags')))))
      (let [ot (::tags object)]
        (and
          (set? ot)
          (every? ot tags')))))

(defn tag
  "Return an object like this `object` but with these `tags` added to its tags,
  if they are not already present. It is an error (and an exception will be
  thrown) if

  1. `object` is not a map;
  2. any of `tags` is not a keyword or sequence of keywords.

  It's legal to include sequences of keywords in `tags`, so that users can do
  useful things like `(tag obj (map keyword some-strings))`."
  [object & tags]
  (l/debug "Tagging" (kind-type object) "with" tags)
  (when-not (map? object)
    (throw (IllegalArgumentException.
             (str "Must be a map: " (kind-type object)))))
  (let [tags' (flatten tags)]
    (when-not (every? keyword? tags')
      (throw (IllegalArgumentException.
               (str "Must be keywords: " (map kind-type tags')))))
        (assoc object ::tags (union (set tags') (::tags object)))))

(defmacro tags
  "Return the tags of this object, if any."
  [object]
  `(::tags ~object))

(defn untag
  "Return an object like this `object` but with these `tags` removed from its
  tags, if present. It is an error (and an exception will be thrown) if

  1. `object` is not a map;
  2. any of `tags` is not a keyword or sequence of keywords."
  [object & tags]
  (when-not (map? object)
    (throw (IllegalArgumentException.
             (str "Must be a map: " (kind-type object)))))
  (let [tags' (flatten tags)]
    (when-not (every? keyword? tags')
      (throw (IllegalArgumentException.
               (str "Must be keywords: " (map kind-type tags')))))
    (update-in object [:walkmap.tag/tags] difference (set tags'))))
