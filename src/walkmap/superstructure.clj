(ns walkmap.superstructure
  "single indexing structure for walkmap objects"
  (:require [walkmap.path :as p]
            [walkmap.polygon :as q]
            [walkmap.stl :as s]
            [walkmap.utils :as u]
            [walkmap.vertex :as v]))

;; TODO: Think about reification/dereification. How can we cull a polygon, if
;; some vertices still index it? I *think* that what's needed is that when
;; we store something in the superstructure, we replace all its vertices (and
;; other dependent structures, if any with their ids - as well as, obviously,
;; adding/merging those vertices/dependent structures into the superstructure
;; as first class objects in themselves. That means, for each identified thing,
;; the superstructure only contains one copy of it.
;;
;; The question then is, when we want to do things with those objects, do we
;; exteract a copy with its dependent structures fixed back up (reification),
;; or do we indirect through the superstructure every time we want to access
;; them? In a sense, the copy in the superstructure is the 'one true copy',
;; but it may become very difficult then to have one true copy of the
;; superstructure - unless we replace the superstructure altogether with a
;; database, which may be the Right Thing To Do.

(defn index-vertex
  "Return a superstructure like `s` in which object `o` is indexed by vertex
  `v`. It is an error (and an exception may be thrown) if

  1. `s` is not a map;
  2. `o` is not a map;
  3. `o` does not have a value for the key `:id`;
  4. `v` is not a vertex."
  [s o v]
  (if-not (v/vertex? o)
    (if (:id o)
      (if (v/vertex? v)
        (let [vi (or (:vertex-index s) {})
              current (or (vi (:id v)) {})]
          ;; deep-merge doesn't merge sets, only maps; so at this
          ;; stage we need to build a map.
          (assoc vi (:id v) (assoc current (:id o) (:id v))))
        (throw (IllegalArgumentException. "Not a vertex: " v)))
      (throw (IllegalArgumentException. (u/truncate (str "No `:id` value: " o) 80))))
    ;; it shouldn't actually be an error to try to index a vertex, but it
    ;; also isn't useful to do so, so I'd be inclined to ignore it.
    (:vertex-index s)))

(defn index-vertices
  "Return a superstructure like `s` in which object `o` is indexed by its
  vertices. It is an error (and an exception may be thrown) if

  1. `s` is not a map;
  2. `o` is not a map;
  3. `o` does not have a value for the key `:id`."
  [s o]
  (assoc
    s
    :vertex-index
    (reduce
      u/deep-merge
      (map
        #(index-vertex s o %)
        (u/vertices o)))))

(defn add-to-superstructure
  "Return a superstructure like `s` with object `o` added. If `o` is a collection,
  return a superstructure like `s` with each element of `o` added. If only one
  argument is supplied it will be assumed to represent `o` and a new
  superstructure will be returned.

  It is an error (and an exception may be thrown) if

  1. `s` is not a map;
  2. `o` is not a map, or a sequence of maps."
  ([o]
   (add-to-superstructure {} o))
  ([s o]
  (cond
    (map? o) (let [o' (if (:id o) o (assoc o :id (keyword (gensym "obj"))))]
               (index-vertices (assoc s (:id o') o') o'))
    (coll? o) (reduce u/deep-merge (map #(add-to-superstructure s %) o))
    (nil? o) o
    :else
    (throw (IllegalArgumentException. (str "Don't know how to index " (or (type o) "nil")))))))

