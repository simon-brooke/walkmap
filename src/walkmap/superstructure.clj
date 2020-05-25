(ns walkmap.superstructure
  "single indexing structure for walkmap objects"
  (:require [walkmap.path :as p]
            [walkmap.polygon :as q]
            [walkmap.stl :as s]
            [walkmap.utils :as u]
            [walkmap.vertex :as v]))

(defn index-vertex
  "Return a superstructure like `s` in which object `o` is indexed by vertex
  `v`. It is an error (and an exception may be thrown) if

  1. `s` is not a map;
  2. `o` is not a map;
  3. `o` does not have a value for the key `:id`;
  4. `v` is not a vertex."
  ;; two copies of the same vertex are not identical enough to one another
  ;; to be used as keys in a map. So our vertices need to have ids, and we need
  ;; to key the vertex-index by vertex ids.
  ;; TODO: BUT WE CANNOT USE GENSYMED ids, because two vertices with the same
  ;; vertices must have the same id!
  [s o v]
  (if-not (v/vertex? o)
    (if (:id o)
      (if (v/vertex? v)
        (let [vi (or (:vertex-index s) {})
              current (or (vi (:id v)) {})]
          ;; deep-merge doesn't merge sets, only maps; so at this
          ;; stage we need to build a map.
          (assoc vi (:id v) (assoc current (:id o) (:id v))))
        (throw (Exception. "Not a vertex: " v)))
      (throw (Exception. (subs (str "No `:id` value: " o) 0 80))))
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
    (throw (Exception. (str "Don't know how to index " (or (type o) "nil")))))))

(:vertex-index (add-to-superstructure (:facets (s/decode-binary-stl "resources/isle_of_man.stl"))))
(s/decode-binary-stl "resources/isle_of_man.stl")
