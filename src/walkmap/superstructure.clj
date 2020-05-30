(ns walkmap.superstructure
  "single indexing structure for walkmap objects"
  (:require [clojure.walk :refer [postwalk]]
            [taoensso.timbre :as l]
            [walkmap.path :as p]
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

(def vertex-index ::vertex-index)

(defn vertices
  "If `o` is an object with vertices, return those vertices, else nil."
  [o]
  (cond
    (v/vertex? o) (list o)
    (q/polygon? o) (:vertices o)
    (p/path? o) (:vertices o)))

(defn index-vertex
  "Return a superstructure like `s` in which object `o` is indexed by vertex
  `v`. It is an error (and an exception may be thrown) if

  1. `s` is not a map;
  2. `o` is not a map;
  3. `o` does not have a value for the key `:walkmap.id/id`;
  4. `v` is not a vertex."
  [s o v]
  (if-not (v/vertex? o)
    (if (:walkmap.id/id o)
      (if (v/vertex? v)
        (let [vi (or (::vertex-index s) {})
              current (or (vi (:walkmap.id/id v)) {})]
          ;; deep-merge doesn't merge sets, only maps; so at this
          ;; stage we need to build a map.
          (assoc vi (:walkmap.id/id v) (assoc current (:walkmap.id/id o) (:walkmap.id/id v))))
        (throw (IllegalArgumentException. "Not a vertex: " v)))
      (throw (IllegalArgumentException. (u/truncate (str "No `:walkmap.id/id` value: " o) 80))))
    ;; it shouldn't actually be an error to try to index a vertex, but it
    ;; also isn't useful to do so, so I'd be inclined to ignore it.
    (::vertex-index s)))

(defn index-vertices
  "Return a superstructure like `s` in which object `o` is indexed by its
  vertices. It is an error (and an exception may be thrown) if

  1. `s` is not a map;
  2. `o` is not a map;
  3. `o` does not have a value for the key `:walkmap.id/id`."
  [s o]
  (u/deep-merge
    s
    {::vertex-index
     (reduce
       u/deep-merge
       {}
       (map
         #(index-vertex s o %)
         (:vertices o)))}))

;; (declare in-retrieve)

;; (defn in-retrieve-map
;;   "Internal to `in-retrieve`, q.v. Handle the case where `x` is a map.
;;   Separated out for debugging/unit testing purposes. Use at your own peril."
;;   [x s]
;;   (let [v (reduce
;;             (fn [m k]
;;               (assoc m k (in-retrieve (x k) s)))
;;             {}
;;             (keys (dissoc x :walkmap.id/id)))
;;         id (:walkmap.id/id x)]
;;     (if id
;;       (assoc
;;         v
;;         :walkmap.id/id
;;         (:walkmap.id/id x))))
;;   )

(defn in-retrieve
  "Internal guts of `retrieve`, q.v. `x` can be anything; `s` must be a
  walkmap superstructure. TODO: recursive, quite likely to blow the fragile
  Clojure stack. Probably better to do this with `walk`, but I don't yet
  understand that."
  [x s]
  (cond
    ;; if it's a keyword identifying something in s, retrieve that something.
    (keyword? x) (if (s x)
                   (in-retrieve (s x) s)
                   x)
    ;; if it's a map, for every key which is not `:walkmap.id/id`, recurse.
    (map? x) (let [v (reduce
                       (fn [m k]
                         (assoc m k (in-retrieve (x k) s)))
                       {}
                       (keys (dissoc x :walkmap.id/id)))
                   id (:walkmap.id/id x)]
               ;; if it has an id, bind it to that id in the returned value.
               (if id
                 (assoc
                   v
                   :walkmap.id/id
                   (:walkmap.id/id x))
                 v))
    (coll? x) (map #(in-retrieve % s) x)
    :else x))

(defn retrieve
  "Retrieve the canonical representation of the object with this `id` from the
  superstructure `s`."
  [id s]
  (in-retrieve (id s) s))

(defn in-store-find-objects
  "Return an id -> object map of every object within `o`. Internal to
  `in-store`, q.v. Use at your own peril."
  ([o]
   (in-store-find-objects o {}))
  ([o s]
   (l/debug "Finding objects in:" o)
   (cond
     (map? o) (if (:walkmap.id/id o)
                (assoc
                  (in-store-find-objects (vals o) s)
                  (:walkmap.id/id o)
                  o)
                (in-store-find-objects (vals o) s))
     (coll? o) (reduce merge s (map #(in-store-find-objects % s) o))
     :else s)))

(defn in-store-replace-with-keys
  "Return a copy of `o` in which each reified walkmap object within `o` has
  been replaced with the `:walkmap.id/id` of that object. Internal to
  `in-store`, q.v. Use at your own peril."
  [o]
  (assoc
    (postwalk #(or (:walkmap.id/id %) %) (dissoc o :walkmap.id/id))
    :walkmap.id/id
    (:walkmap.id/id o)))

;; (in-store-replace-with-keys (p/path (v/vertex 0 0 0) (v/vertex 0 1 2) (v/vertex 3 3 3)))
;; (in-store-find-objects (p/path (v/vertex 0 0 0) (v/vertex 0 1 2) (v/vertex 3 3 3)))

(defn store
  "Return a superstructure like `s` with object `o` added. If only one
  argument is supplied it will be assumed to represent `o` and a new
  superstructure will be returned.

  It is an error (and an exception may be thrown) if

  1. `s` is not a map;
  2. `o` is not a recognisable walkmap object"
  ([o]
   (store o {}))
  ([o s]
   (when-not (:walkmap.id/id o)
     (throw
       (IllegalArgumentException.
         (str "Not a walkmap object: no value for `:walkmap.id/id`: "
              (u/kind-type o)))))
   (when-not (map? s)
     (throw
       (IllegalArgumentException.
         (str "Superstructure must be a map: " (u/kind-type s)))))
   (assoc
     (u/deep-merge s (in-store-find-objects o))
     (:walkmap.id/id o)
     (in-store-replace-with-keys o)
     ::vertex-index
     (u/deep-merge
       (index-vertices s o)
       (::vertex-index s)))))
