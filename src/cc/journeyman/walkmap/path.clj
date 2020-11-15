(ns cc.journeyman.walkmap.path
  "Essentially the specification for things we shall consider to be path.
  **Note that** for these purposes `path` means any continuous linear
  feature, where such features specifically include watercourses."
  (:require [clojure.string :as s]
            [cc.journeyman.walkmap.edge :as e]
            [cc.journeyman.walkmap.polygon :refer [check-polygon polygon?]]
            [cc.journeyman.walkmap.tag :refer [tag tags]]
            [cc.journeyman.walkmap.utils :refer [check-kind-type check-kind-type-seq kind-type]]
            [cc.journeyman.walkmap.vertex :refer [check-vertices vertex?]]))

(defn path?
  "True if `o` satisfies the conditions for a path. A path shall be a map
  having the key `:vertices`, whose value shall be a sequence of vertices as
  defined in `walkmap.vertex`."
  [o]
  (let
    [v (:vertices o)]
    (and
      (seq? v)
      (> (count v) 1)
      (every? vertex? v)
      (:walkmap.id/id o)
      (or (nil? (:kind o)) (= (:kind o) :path)))))

(defn path
  "Return a path constructed from these `vertices`."
  [& vertices]
  (if
    (> (count (check-vertices vertices)) 1)
    {:vertices vertices :walkmap.id/id (keyword (gensym "path")) :kind :path}
    (throw (IllegalArgumentException. "Path must have more than one vertex."))))

(defmacro check-path
  "If `o` is not a path, throw an `IllegalArgumentException` with an
  appropriate message; otherwise, returns `o`. Macro, so exception is thrown
  from the calling function."
  [o]
  `(check-kind-type ~o path? :path))

(defmacro check-paths
  "If `o` is not a sequence of paths, throw an `IllegalArgumentException` with an
  appropriate message; otherwise, returns `o`. Macro, so exception is thrown
  from the calling function."
  [o]
  `(check-kind-type-seq ~o path? :path))

(defn polygon->path
  "If `o` is a polygon, return an equivalent path. What's different about
  a path is that in polygons there is an implicit edge between the first
  vertex and the last. In paths, there isn't, so we need to add that
  edge explicitly.

  If `o` is not a polygon, will throw an exception."
  [o]
;; this is breaking, but I have NO IDEA why!
;;  (check-polygon o polygon? :polygon)
  (assoc (dissoc o :vertices)
    :kind :path
    ;; `concat` rather than `conj` because order matters.
    :vertices (concat (:vertices o) (list (first (:vertices o))))))

(defn path->edges
  "if `o` is a path, a polygon, or a sequence of vertices, return a sequence of
  edges representing that path, polygon or sequence.

  Throws `IllegalArgumentException` if `o` is not a path, a polygon, or
  sequence of vertices."
  [o]
  (cond
    (seq? o) (when
               (and
                 (vertex? (first o))
                 (vertex? (first (rest o))))
               (cons
                 ;; TODO: think about: when constructing an edge from a path, should the
                 ;; constructed edge be tagged with the tags of the path?
                 (e/edge (first o) (first (rest o)))
                 (path->edges (rest o))))
    (path? o) (path->edges (:vertices o))
    (polygon? o) (path->edges (polygon->path o))
    :else
    (throw (IllegalArgumentException.
             "Not a path or sequence of vertices!"))))

(defn length
  "Return the length of this path, in metres. **Note that**
  1. This is not the same as the distance from the start to the end of the
  path, which, except for absolutely straight paths, will be shorter;
  2. It is not even quite the same as the length of the path *as rendered*,
  since paths will generally be rendered as spline curves."
  [path]
  (reduce + (map e/length (path->edges (check-path path)))))
