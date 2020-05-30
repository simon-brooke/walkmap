(ns walkmap.path
  "Essentially the specification for things we shall consider to be path.
  **Note that** for these purposes `path` means any continuous linear
  feature, where such features specifically include watercourses."
  (:require [clojure.string :as s]
            [walkmap.edge :as e]
            [walkmap.polygon :refer [polygon?]]
            [walkmap.utils :refer [kind-type]]
            [walkmap.vertex :refer [vertex?]]))

(defn path?
  "True if `o` satisfies the conditions for a path. A path shall be a map
  having the key `:vertices`, whose value shall be a sequence of vertices as
  defined in `walkmap.vertex`."
  [o]
  (let
    [v (:vertices o)]
    (and
      (seq? v)
      (> (count v) 2)
      (every? vertex? v)
      (:walkmap.id/id o)
      (or (nil? (:kind o)) (= (:kind o) :path)))))

(defn path
  "Return a path constructed from these `vertices`."
  [& vertices]
  (when-not (every? vertex? vertices)
    (throw (IllegalArgumentException.
             (str
               "Each item on path must be a vertex: "
               (s/join " " (map kind-type (remove vertex? vertices)))))))
  {:vertices vertices :walkmap.id/id (keyword (gensym "path")) :kind :path})

(defn polygon->path
  "If `o` is a polygon, return an equivalent path. What's different about
  a path is that in polygons there is an implicit edge between the first
  vertex and the last. In paths, there isn't, so we need to add that
  edge explicitly.

  If `o` is not a polygon, will throw an exception."
  [o]
  (when-not (polygon? o)
    (throw (IllegalArgumentException. (str "Not a polygon: " (kind-type o)))))
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
    (seq? o)
    (when
      (and
        (vertex? (first o))
        (vertex? (first (rest o))))
      (cons
        ;; TODO: think about: when constructing an edge from a path, should the
        ;; constructed edge be tagged with the tags of the path?
        (e/edge (first o) (rest o))
        (path->edges (rest o))))
    (path? o)
    (path->edges (:vertices o))
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
  (if
    (path? path)
    (reduce + (map e/length (path->edges path)))
    (throw (IllegalArgumentException. "Not a path!"))))
