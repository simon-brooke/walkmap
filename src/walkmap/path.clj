(ns walkmap.path
  "Essentially the specification for things we shall consider to be path."
  (:require [walkmap.polygon :refer [polygon?]]
            [walkmap.vertex :refer [vertex?]]))

(defn path?
  "True if `o` satisfies the conditions for a path. A path shall be a map
  having the key `:nodes`, whose value shall be a sequence of vertices as
  defined in `walkmap.vertex`."
  [o]
  (let
    [v (:nodes o)]
    (and
      (seq? v)
      (> (count v) 2)
      (every? vertex? v)
      (or (nil? (:kind o)) (= (:kind o) :path)))))

(defn make-path
  [nodes]
  (if
    (every? vertex? nodes)
    {:nodes nodes :id (keyword (gensym "path")) :kind :path}
    (throw (Exception. "Each item on path must be a vertex."))))

(defn polygon->path
  "If `o` is a polygon, return an equivalent path. What's different about
  a path is that in polygons there is an implicit edge between the first
  vertex and the last. In paths, there isn't, so we need to add that
  edge explicitly.

  If `o` is not a polygon, will throw an exception."
  [o]
  (if
    (polygon? o)
    (assoc (dissoc o :vertices) :kind :path :nodes (concat (:vertices o) (list (first (:vertices o)))))
    (throw (Exception. "Not a polygon!"))))

