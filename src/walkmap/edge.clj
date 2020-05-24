(ns walkmap.edge
  "Essentially the specification for things we shall consider to be an edge.
  An edge is a line segment having just a start and an end, with no intervening
  nodes."
  (:require [walkmap.path :refer [path? polygon->path]]
            [walkmap.polygon :refer [polygon?]]
            [walkmap.vertex :refer [vertex?]]))

(defn edge?
  "True if `o` satisfies the conditions for a path. A path shall be a map
  having the keys `:start` and `:end`, such that the values of each of those
  keys shall be a vertex."
  [o]
  (and
    (map? o)
    (vertex? (:start o))
    (vertex? (:end o))))

(defn path->edges
  "if `o` is a path, a polygon, or a sequence of vertices, return a sequence of
  edges representing that path, polygon or sequence."
  [o]
  (cond
    (seq? o)
    (when
      (and
        (vertex? (first o))
        (vertex? (first (rest o))))
      (cons
        {:start (first o)
         :end (first (rest o))}
        (path->edges (rest o))))
    (path? o)
    (path->edges (:nodes o))
    (polygon? o)
    (path->edges (polygon->path o))))

