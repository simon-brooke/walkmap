(ns walkmap.vertex
  "Essentially the specification for things we shall consider to be vertices.

  Note that there's no `distance` function here; to find the distance between
  two vertices, create an edge from them and use `walkmap.edge/length`."
  (:require [clojure.math.numeric-tower :as m]
            [clojure.string :as s]
            [taoensso.timbre :as l]
            [walkmap.geometry :refer [=ish]]
            [walkmap.utils :refer [kind-type truncate]]))

(defn vertex-key
  "Making sure we get the same key everytime we key a vertex with the same
  coordinates. `o` must have numeric values for `:x`, `:y`, and optionally
  `:z`; it is an error and an exception will be thrown if `o` does not
  conform to this specification.

  **Note:** these keys can be quite long. No apology is made: it is required
  that the same key can *never* refer to two different locations in space."
  [o]
  (keyword
    (s/replace
      (cond
        (and (:x o) (:y o) (:z o))
        (str "vert_" (:x o) "_" (:y o) "_" (:z o))
        (and (:x o) (:y o))
        (str "vert_" (:x o) "_" (:y o))
        :else
        (throw (IllegalArgumentException.
                 (truncate (str "Not a vertex: " (or o "nil")) 80))))
      "."
      "-")))

(defn vertex?
  "True if `o` satisfies the conditions for a vertex. That is, essentially,
  that it must rerpresent a two- or three- dimensional vector. A vertex is
  shall be a map having at least the keys `:x` and `:y`, where the value of
  those keys is a number. If the key `:z` is also present, its value must also
  be a number.

  The name  `vector?` was not used as that would clash with a function of that
  name in `clojure.core` whose semantics are entirely different."
  [o]
  (and
    (map? o)
    (:walkmap.id/id o)
    (number? (:x o))
    (number? (:y o))
    (or (nil? (:z o)) (number? (:z o)))
    (or (nil? (:kind o)) (= (:kind o) :vertex))))

(defn vertex=
  "True if vertices `v1`, `v2` represent the same vertex."
  [v1 v2]
  (every?
    #(=ish (% v1) (% v2))
    [:x :y :z]))

(defn vertex*
  "Return a vertex like `v1`, but with each of its coordinates multiplied
  by the equivalent vertex in `v2`."
  [v1 v2]
  (if
    (and (vertex? v1) (vertex? v2))
    (let [f (fn [v1 v2 coord]
              (* (or (coord v1) 0)
                 ;; one here is deliberate!
                 (or (coord v2) 1)))]
      (assoc v1 :x (f v1 v2 :x)
        :y (f v1 v2 :y)
        :z (f v1 v2 :z)))
    (do (l/warn
          (s/join
            " "
            ["in `vertex-multiply`, both must be vectors. v1:" v1 "v2:" v2]))
      v1)))

(defn vertex
  "Make a vertex with this `x`, `y` and (if provided) `z` values. Returns a map
  with those values, plus a unique `:walkmap.id/id` value, and `:kind` set to `:vertex`.
  It's not necessary to use this function to create a vertex, but the `:walkmap.id/id`
  must be present and must be unique."
  ([x y]
   (let [v {:x x :y y :kind :vertex}]
     (assoc v :walkmap.id/id (vertex-key v))))
  ([x y z]
   (let [v (assoc (vertex x y) :z z)]
     (assoc v :walkmap.id/id (vertex-key v)))))

(defn canonicalise
  "If `o` is a map with numeric values for `:x`, `:y` and optionally `:z`,
  upgrade it to something we will recognise as a vertex."
  [o]
  (if
    (and
      (map? o)
      (number? (:x o))
      (number? (:y o))
      (or (nil? (:z o)) (number? (:z o))))
    (assoc o :kind :vertex :walkmap.id/id (vertex-key o))
    (throw
      (IllegalArgumentException.
        (truncate
          (str "Not a proto-vertex: must have numeric `:x` and `:y`: "
               (or o "nil"))
          80)))))

(def ensure3d
  "Given a vertex `o`, if `o` has a `:z` value, just return `o`; otherwise
  return a vertex like `o` but having thie `dflt` value as the value of its
  `:z` key, or zero as the value of its `:z` key if `dflt` is not specified.

  If `o` is not a vertex, throws an exception."
  (memoize
    (fn
      ([o]
       (ensure3d o 0.0))
      ([o dflt]
       (cond
         (not (vertex? o)) (throw
                             (IllegalArgumentException.
                               (truncate (str "Not a vertex: " (or o "nil")) 80)))
         (:z o) o
         :else (assoc o :z dflt))))))

(def ensure2d
  "If `o` is a vertex, set its `:z` value to zero; else throw an exception."
  (memoize
    (fn [o]
      (if
        (vertex? o)
        (assoc o :z 0.0)
        (throw
          (IllegalArgumentException.
            (truncate (str "Not a vertex: " (or o "nil")) 80)))))))

(defn within-box?
  "True if `target` is within the box defined by `minv` and `maxv`. All
  arguments must be vertices; additionally, both `minv` and `maxv` must
  have `:z` coordinates."
  [target minv maxv]
  (when-not (and (vertex? target) (vertex? minv) (vertex? maxv))
    (throw (IllegalArgumentException.
             (s/join " " ["Arguments to `within-box?` must be vertices:"
                          (map kind-type [target minv maxv])]))))
  (every?
    (map
      #(< (% minv) (or (% target) 0) (% maxv))
      [:x :y :z])))
