(ns walkmap.vertex
  "Essentially the specification for things we shall consider to be vertices.")

(defn vertex-key
  "Making sure we get the same key everytime we key a vertex with the same
  coordinates. `o` must have numeric values for `:x`, `:y`, and optionally
  `:z`."
  [o]
  (cond
    (and (:x o) (:y o) (:z o)) (keyword (str "vert{" (:x o) "|" (:y o) "|" (:z o) "}"))
    (and (:x o) (:y o)) (keyword (str "vert{" (:x o) "|" (:y o) "}"))
    :else (throw (Exception. "Not a vertex."))))

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
    (:id o)
    (number? (:x o))
    (number? (:y o))
    (or (nil? (:z o)) (number? (:z o)))
    (or (nil? (:kind o)) (= (:kind o) :vertex))))

(defn make-vertex
  "Make a vertex with this `x`, `y` and (if provided) `z` values. Returns a map
  with those values, plus a unique `:id` value, and `:kind` set to `:vertex`.
  It's not necessary to use this function to create a vertex, but the `:id`
  must be present and must be unique."
  ([x y]
   (let [v {:x x :y y :kind :vertex}]
     (assoc v :id (vertex-key v))))
  ([x y z]
   (assoc (make-vertex x y) :z z)))

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
         (not (vertex? o)) (throw (Exception. "Not a vertex!"))
         (:z o) o
         :else (assoc o :z dflt))))))

(def ensure2d
  "If `o` is a vertex, set its `:z` value to zero; else throw an exception."
  (memoize
    (fn [o]
      (if
        (vertex? o)
        (assoc o :z 0.0)
        (throw (Exception. "Not a vertex!"))))))
