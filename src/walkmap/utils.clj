(ns walkmap.utils
  "Miscellaneous utility functions."
  (:require [clojure.edn :as edn :only [read]]
            [clojure.java.io :as io]
            [clojure.math.numeric-tower :as m]
            [clojure.string :as s]))

(defn deep-merge
  "Recursively merges maps. If vals are not maps, the last value wins."
  ;; TODO: not my implementation, not sure I entirely trust it.
  ;; TODO TODO: if we are to successfully merge walkmap objects, we must
  ;; return, on each object, the union of its tags if any.
  [& vals]
  (if (every? map? vals)
    (apply merge-with deep-merge vals)
    (last vals)))

(defn truncate
  "If string `s` is more than `n` characters long, return the first `n`
  characters; otherwise, return `s`."
  [s n]
  (if (and (string? s) (number? n) (> (count s) n))
    (subs s 0 n)
    s))

(defn kind-type
  "Identify the type of an `object`, e.g. for logging. If it has a `:kind` key,
  it's one of ours, and that's what we want. Otherwise, we want its type; but
  the type of `nil` is `nil`, which doesn't get printed when assembling error
  ,essages, so return \"nil\"."
  [object]
  (or (:kind object) (type object) "nil"))

(defn =ish
  "True if numbers `n1`, `n2` are roughly equal; that is to say, equal to
  within `tolerance` (defaults to one part in one hundred thousand)."
  ([n1 n2]
   (if (and (number? n1) (number? n2))
     (let [m (m/abs (min n1 n2))
           t (if (zero? m) 0.00001 (* 0.00001 m))]
       (=ish n1 n2 t))
     (= n1 n2)))
  ([n1 n2 tolerance]
   (if (and (number? n1) (number? n2))
     (< (m/abs (- n1 n2)) tolerance)
     (= n1 n2))))

(defmacro check-kind-type
  "If `object` is not of kind-type `expected`, throws an
  IllegalArgumentException with an appropriate message; otherwise, returns
  `object`. If `checkfn` is supplied, it should be a function which tests
  whether the object is of the expected kind-type.

  Macro, so that the exception is thrown from the calling function."
  ([object expected]
   `(if-not (= (kind-type ~object) ~expected)
      (throw
        (IllegalArgumentException.
          (s/join
            " "
            ["Expected" ~expected "but found" (kind-type ~object)])))
      ~object))
  ([object checkfn expected]
   `(if-not (~checkfn ~object)
      (throw
        (IllegalArgumentException.
          (s/join
            " "
            ["Expected" ~expected "but found" (kind-type ~object)])))
      ~object)))

(defmacro check-kind-type-seq
  "If some item on sequence `s` is not of the `expected` kind-type, throws an
  IllegalArgumentException with an appropriate message; otherwise, returns
  `object`. If `checkfn` is supplied, it should be a function which tests
  whether the object is of the expected kind-type.

  Macro, so that the exception is thrown from the calling function."
  ([s expected]
  `(if-not (every? #(= (kind-type %) ~expected) ~s)
     (throw
       (IllegalArgumentException.
         (s/join
           " "
           ["Expected sequence of"
            ~expected
            "but found ("
            (s/join ", " (remove #(= ~expected %) (map kind-type ~s)))
            ")"])))
     ~s))
  ([s checkfn expected]
  `(if-not (every? #(~checkfn %) ~s)
     (throw
       (IllegalArgumentException.
         (s/join
           " "
           ["Expected sequence of"
            ~expected
            "but found ("
            (s/join ", " (remove #(= ~expected %) (map kind-type ~s)))
            ")"])))
     ~s)))

(defn load-edn
  "Load edn from an io/reader source (filename or io/resource)."
  [source]
  (try
    (with-open [r (io/reader source)]
      (edn/read (java.io.PushbackReader. r)))
    (catch java.io.IOException e
      (printf "Couldn't open '%s': %s\n" source (.getMessage e)))
    (catch RuntimeException e
      (printf "Error parsing edn file '%s': %s\n" source (.getMessage e)))))
