(ns walkmap.stl
  "Utility functions dealing with stereolithography (STL) files. Not a stable API yet!"
  (:require [clojure.java.io :as io :refer [file output-stream input-stream]]
            [clojure.string :as s]
            [me.raynes.fs :as fs]
            [org.clojars.smee.binary.core :as b]
            [taoensso.timbre :as l :refer [info error spy]]
            [walkmap.edge :as e]
            [walkmap.polygon :refer [polygon?]]
            [walkmap.tag :refer [tag]]
            [walkmap.utils :as u]
            [walkmap.vertex :as v])
  (:import org.clojars.smee.binary.core.BinaryIO
           java.io.DataInput))

(defn stl?
  "True if `o` is recogniseable as an STL structure. An STL structure must
  have a key `:facets`, whose value must be a sequence of polygons; and
  may have a key `:header` whose value should be a string, and/or a key
  `:count`, whose value should be a positive integer.

  If `verify-count?` is passed and is not `false`, verify that the value of
  the `:count` header is equal to the number of facets."
  ([o]
   (stl? o false))
  ([o verify-count?]
   (and
     (map? o)
     (:facets o)
     (every? polygon? (:facets o))
     (if (:header o) (string? (:header o)) true)
     (if (:count o) (integer? (:count o)) true)
     (or (nil? (:kind o)) (= (:kind o) :stl))
     (if verify-count? (= (:count o) (count (:facets o))) true))))

(def vect
  "A codec for vectors within a binary STL file."
  (b/ordered-map
    :x :float-le
    :y :float-le
    :z :float-le))

(def facet
  "A codec for a facet (triangle) within a binary STL file."
  (b/ordered-map
    :normal vect
    :vertices [vect vect vect]
    :abc :ushort-le))

(def binary-stl
  "A codec for binary STL files"
  (b/ordered-map
   :header (b/string "ISO-8859-1" :length 80) ;; for the time being we neither know nor care what's in this.
   :count :uint-le
   :facets (b/repeated facet)))

(defn centre
  "Return a canonicalised `facet` (i.e. a triangular polygon) with an added
  key `:centre` whose value represents the centre of this facet in 3
  dimensions. This only works for triangles, so is here not in
  `walkmap.polygon`. It is an error (although no exception is currently
  thrown) if the object past is not a triangular polygon."
  [facet]
  (let [vs (:vertices facet)
        v1 (first vs)
        opposite (e/edge (nth vs 1) (nth vs 2))
        oc (e/centre opposite)]
    (assoc
      facet
      :centre
      (v/vertex
        (+ (:x v1) (* (- (:x oc) (:x v1)) 2/3))
        (+ (:y v1) (* (- (:y oc) (:y v1)) 2/3))
        (+ (:z v1) (* (- (:z oc) (:z v1)) 2/3))))))

(defn canonicalise
  "Objects read in from STL won't have all the keys/values we need them to have.
  `o` may be a map (representing a facet or a vertex), or a sequence of such maps;
  if it isn't recognised it is at present just returned unchanged. `map-kind`, if
  passed, must be a keyword indicating the value represented by the `z` axis
  (defaults to `:height`). It is an error, and an exception will be thrown, if
  `map-kind` is not a keyword."
  ([o] (canonicalise o :height))
  ([o map-kind]
   (when-not
     (keyword? map-kind)
     (throw (IllegalArgumentException.
              (u/truncate (str "Must be a keyword: " (or map-kind "nil")) 80))))
   (cond
     (and (coll? o) (not (map? o))) (map #(canonicalise % map-kind) o)
     ;; if it has :facets it's an STL structure, but it doesn't yet conform to `stl?`
     (:facets o) (assoc o
                   :kind :stl
                   :id (or (:id o) (keyword (gensym "stl")))
                   :facets (canonicalise (:facets o) map-kind))
     ;; if it has :vertices it's a polygon, but it doesn't yet conform to `polygon?`
     (:vertices o) (centre
                     (tag
                       (assoc o
                         :id (or (:id o) (keyword (gensym "poly")))
                         :kind :polygon
                         :vertices (canonicalise (:vertices o) map-kind))
                       :facet map-kind))
     ;; if it has a value for :x it's a vertex, but it doesn't yet conform to `vertex?`
     (:x o) (v/canonicalise o)
     ;; shouldn't happen
     :else o)))

(defn decode-binary-stl
  "Parse a binary STL file from this `filename` and return an STL structure
  representing its contents. `map-kind`, if passed, must be a keyword
  indicating the value represented by the `z` axis (defaults to `:height`).
  It is an error, and an exception will be thrown, if `map-kind` is not a
  keyword.

  **NOTE** that we've no way of verifying that the input file is binary STL
  data, if it is not this will run but will return garbage."
  ([filename]
   (decode-binary-stl filename :height))
  ([filename map-kind]
   (when-not
     (keyword? map-kind)
     (throw (IllegalArgumentException.
              (u/truncate (str "Must be a keyword: " (or map-kind "nil")) 80))))
   (let [in (io/input-stream filename)]
     (canonicalise (b/decode binary-stl in) map-kind))))

(defn- vect->str [prefix v]
  (str prefix " " (:x v) " " (:y v) " " (:z v) "\n"))

(defn- facet2str [tri]
  (str
    (vect->str "facet normal" (:normal tri))
    "outer loop\n"
    (apply str
           (map
             #(vect->str "vertex" %)
             (:vertices tri)))
    "endloop\nendfacet\n"))

(defn stl->ascii
  "Return as a string an ASCII rendering of the `stl` structure."
  ([stl]
   (stl->ascii stl "unknown"))
  ([stl solidname]
   (str
     "solid "
     solidname
     (s/trim (:header stl))
     "\n"
     (apply
       str
       (map
         facet2str
         (:facets stl)))
     "endsolid "
     solidname
     "\n")))

(defn write-ascii-stl
  "Write an `stl` structure as read by `decode-binary-stl` to this
  `filename` as ASCII encoded STL."
  ([filename stl]
   (let [b (fs/base-name filename true)]
     (write-ascii-stl
       filename stl
       (subs b 0 (or (s/index-of b ".") (count b))))))
  ([filename stl solidname]
   (l/debug "Solid name is " solidname)
   (spit
     filename
     (stl->ascii stl solidname))))

(defn binary-stl-to-ascii
  "Convert the binary STL file indicated by `in-filename`, and write it to
  `out-filename`, if specified; otherwise, to a file with the same basename
  as `in-filename` but the extension `.ascii.stl`."
  ([in-filename]
   (let [[_ ext] (fs/split-ext in-filename)]
     (binary-stl-to-ascii
       in-filename
       (str
         (subs
           in-filename
           0
           (or
             (s/last-index-of in-filename ".")
             (count in-filename)))
         ".ascii"
         ext))))
  ([in-filename out-filename]
   (write-ascii-stl out-filename (decode-binary-stl in-filename))))
