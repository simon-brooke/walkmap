(ns walkmap.core
  (:require [clojure.java.io :as io :refer [file output-stream input-stream]]
            [clojure.string :as s]
            [dali.io :as svg]
            [me.raynes.fs :as fs]
            [org.clojars.smee.binary.core :as b]
            [taoensso.timbre :as l :refer [info error spy]])
  (:import org.clojars.smee.binary.core.BinaryIO
           java.io.DataInput))

(def vect
  (b/ordered-map
    :x :float-le
    :y :float-le
    :z :float-le))

(def facet
  (b/ordered-map
    :normal vect
    :vertices [vect vect vect]
    :abc :ushort-le))

(def binary-stl
  (b/ordered-map
   :header (b/string "ISO-8859-1" :length 80) ;; for the time being we neither know nor care what's in this.
   :count :uint-le
   :facets (b/repeated facet)))

(defn decode-binary-stl
  "Parse a binary STL file from this `filename`."
  [filename]
  (let [in (io/input-stream filename)]
    (b/decode binary-stl in)))

(defn- vect2str [prefix v]
  (str prefix " " (:x v) " " (:y v) " " (:z v) "\n"))

(defn- facet2str [tri]
  (str
      (vect2str "facet normal" (:normal tri))
      "outer loop\n"
    (apply str
      (map
        #(vect2str "vertex" %)
        (:vertices tri)))
    "endloop\nendfacet\n"))

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
       "\n"))))

(defn binary-stl-to-ascii
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

(def stl (decode-binary-stl "resources/small_hill.stl"))

(defn facet-to-svg-poly
  [facet]
  (vec
    (cons
      :polygon
      (map #(vec (list (:x %) (:y %))) (:vertices facet)))))

(facet-to-svg-poly (first (:facets stl)))

(defn stl-to-svg
  "Convert this in-memory `stl` structure, as read by `decode-binary-stl`, into
  an in-memory (Dali) SVG structure, and return it."
  [stl]
  (let [minx (reduce
               min
               (map
                 #(reduce min (map :x (:vertices %)))
                 (:facets stl)))
        maxx (reduce
               max
               (map
                 #(reduce max (map :x (:vertices %)))
                 (:facets stl)))
        miny (reduce
               min
               (map
                 #(reduce min (map :y (:vertices %)))
                 (:facets stl)))
        maxy (reduce
               max
               (map
                 #(reduce max (map :y (:vertices %)))
                 (:facets stl)))]
    [:dali/page
     {:width (- maxx minx)
      :height (- maxy miny)
      :viewBox (s/join " " (map str [minx miny maxx maxy]))}
     (vec
       (cons
         :g
         (map
           facet-to-svg-poly
           (:facets stl))))]))

(defn binary-stl-file-to-svg
  "Given only an `in-filename`, parse the indicated file, expected to be
  binary STL, and return an equivalent SVG structure. Given both `in-filename`
  and `out-filename`, as side-effect write the SVG to the indicated output file."
  ([in-filename]
   (stl-to-svg (decode-binary-stl in-filename)))
  ([in-filename out-filename]
   (let [s (binary-stl-file-to-svg in-filename)]
     (svg/render-svg
       s
       out-filename)
     s)))

(map facet-to-svg-poly (:facets stl))
(svg/render-svg (stl-to-svg stl) "frobox.svg")
