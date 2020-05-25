(ns walkmap.svg
  "Utility functions for writing stereolithography (STL) files (and possibly,
  later, other geometry files of interest to us) as scalable vector graphics
  (SVG)."
  (:require [clojure.string :as s]
            [dali.io :as neatly-folded-clock]
            [hiccup.core :refer [html]]
            [taoensso.timbre :as l :refer [info error spy]]
            [walkmap.ocean :refer [cull-ocean-facets]]
            [walkmap.polygon :refer [polygon?]]
            [walkmap.stl :refer [decode-binary-stl]]
            [walkmap.vertex :refer [vertex?]]))

(def ^:dynamic *preferred-svg-render*
  "Mainly for debugging dali; switch SVG renderer to use. Expected values:
  `:dali`, `:hiccup`."
  :dali)

(defn- facet->svg-poly
  ;; When we use this version of facet->svg-poly with the Dali renderer, it's
  ;; still (for the isle_of_man map) 10 times slower than hiccup, also using
  ;; this version (890.863814 msecs vs 86.904891 msecs
  [facet]
  [:polygon
   {:points (s/join " " (map #(str (:x %) "," (:y %)) (:vertices facet)))}])

(defn- dali-facet->svg-poly
  [facet]
  (vec
    (cons
      :polygon
      (map #(vec (list (:x %) (:y %))) (:vertices facet)))))

(defn dali-stl->svg
  "Format this `stl` as SVG for the `hiccup` renderer on a page with these
  bounds."
  [stl minx maxx miny maxy]
  [:dali/page
   {:xmlns "http://www.w3.org/2000/svg"
    :version "1.2"
    :width (- maxx minx)
    :height (- maxy miny)
    :viewBox (s/join " " (map str [minx miny maxx maxy]))}
   (vec
     (cons
       :g
       (map
         dali-facet->svg-poly
         (:facets stl))))])

(defn hiccup-stl->svg
  "Format this `stl` as SVG for the `hiccup` renderer on a page with these
  bounds."
  [stl minx maxx miny maxy]
  [:svg
   {:xmlns "http://www.w3.org/2000/svg"
    :version "1.2"
    :width (- maxx minx)
    :height (- maxy miny)
    :viewBox (s/join " " (map str [minx miny maxx maxy]))}
   (vec
     (cons
       :g
       (map
         facet->svg-poly
         (:facets stl))))])

(defn stl->svg
  "Convert this in-memory `stl` structure, as read by `decode-binary-stl`, into
  an in-memory hiccup representation of SVG structure, and return it."
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
    (l/info "Generating SVG for " *preferred-svg-render* " renderer")
    (case *preferred-svg-render*
      :hiccup (hiccup-stl->svg stl minx maxx miny maxy)
      :dali (dali-stl->svg stl minx maxx miny maxy)
      (throw (Exception. "Unexpected renderer value: " *preferred-svg-render*)))))

(defn binary-stl-file->svg
  "Given only an `in-filename`, parse the indicated file, expected to be
  binary STL, and return an equivalent SVG structure. Given both `in-filename`
  and `out-filename`, as side-effect write the SVG to the indicated output file."
  ([in-filename]
   (stl->svg (cull-ocean-facets (decode-binary-stl in-filename))))
  ([in-filename out-filename]
   (let [s (binary-stl-file->svg in-filename)]
     (l/info "Emitting SVG with " *preferred-svg-render* " renderer")
     (case *preferred-svg-render*
       :dali (neatly-folded-clock/render-svg s out-filename)
       :hiccup (spit out-filename (html s))
       (throw (Exception. "Unexpected renderer value: " *preferred-svg-render*)))
     s)))
