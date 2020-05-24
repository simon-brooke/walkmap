(ns walkmap.stl
  "Utility functions for writing stereolithography (STL) files (and possibly,
  later, other geometry files of interest to us) as scalable vector graphics
  (SVG)."
  (:require [clojure.string :as s]
            [taoensso.timbre :as l :refer [info error spy]]
            [walkmap.polygon :refer [polygon? vertex?]]))

(defn- facet->svg-poly
  [facet]
  [:polygon
   {:points (s/join " " (map #(str (:x %) "," (:y %)) (:vertices facet)))}])

(defn stl->svg
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
           (:facets stl))))]))
