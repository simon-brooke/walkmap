(ns cc.journeyman.walkmap.mw-stl
  "Convert from Microworld to STL format"
  (:require [cc.journeyman.walkmap.ocean :refer [cull-ocean-facets]]
            [cc.journeyman.walkmap.polygon :refer [polygon]]
            [cc.journeyman.walkmap.vertex :refer [vertex]]
            [mw-engine.utils :refer [get-cell map-world]]))

(defn mean-altitude
  [cells]
  (let [c (remove nil? cells)]
    (if (= (count c) 1)
      (:altitude (first c))
      (/ (reduce + (map :altitude c)) (count c)))))

(defn cell->facets
  "Convert a cell into facets.
   
   All facets in an STL file must be triangles, so each MicroWorld cell needs 
   to be split into two. I am not at present seeing a rational rule for
   splitting cells, so at this stage I'm going to do it randomly to prevent
   parallel ridge artifacts appearing in the output.
   
   This function is designed to be used with `mw-engint.utils/map-world`, q.v."
  [world cell]
  (let [;; bounds
        n (:y cell)
        e (:x cell)
        s (inc n)
        w (inc e)
        ;; corner altitudes
        nea (mean-altitude [cell (get-cell world (dec n) (dec e))])
        nwa (mean-altitude [cell (get-cell world (dec n) (inc e))])
        swa (mean-altitude [cell (get-cell world (inc n) (inc e))])
        sea (mean-altitude [cell (get-cell world (inc n) (dec e))])]
    (if (rand-nth [true false])
      [(polygon (vertex n e nea) (vertex n w nwa) (vertex s e sea))
       (polygon (vertex n w nwa) (vertex s e sea) (vertex s w swa))]
      [(polygon (vertex n e nea) (vertex n w nwa) (vertex s w sea))
       (polygon (vertex n e nwa) (vertex s e sea) (vertex s w swa))])))

(defn mw->stl
  "Return an STL structure representing the topology of this MicroWorld world 
   `mw`. If `title` is supplied use that as the title of the STL structure."
  ([mw]
   (mw->stl mw nil))
  ([mw title]
   (let [facets (cull-ocean-facets (flatten (map-world mw cell->facets)))
         stl {:facets facets
              :count (count facets)}]
     (if title (assoc stl :title title) stl))))