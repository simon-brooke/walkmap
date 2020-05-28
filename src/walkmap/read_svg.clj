(ns walkmap.read-svg
  "Utility functions for  scalable vector graphics (SVG) into walkmap
  structures."
  (:require [clojure.data.zip :as dz]
            [clojure.data.zip.xml :as zx]
            [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.xml :as x]
            [clojure.zip :as z]
            [taoensso.timbre :as l :refer [info error spy]]
            [walkmap.path :refer [path]]
;;            [walkmap.polygon :refer [polygon]]
            [walkmap.tag :refer [tag]]
            [walkmap.vertex :refer [vertex vertex?]]))

(defn upper-case?
  [s]
  (every? #(Character/isUpperCase %) s))

(defn match->vertex
  [match-vector x y]
  (let [command (nth match-vector 1)
        xcoord (read-string (nth match-vector 2))
        ycoord (read-string (nth match-vector 3))
        ;; upper case command letters mean the coordinates that follow are
        ;; absolute; lower case, relative.
        x' (if (upper-case? command) xcoord (+ x xcoord))
        y' (if (upper-case? command) ycoord (+ y ycoord))]
    (case (s/lower-case command)
      ("m" "l") {:vertex (vertex x' y') :x x' :y y'})))

(defn command-string->vertices
  [s]
  (let [matcher (re-matcher #"([a-zA-Z]) +([-+]?[0-9]*\.?[0-9]+) +([-+]?[0-9]*\.?[0-9]+) +" s)]
    (loop [match (re-find matcher) ;loop starts with 2 set arguments
           result []
           x 0
           y 0]
      (if-not match
        (filter vertex? result)
        (let [m (match->vertex match x y)]
          (recur (re-find matcher)    ;loop with 2 new arguments
                 (conj result (:vertex m))
                 (:x m)
                 (:y m)))))))

(defn path-elt->path

  [elt]
  (tag
    (path (command-string->vertices (-> elt :attrs :d)))
    (when (-> elt :attrs :class)
      (map keyword (s/split (-> elt :attrs :class) #" ")))))

(defn progeny
  "Return all the nodes in the XML structure below this `elt` which match
  this `predicate`."
  ;; the name `descendants` is bound in `clojure.core` for something quite
  ;; different, and I chose not to rebind it.
  [elt predicate]
  (if
    (apply predicate (list elt))
    (list elt)
    (reduce
      concat
      (remove
        empty?
        (map
          #(progeny % predicate)
          (:content elt))))))

(defn read-svg
  ;; I tried to get this working with all the clever zip stuff in
  ;; `clojure.zip`, `clojure.data.zip`, and so on. It would probably have
  ;; been more elegant, but it kept crashing out of heap space on even
  ;; quite small XML files. So I've implemented my own solution.
  ([file-name]
   (read-svg file-name nil))
  ([file-name map-kind]
    (let [xml (x/parse (io/file file-name))
          paths (progeny xml #(= (:tag %) :path))]
      (map path-elt->path paths))))

(read-svg "resources/iom/manual_roads.svg")


;; (def xx (z/xml-zip (x/parse (io/file "resources/iom/manual_roads.svg"))))

;; (type xx)
;; (first xx)

;; (zx/xml-> xx :svg :g :path)

;; (def xxx (x/parse (io/file "resources/iom/manual_roads.svg")))

