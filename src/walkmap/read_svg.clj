(ns walkmap.read-svg
  "Utility functions for  scalable vector graphics (SVG) into walkmap
  structures."
  (:require [clojure.data.zip :as dz]
            [clojure.data.zip.xml :as zx]
            [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.xml :as x]
            [clojure.zip :as z]
            [taoensso.timbre :as l]
            [walkmap.path :refer [path]]
;;            [walkmap.polygon :refer [polygon]]
            [walkmap.tag :refer [tag]]
            [walkmap.utils :refer [kind-type truncate]]
            [walkmap.vertex :refer [vertex vertex?]]))

(defn upper-case?
  [s]
  (every? #(Character/isUpperCase %) s))

(defn match->vertex
  [match-vector x y]
  (when-not (empty? match-vector)
    (let [command (nth match-vector 1)
          xcoord (read-string (nth match-vector 2))
          ycoord (read-string (nth match-vector 3))
          ;; upper case command letters mean the coordinates that follow are
          ;; absolute; lower case, relative.
          x' (if (upper-case? command) xcoord (+ x xcoord))
          y' (if (upper-case? command) ycoord (+ y ycoord))]
      (case (s/lower-case command)
        ("m" "l") {:vertex (vertex x' y') :x x' :y y'}
        nil))))

(defn command-string->vertices
  "Return the destination of each successive line (`l`, `L`) and move (`m`, `M`)
  command in this string `s`, expected to be an SVG path command string."
  [s]
  (let [cmd-matcher ;; matches a 'command' in the string: a letter followed by
        ;;spaces and numbers
        (re-matcher #"[a-zA-Z][^a-zA-Z]*" s)
        seg-pattern ;; matches a command which initiates a move of the current
        ;; position.
        #"([a-zA-Z]) +([-+]?[0-9]*\.?[0-9]+) +([-+]?[0-9]*\.?[0-9]+) +"]
    (loop [match (re-find cmd-matcher)
           result []
           x 0
           y 0]
      (if-not match
        (filter vertex? result)
        (let [m (match->vertex (re-find seg-pattern match) x y)]
          (recur (re-find cmd-matcher)    ;loop with 2 new arguments
                 (conj result (:vertex m))
                 (or (:x m) x)
                 (or (:y m) y)))))))

(defn path-elt->path
  "Given the SVG path element `elt`, return a walkmap path structure
  representing the line (`l`, `L`) and move (`m`, `M`) commands in
  that path."
  [elt]
  (if (= (-> elt :tag) :path)
    (let [vs (command-string->vertices (-> elt :attrs :d))
          p  (when-not (empty? vs) (apply path vs))]
      (if (and p (-> elt :attrs :class))
        (tag p (map keyword (s/split (-> elt :attrs :class) #" ")))
        p))
    (throw (IllegalArgumentException.
             (str "Must be an SVG `path` element: " elt)))))

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
      (remove nil? (map path-elt->path paths)))))
