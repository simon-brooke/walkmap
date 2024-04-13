(ns cc.journeyman.walkmap.test-all
  (:require [cc.journeyman.walkmap.mw-stl :refer [mw->stl]]
            [cc.journeyman.walkmap.stl :refer [write-ascii-stl]]
            [mw-engine.world :refer [world?]]))

(defn run-test
  ([^String name]
   (run-test (str name ".edn") (str name ".stl") name))
  ([^String mw-name ^String stl-name ^String title]
   (let [mw (read-string (slurp mw-name))]
     (if (world? mw)
       (write-ascii-stl
        stl-name
        (mw->stl mw) 
        title)
       (throw (IllegalArgumentException.
               (format
                "Content of file '%s' was not recognised as a valid world"
                mw-name)))))))