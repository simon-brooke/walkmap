(ns walkmap.core
  "This namespace mostly gets used as a scratchpad for ideas which haven't yet
  solidified."
  (:require [clojure.java.io :as io :refer [file output-stream input-stream]]
            [clojure.string :as s]
            [hiccup.core :refer [html]]
            [me.raynes.fs :as fs]
            [taoensso.timbre :as l :refer [info error spy]]))

