(ns walkmap.stl-test
  (:require [clojure.test :refer :all]
            [walkmap.stl :refer :all]
            [walkmap.polygon :refer [polygon?]]
            [walkmap.vertex :refer [vertex?]]))

(deftest canonicalise-test
  (testing "Canonicalisation of objects read from STL: vertices."
    (is (vertex? (canonicalise {:x 3.0, :y 1.0, :z 1.0}))
        "Vertex: should have an `:id` and `:kind` = `:vertex`.")
    (is (= (:x (canonicalise {:x 3.0, :y 1.0, :z 1.0})) 3.0)
        "`:x` value should be unchanged.")
    (is (= (:y (canonicalise {:x 3.0, :y 1.0, :z 1.0})) 1.0)
        "`:y` value should be unchanged.")
    (is (= (:z (canonicalise {:x 3.0, :y 1.0, :z 1.0})) 1.0)
        "`:z` value should be unchanged.")
    (is (every?
          vertex?
          (canonicalise [{:x 3.0, :y 1.0, :z 1.0}
                         {:x 2.0, :y 3.0, :z 1.0}
                         {:x 0.0, :y 0.0, :z 1.0}]))
        "Vertices: should recurse."))
  (testing "Canonicalisation of objects read from STL: facets/polygons."
    (let [p {:normal {:x -0.0, :y 0.0, :z 1.0},
             :vertices [{:x 3.0, :y 1.0, :z 1.0}
                        {:x 2.0, :y 3.0, :z 1.0}
                        {:x 0.0, :y 0.0, :z 1.0}],
             :abc 0}
          p' (canonicalise p)]
      (is (polygon? p')
          "Polygon: should have an `:id` and `:kind` = `:polygon`.")
      (is (= (count (:vertices p)) (count (:vertices p')))
          "Number of vertices should not change")
      (map
        #(is (= (map % (:vertices p))(map % (:vertices p')))
             (str "Order of vertices should not change: " %))
        [:x :y :z]))
    (is (every?
          polygon?
          (canonicalise
            [{:normal {:x -0.0, :y 0.0, :z 1.0},
              :vertices [{:x 3.0, :y 1.0, :z 1.0}
                         {:x 2.0, :y 3.0, :z 1.0}
                         {:x 0.0, :y 0.0, :z 1.0}],
              :abc 0}
             {:normal {:x 0.0, :y 0.0, :z 1.0},
              :vertices [{:x 10.0, :y 4.0, :z 1.0}
                         {:x 22.0, :y 3.0, :z 1.0}
                         {:x 13.0, :y 5.0, :z 1.0}],
              :abc 0}
             {:normal {:x 0.0, :y 0.0, :z 1.0},
                       :vertices [{:x 26.0, :y 46.0, :z 1.0}
                                  {:x 29.0, :y 49.0, :z 1.0}
                                  {:x 31.0, :y 61.0, :z 1.0}],
                       :abc 0}
             {:normal {:x -0.0, :y 0.0, :z 1.0},
              :vertices [{:x 16.0, :y 33.0, :z 1.0}
                         {:x 15.0, :y 35.0, :z 1.0}
                         {:x 13.0, :y 32.0, :z 1.0}],
              :abc 0}
             {:normal {:x 0.0, :y 0.0, :z 1.0},
                       :vertices [{:x 81.0, :y 0.0, :z 1.0}
                                  {:x 54.0, :y 27.0, :z 1.0}
                                  {:x 51.0, :y 20.0, :z 1.0}],
                       :abc 0}]))
        "Facets/polygons: should recurse."))
  (testing "Canonicalisation of entire STL structure."
    (let [stl {:header "Dummy test STL",
               :count 5,
               :facets [{:normal {:x -0.0, :y 0.0, :z 1.0},
                         :vertices [{:x 3.0, :y 1.0, :z 1.0}
                                    {:x 2.0, :y 3.0, :z 1.0}
                                    {:x 0.0, :y 0.0, :z 1.0}],
                         :abc 0}
                        {:normal {:x 0.0, :y 0.0, :z 1.0},
                         :vertices [{:x 10.0, :y 4.0, :z 1.0}
                                    {:x 22.0, :y 3.0, :z 1.0}
                                    {:x 13.0, :y 5.0, :z 1.0}],
                         :abc 0}
                        {:normal {:x 0.0, :y 0.0, :z 1.0},
                         :vertices [{:x 26.0, :y 46.0, :z 1.0}
                                    {:x 29.0, :y 49.0, :z 1.0}
                                    {:x 31.0, :y 61.0, :z 1.0}],
                         :abc 0}
                        {:normal {:x -0.0, :y 0.0, :z 1.0},
                         :vertices [{:x 16.0, :y 33.0, :z 1.0}
                                    {:x 15.0, :y 35.0, :z 1.0}
                                    {:x 13.0, :y 32.0, :z 1.0}],
                         :abc 0}
                        {:normal {:x 0.0, :y 0.0, :z 1.0},
                         :vertices [{:x 81.0, :y 0.0, :z 1.0}
                                    {:x 54.0, :y 27.0, :z 1.0}
                                    {:x 51.0, :y 20.0, :z 1.0}],
                         :abc 0}]}
          stl' (canonicalise stl)]
      (is (stl? stl') "Stl: should have an `:id` and `:kind` = `:stl`."))))
