(ns walkmap.ocean-test
  (:require [clojure.test :refer :all]
            [walkmap.ocean :refer :all]
            [walkmap.polygon :refer [polygon]]
            [walkmap.vertex :refer [vertex vertex=]]))

(deftest ocean-tests
  (testing "Identification of polygons at sea level"
    (is (ocean? (polygon (vertex 0 0 0) (vertex 0 1 0) (vertex 1 0 0)))
        "All `:z` coordinates are zero, and default binding for `*sea-level*`
        => ocean.")
    (is (false? (ocean? (polygon (vertex 0 0 1) (vertex 0 1 0) (vertex 1 0 0))))
        "Not all `:z` coordinates are zero, and default binding for `*sea-level*`
        => not ocean.")
    (is (false? (ocean? (polygon (vertex 0 0 5) (vertex 0 1 5) (vertex 1 0 5))))
        "Not all `:z` coordinates are five, and default binding for `*sea-level*`
        => not ocean.")
    (binding [*sea-level* 5]
      (is (false? (ocean? (polygon (vertex 0 0 0) (vertex 0 1 0) (vertex 1 0 0))))
          "All `:z` coordinates are zero, and `*sea-level*` rebound to five
          => not ocean.")
      (is (false? (ocean? (polygon (vertex 0 0 1) (vertex 0 1 0) (vertex 1 0 0))))
          "Not all `:z` coordinates are zero, and `*sea-level*` rebound to five
          => not ocean.")
      (is (ocean? (polygon (vertex 0 0 5) (vertex 0 1 5) (vertex 1 0 5)))
          "Not all `:z` coordinates are five, and `*sea-level*` rebound to five
          => ocean."))))

(deftest cull-ocean-facets-tests
  (testing "Culling of ocean facets (not currently used)."
    (let [stl {:facets [(polygon (vertex 0 0 0) (vertex 0 1 0) (vertex 1 0 0))
                        (polygon (vertex 0 0 1) (vertex 0 1 0) (vertex 1 0 0))
                        (polygon (vertex 0 0 5) (vertex 0 1 5) (vertex 1 0 5))]}
          expected {:facets
                    [(polygon (vertex 0 0 1) (vertex 0 1 0) (vertex 1 0 0))
                     (polygon (vertex 0 0 5) (vertex 0 1 5) (vertex 1 0 5))]}
          actual (cull-ocean-facets stl)]
      (map
        #(is (vertex= (nth (:facets expected) %) (nth (:facets actual) %))
             (str "Facet " % " did not match."))
        (range (max (count (:facets expected)) (count (:facets actual))))))
    (binding [*sea-level* 5]
      (let [stl {:facets [(polygon (vertex 0 0 0) (vertex 0 1 0) (vertex 1 0 0))
                          (polygon (vertex 0 0 1) (vertex 0 1 0) (vertex 1 0 0))
                          (polygon (vertex 0 0 5) (vertex 0 1 5) (vertex 1 0 5))]}
            expected {:facets
                      [(polygon (vertex 0 0 0) (vertex 0 1 0) (vertex 1 0 0))
                       (polygon (vertex 0 0 1) (vertex 0 1 0) (vertex 1 0 0))]}
            actual (cull-ocean-facets stl)]
        (map
          #(is (vertex= (nth (:facets expected) %) (nth (:facets actual) %))
               (str "Facet " % " did not match."))
          (range (max (count (:facets expected)) (count (:facets actual)))))))))
