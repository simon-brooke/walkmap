(ns cc.journeyman.walkmap.path-test
  (:require [clojure.test :refer :all]
            [cc.journeyman.walkmap.edge :refer [edge?]]
            [cc.journeyman.walkmap.path :refer [check-path check-paths 
                                                length path path? path->edges
                                                polygon->path]]
            [cc.journeyman.walkmap.polygon :refer [polygon]]
            [cc.journeyman.walkmap.utils :refer [kind-type]]
            [cc.journeyman.walkmap.vertex :refer [vertex vertex=]]))

(deftest path-tests
  (testing "Path instantiation"
    (is (= (kind-type (path (vertex 0 0 0) (vertex 1 1 1))) :path)
        "Paths should be identified as paths.")
    (is (path? (path (vertex 0 0 0) (vertex 1 1 1)))
        "Paths should test as paths.")
    (is (check-path (path (vertex 0 0 0) (vertex 1 1 1)))
        "No exception should be thrown when checking a valid path.")
    (is (thrown?
          IllegalArgumentException
          (check-path
            (update-in
              (path (vertex 0 0 0) (vertex 1 1 1))
              [:vertices]
              conj
              "Not a vertex")))
        "Checking an invalid path should throw an exception.")
    (is (thrown?
          IllegalArgumentException
          (path (vertex 0 0 0)))
        "Too short.")
    (is (thrown?
          IllegalArgumentException
          (path (vertex 0 0 0) (vertex 1 1 1) "Not a vertex"))
        "Non-vertex included.")
    (is (thrown?
          IllegalArgumentException
          (path (vertex 0 0 0) (vertex 1 1 1) "Not a vertex."))
        "Passing something which is not a vertex when constructing a path whould
        cause an exception to be thrown.")))

(deftest conversion-tests
  (testing "Converting polygons to paths"
    (let [poly (polygon (vertex 0 0 0) (vertex 1 0 0) (vertex 1 1 0) (vertex 0 1 0))
          p (polygon->path poly)]
      (is (path? p) "Should be a path.")
      (is (vertex= (first (:vertices p)) (last (:vertices p)))
          "First and last vertices of the generated path should be equal to
          one another.")
      (is (= (count (:vertices p)) (inc (count (:vertices poly))))
          "The generated path should have one more vertex than the polygon.")
      (map
        #(is (vertex= (nth (:vertices poly) %) (nth (:vertices p) %))
             (str "Vertex " % " from each set of vertices should be the same."))
        (range (count (:vertices poly))))))
  (testing "Converting polygons and paths to edges."
    (let [poly (polygon (vertex 0 0 0) (vertex 1 0 0) (vertex 1 1 0) (vertex 0 1 0))
          edges (path->edges poly)]
      (is (every? edge? edges)
          "Every returned edge should be an edge.")
      (is (= (count (:vertices poly)) (count edges))
          "There should be the same number of edges as the vertices of the polygon")
      (doall
        (map
          #(is
             (vertex= (nth (:vertices poly) %) (:start (nth edges %)))
             (str
               "Each edge should start from the same place as the corresponding
               vertex: " %))
          (range (count (:vertices poly)))))
      (doall
        (map
          #(is
             (vertex= (nth (:vertices poly) (mod (inc %) (count (:vertices poly))))
                      (:end (nth edges %)))
             (str
               "Each edge should end at the same place as the subsequent
               vertex: " %))
          (range (count (:vertices poly))))))
    (is (thrown? IllegalArgumentException
                 (path->edges "Not a legal argument.")))))

(deftest check-paths-tests
  (testing "Checking multiple paths."
    (is (thrown? IllegalArgumentException
                 (check-paths [(path (vertex 0 0 0)
                                     (vertex 1 0 0)
                                     (vertex 1 1 0)
                                     (vertex 0 1 0)
                                     (vertex 0 0 0))
                               (path (vertex 0 0 1)
                                     (vertex 1 0 1)
                                     (vertex 1 1 1)
                                     (vertex 0 1 1)
                                     (vertex 0 0 1))
                               (vertex 0 0 0)]))
        "Not all elements are paths")
    (is (check-paths [(path (vertex 0 0 0)
                            (vertex 1 0 0)
                            (vertex 1 1 0)
                            (vertex 0 1 0)
                            (vertex 0 0 0))
                      (path (vertex 0 0 1)
                            (vertex 1 0 1)
                            (vertex 1 1 1)
                            (vertex 0 1 1)
                            (vertex 0 0 1))])
        "All elements are paths")))

(deftest length-tests
  (testing "length of paths"
    (let [p (path (vertex 0 0 0) (vertex 1 0 0) (vertex 1 1 0) (vertex 0 1 0) (vertex 0 0 0))]
      (is (= (length p) 4) "By inspection."))))
