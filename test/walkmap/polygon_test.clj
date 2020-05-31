(ns walkmap.polygon-test
  (:require [clojure.test :refer :all]
;;            [clojure.algo.generic.math-functions :as m]
;;             [walkmap.edge :refer [edge?]]
;;             [walkmap.path :refer :all]
            [walkmap.polygon :refer :all]
             [walkmap.utils :refer [kind-type]]
             [walkmap.vertex :refer [vertex vertex? vertex=]])
  )

(deftest polygon-tests
  (testing "Constructing polygons"
    (let [square (polygon (vertex 0 0 0) (vertex 1 0 0)
                          (vertex 1 1 0) (vertex 0 1 0))
          triangle (polygon (vertex 0 0 0) (vertex 0 3 0)
                            (vertex 4 0 0))]
      (is (= (kind-type square) :polygon)
          "Square should have `:kind` = `:polygon`.")
      (is (= (kind-type triangle) :polygon)
          "Triangle should have `:kind` = `:polygon`.")
      (is (polygon? square) "Square should be a polygon.")
      (is (polygon? triangle) "Triangle should be a polygon.")
      (is (false? (triangle? square)) "Square is not a triangle.")
      (is (triangle? triangle) "Triangle is a triangle.")
      (is (check-polygon square) "No exception should be thrown.")
      (is (check-polygon triangle) "No exception should be thrown.")
      (is (check-triangle triangle) "No exception should be thrown.")
      (is (check-polygons [square triangle])
          "No exception should be thrown.")
      (is (thrown?
            IllegalArgumentException
            (check-polygon "Not a polygon")) "Not a polygon")
      (is (thrown?
            IllegalArgumentException
            (check-polygons [square triangle "Not a polygon"]))
          "One value is not a polygon.")
      (is (thrown?
            IllegalArgumentException (check-triangle square))
          "Not a triangle.")
      (is (thrown?
            IllegalArgumentException (polygon (vertex 0 0 0) (vertex 1 0 0)))
          "Too few vertices.")
      (is (thrown?
            IllegalArgumentException (polygon (vertex 0 0 0) (vertex 1 0 0)
                                              (vertex 1 1 0) "Not a vertex"
                                              (vertex 0 1 0)))
          "Non-vertex included.")
      )
    ))

(deftest gradient-tests
  (testing "Finding the gradient across a triangle."
    (let [tri (polygon (vertex 0 0 1) (vertex 1 0 0) (vertex 1 1 0.5))
          gra (gradient tri)]
      (is (nil? (:gradient tri)) "Basic trangle should not have a gradient.")
      (is (vertex? (:gradient gra))
          "After passing through gradient function, it should have a gradient.")
      ;; TODO: I need to check that the gradient is being computed correclt,
      ;; but my brain isn't up to the trigonometry just now.
      )))

(deftest centre-tests
  (testing "Finding the centres of polygons."
    (let [square (polygon (vertex 0 0 0) (vertex 1 0 0)
                          (vertex 1 1 0) (vertex 0 1 0))
          triangle (polygon (vertex 0 0 0) (vertex 0 3 0)
                            (vertex 4 0 0))
          centred (centre triangle)]
      (is (vertex= (:centre centred) (vertex 1.3333333 1.0 0.0))
          "By inspection (check this maths!).")
      (is (thrown?
            UnsupportedOperationException
            (centre square))
          "We can't yet find the centre of a quadrilateral, but we should be
          able to do so, so it isn't an illegal argument, it just doesn't
          work.")
      (is (thrown?
            IllegalArgumentException
            (centre "Not a polygon"))
          "Anything else that isn't a polygon, though, is an illegal argument."))))

