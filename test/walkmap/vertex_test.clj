(ns walkmap.utils-test
  (:require [clojure.test :refer :all]
            [walkmap.vertex :refer :all]))

(deftest vertex-equal-tests
  (testing "Equality of vertices"
    (is (vertex= (vertex 0 0 0) (vertex 0 0 0))
        "should be equal")
    (is (vertex= (vertex 0 0 0) (vertex 0.0000001 0 0))
        "differences less than one part in a million should be ignored")
    (is (vertex= (vertex 0 0 0) (vertex 0 0 1))
        "should not be equal")
    (is (thrown? IllegalArgumentException
                 (vertex= (vertex 0 0 0) "Not a vertex"))
        "Exception should be thrown: not a vertex.")))

(deftest vertex-multiply-tests
  (testing "multiplication of vertices"
    (let [v (vertex (rand) (rand) (rand))
          u (vertex 1 1 1)
          v' (vertex* v u)]
      (is (vertex= v v')
          "Multiplication by {:x 1 :y 1 :z 1} should not change the vertex"))
    (let [v (vertex 0.333333 0.25 0.2)
          d (vertex 3 4 5)
          v' (vertex* v d)
          expected (vertex 1 1 1)]
      (is (vertex= expected v')
          "Multiplication by values other than {:x 1 :y 1 :z 1} should change
          the vertex"))
    (let [v (vertex 0.333333 0.25 0.2)
          d (vertex 3 4)
          v' (vertex* v d)
          expected (vertex 1 1 0.2)]
      (is (vertex= expected v')
          "Multiplication by a 2D vertex should not change `:z`"))
    (let [v (vertex 0.333333 0.25)
          d (vertex 3 4)
          v' (vertex* v d)
          expected (vertex 1 1 0)]
      (is (vertex= expected v')
          "Multiplication of a 2D vertex should result in `:z` = zero"))
    (is (thrown? IllegalArgumentException
                 (vertex* (vertex 0 0 0) "Not a vertex"))
        "Exception should be thrown: not a vertex.")))
