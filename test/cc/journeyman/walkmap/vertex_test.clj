(ns cc.journeyman.walkmap.vertex-test
  (:require [clojure.test :refer :all]
            [cc.journeyman.walkmap.utils :refer [=ish kind-type]]
            [cc.journeyman.walkmap.vertex :refer [canonicalise ensure3d vertex
                                                  vertex= vertex* vertex? 
                                                  within-box?]]))

(deftest vertex-equal-tests
  (testing "Equality of vertices"
    (is (vertex= (vertex 0 0 0) (vertex 0 0 0))
        "should be equal")
    (is (vertex= (vertex 0 0 0) (vertex 0.0000001 0 0))
        "differences less than one part in a million should be ignored")
    (is (false? (vertex= (vertex 0 0 0) (vertex 0 0 1)))
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
    (let [v (vertex 0.3333333 0.25 0.2)
          d (vertex 3 4)
          v' (vertex* v d)
          expected (vertex 1 1 0.2)]
      (is (vertex= expected v')
          "Multiplication by a 2D vertex should not change `:z`"))
    (let [v (vertex 0.3333333 0.25)
          d (vertex 3 4)
          v' (vertex* v d)
          expected (vertex 1 1 0)]
      (is (=ish 0 (:z v'))
          "Multiplication of a 2D vertex should result in `:z` = zero"))
    (is (thrown? IllegalArgumentException
                 (vertex* 3 (vertex 0 0 0)))
        "Exception should be thrown: not a vertex (1st arg).")
    (is (thrown? IllegalArgumentException
                 (vertex* (vertex 0 0 0) "Not a vertex"))
        "Exception should be thrown: not a vertex (2nd arg).")))

(deftest canonicalise-tests
  (testing "Canonicalisation of vertices."
    (is (thrown? IllegalArgumentException
                 (canonicalise {:x "3" :y 4}))
        "Exception should be thrown: not a number (`:x` coord).")
    (is (thrown? IllegalArgumentException
                 (canonicalise {:x 3 :y :Jam}))
        "Exception should be thrown: not a number (`:y` coord).")
    (is (thrown? IllegalArgumentException
                 (canonicalise {:x 3 :y :4 :z {:foo "bar"}}))
        "Exception should be thrown: not a number (`:z` coord).")
    (let [v (canonicalise {:x 3 :y 4})]
      (is
        (= (:walkmap.id/id v)
           (keyword (str "vert_" (:x v) "_" (:y v))))
        "Vertex ids should match the expected pattern.")
      (is (= (kind-type v) :vertex)
          "A canonicalised 2d vertex should have the kind `:vertex`.")
      (is (vertex? v)
          "A canonicalised 2d vertex should be recognisable as a vertex."))
    (let [v (canonicalise {:x 3 :y 4 :z 5})]
      (is
        (= (:walkmap.id/id v)
           (keyword (str "vert_" (:x v) "_" (:y v) "_" (:z v))))
        "Vertex ids should match the expected pattern.")
      (is (= (kind-type v) :vertex)
          "A canonicalised 3d vertex should have the kind `:vertex`.")
      (is (vertex? v)
          "A canonicalised 3d vertex should be recognisable as a vertex."))))

(deftest ensure3d-tests
  (testing "Coercing vertices to three dimensions"
    (let [v (vertex 2 3)
          v' (ensure3d v)]
      (is (zero? (:z v'))
          "If not already 3d, and no `dflt` arg specified, `:z` should be zero."))
    (let [v (vertex 2 3)
          v' (ensure3d v 5)]
      (is (= (:z v') 5)
          "If not already 3d, and `dflt` arg specified, `:z` should be
          equal to `dflt`."))
    (let [v (vertex 2 3 4)
          v' (ensure3d v 5)]
      (is (= v v')
          "If already 3d, should be unchanged."))))

(deftest within-box-tests
  (testing "Checking whether a vertex is within a specified region: 2d."
    (is (within-box? (vertex 2 2) (vertex 1 1) (vertex 3 3)) "Should be.")
    (is (within-box? (vertex 1 3) (vertex 1 1) (vertex 3 3)) "Should be.")
    (is (false? (within-box? (vertex 0 2) (vertex 1 1) (vertex 3 3)))
        "Outside west")
    (is (false? (within-box? (vertex 5 2) (vertex 1 1) (vertex 3 3)))
        "Outside east")
    (is (false? (within-box? (vertex 2 0) (vertex 1 1) (vertex 3 3)))
        "Outside south")
    (is (false? (within-box? (vertex 2 5) (vertex 1 1) (vertex 3 3)))
        "Outside north")
    (is (false? (within-box? (vertex 2 3.000001) (vertex 1 1) (vertex 3 3)))
        "Very slightly outside north"))
  (testing "Checking whether a vertex is within a specified region: 3d."
    (is (within-box?
          (vertex 2 2 2) (vertex 1 1 1) (vertex 3 3 3)) "Should be.")
    (is (within-box?
          (vertex 1 3 3) (vertex 1 1 1) (vertex 3 3 3)) "Should be.")
    (is (false?
          (within-box? (vertex 0 2 2) (vertex 1 1 1) (vertex 3 3 3)))
        "Outside west")
    (is (false?
          (within-box? (vertex 5 2 2) (vertex 1 1 1) (vertex 3 3 3)))
        "Outside east")
    (is (false?
          (within-box? (vertex 2 0 2) (vertex 1 1 1) (vertex 3 3 3)))
        "Outside south")
    (is (false?
          (within-box? (vertex 2 5 2) (vertex 1 1 1) (vertex 3 3 3)))
        "Outside north")
    (is (false?
          (within-box? (vertex 2 0 2) (vertex 1 1 1) (vertex 3 3 3)))
        "Outside south")
    (is (false?
          (within-box? (vertex 2 2 0) (vertex 1 1 1) (vertex 3 3 3)))
        "Outside down")
    (is (false?
          (within-box? (vertex 2 2 5) (vertex 1 1 1) (vertex 3 3 3)))
        "Outside up"))
  (testing "Bad arguments."
    (is (thrown? IllegalArgumentException
                 (within-box? :fred (vertex 1 1 1) (vertex 3 3 3)))
        "Not a vertex: `target`.")
    (is (thrown? IllegalArgumentException
                 (within-box? (vertex 2 2 2) :ginny (vertex 3 3 3)))
        "Not a vertex: `minv`.")
    (is (thrown? IllegalArgumentException
                 (within-box? (vertex 2 2 2) (vertex 1 1 1) :henry))
        "Not a vertex: `maxv`.")))
