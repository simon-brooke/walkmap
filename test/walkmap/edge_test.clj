(ns walkmap.edge-test
  (:require [clojure.math.numeric-tower :as m]
            [clojure.test :refer :all]
            [walkmap.edge :refer :all]
            [walkmap.vertex :refer [vertex vertex=]]))

(deftest edge-test
  (testing "identification of edges."
    (is (edge? {:start (vertex 0.0 0.0 0.0)
                :end (vertex 3 4 0.0)}) "It is.")
    (is (not (edge? {:start {:y 0.0 :z 0.0 :walkmap.id/id 'foo}
                     :end {:x 3 :y 4 :z 0.0 :walkmap.id/id 'bar}})) "Start lacks :x key")
    (is (not (edge? {:start {:x nil :y 0.0 :z 0.0 :walkmap.id/id 'foo}
                     :end {:x 3 :y 4 :z 0.0 :walkmap.id/id 'bar}})) "Start lacks :x value")
    (is (not (edge? {:begin {:x nil :y 0.0 :z 0.0 :walkmap.id/id 'foo}
                     :end {:x 3 :y 4 :z 0.0 :walkmap.id/id 'bar}})) "Lacks start key")
    (is (not (edge? {:start {:x nil :y 0.0 :z 0.0 :walkmap.id/id 'foo}
                     :finish {:x 3 :y 4 :z 0.0 :walkmap.id/id 'bar}})) "Lacks end key")
    (is (not (edge? {:start {:x "zero" :y 0.0 :z 0.0 :walkmap.id/id 'foo}
                     :end {:x 3 :y 4 :z 0.0 :walkmap.id/id 'bar}})) "Value of x in start is not a number")
    (is (false? (edge? "I am not an edge")) "Edge mustbe a map.")))

(deftest collinear-test
  (testing "collinearity"
    (is (collinear? {:start {:x 0.0 :y 0.0 :z 0.0 :walkmap.id/id 'foo} :end {:x 3.0 :y 4.0 :z 0.0 :walkmap.id/id 'bar}}
                    {:start {:x 3.0 :y 4.0 :z 0.0 :walkmap.id/id 'foo} :end {:x 9.0 :y 12.0 :z 0.0 :walkmap.id/id 'bar}})
        "Should be")
    (is (not
          (collinear? {:start {:x 0.0 :y 0.0 :z 0.0 :walkmap.id/id 'foo} :end {:x 3 :y 4 :z 0.0 :walkmap.id/id 'bar}}
                      {:start {:x 1.0 :y 2.0 :z 3.5 :walkmap.id/id 'foo} :end {:x 4.0 :y 6.0 :z 3.5 :walkmap.id/id 'bar}}))
        "Should not be!")
    (is (collinear? {:start {:x 0.0 :y 0.0 :z 0.0 :walkmap.id/id 'foo} :end {:x 3.0 :y 4.0 :z 0.0 :walkmap.id/id 'bar}}
                    {:start {:x 0.0 :y 0.0 :z 0.0 :walkmap.id/id 'foo} :end {:x 9.0 :y 12.0 :z 0.0 :walkmap.id/id 'bar}})
        "Edge case: same start location")
    (is (collinear? {:start {:x 0.0 :y 0.0 :z 0.0 :walkmap.id/id 'foo} :end {:x 9.0 :y 12.0 :z 0.0 :walkmap.id/id 'bar}}
                    {:start {:x 3.0 :y 4.0 :z 0.0 :walkmap.id/id 'foo} :end {:x 9.0 :y 12.0 :z 0.0 :walkmap.id/id 'bar}})
        "Edge case: same end location")
    ))

(deftest collinear2d-test
  (testing "Collinearity when projected onto the x,y plane."
    (is (collinear2d? (edge (vertex 1.0 1.0) (vertex 5.0 5.0))
                              (edge (vertex 4.0 4.0) (vertex 6.0 6.0)))
        "Collinear, overlapping.")
    (is (collinear2d? (edge (vertex 1.0 1.0 0.0) (vertex 5.0 5.0 5.0))
                              (edge (vertex 4.0 4.0 79.3) (vertex 6.0 6.0 0.2)))
        "Separated in the z axis, but collinear in x, y.")))

(deftest construction-test
  (testing "Construction of edges."
    (is (edge? (edge (vertex 1.0 2.0 3.0) (vertex 4.0 8.0 12.0)))
        "If both arguments are vertices, we should get an edge")
    (is (thrown? IllegalArgumentException (edge "Not a vertex" (vertex 1 2)))
        "If first argument is not a vertex, we should get an exception.")
    (is (thrown? IllegalArgumentException (edge (vertex 1 2) "Not a vertex"))
        "If second argument is not a vertex, we should get an exception.")))

(deftest intersection2d-test
  (testing "intersection of two edges projected onto the x,y plane."
    (is (thrown? IllegalArgumentException
                 (intersection2d
                   (edge (vertex 1.0 1.0) (vertex 5.0 5.0))
                   "This is not an edge"))
        "Not an edge (second arg) -> exception.")
    (is (thrown? IllegalArgumentException
                 (intersection2d
                   "This is not an edge"
                   (edge (vertex 1.0 1.0) (vertex 5.0 5.0))))
        "Not an edge (first arg) -> exception.")
    (is (nil? (intersection2d (edge (vertex 1.0 1.0) (vertex 5.0 5.0))
                              (edge (vertex 1.0 2.0) (vertex 5.0 6.0))))
        "Parallel but not intersecting.")
    (is (:x (intersection2d (edge (vertex 1.0 1.0) (vertex 5.0 5.0))
                              (edge (vertex 4.0 4.0) (vertex 6.0 6.0)))
            5.0)
        "Collinear, overlapping, should choose the overlapping end of the first edge.")
    (is (= (:x (intersection2d (edge (vertex 1.0 1.0) (vertex 5.0 5.0))
                              (edge (vertex 1.0 5.0) (vertex 5.0 1.0))))
           3.0)
        "Crossing, should intersect at 3.0, 3.0: x coord.")
    (is (= (:y (intersection2d (edge (vertex 1.0 1.0) (vertex 5.0 5.0))
                              (edge (vertex 1.0 5.0) (vertex 5.0 1.0))))
           3.0)
        "Crossing, should intersect at 3.0, 3.0: y coord.")
    (is (= (:y (intersection2d (edge (vertex 1.0 1.0 0.0) (vertex 5.0 5.0 0.0))
                              (edge (vertex 1.0 5.0 999) (vertex 5.0 1.0 379))))
           3.0)
        "Crossing, presence of z coordinate should make no difference")))

(deftest length-test
  (testing "length of an edge"
    (is (= (length {:start {:x 0.0 :y 0.0 :z 0.0 :walkmap.id/id 'foo} :end {:x 3.0 :y 4.0 :z 0.0 :walkmap.id/id 'bar}}) 5.0))))

(deftest minimad-test
  (testing "finding minimum and maximum coordinates of edges."
    (is (= (minimaxd (edge (vertex 1.0 2.0 3.0) (vertex 4.0 8.0 12.0)) :x min) 1.0))
    (is (= (minimaxd (edge (vertex 1.0 2.0 3.0) (vertex 4.0 8.0 12.0)) :y max) 8.0))))

(deftest parallel-test
  (testing "parallelism"
    (is (parallel? (edge (vertex 0.0 0.0 0.0) (vertex 3 4 0.0))
                   (edge (vertex 1.0 2.0 3.5) (vertex 4.0 6.0 3.5)))
        "Should be")
    (is (not
          (parallel? (edge (vertex 0.0 0.0 0.0) (vertex 3 4 0.0))
                     (edge (vertex 1.0 2.0 3.5) (vertex 4.0 6.0 3.49))))
        "Should not be!")))

(deftest overlaps2d-test
  (testing "whether two edges are in the same area of the x,y plane."
    (is (false? (overlaps2d? (edge (vertex 1 1) (vertex 4 4)) (edge (vertex 5 5) (vertex 8 8)))))
    (is (overlaps2d? (edge (vertex 1 1) (vertex 4 4)) (edge (vertex 4 4) (vertex 1 1))))))

(deftest unit-vector-test
  (testing "deriving the unit vector"
    (is (vertex=
          (unit-vector (edge (vertex 0.0 0.0 0.0) (vertex 3 4 0.0)))
          (vertex 0.6 0.8 0.0)))
    (is (vertex=
          (unit-vector (edge (vertex 1.0 2.0 3.5) (vertex 4.0 6.0 3.5)))
          (vertex 0.6 0.8 0.0)))))
