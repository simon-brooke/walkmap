(ns walkmap.edge-test
  (:require [clojure.test :refer :all]
            [walkmap.edge :refer :all]))

(deftest edge-test
  (testing "identification of edges."
    (is (edge? {:start {:x 0.0 :y 0.0 :z 0.0} :end {:x 3 :y 4 :z 0.0}}) "It is.")
    (is (not (edge? {:start {:y 0.0 :z 0.0} :end {:x 3 :y 4 :z 0.0}})) "Start lacks :x key")
    (is (not (edge? {:start {:x nil :y 0.0 :z 0.0} :end {:x 3 :y 4 :z 0.0}})) "Start lacks :x value")
    (is (not (edge? {:begin {:x nil :y 0.0 :z 0.0} :end {:x 3 :y 4 :z 0.0}})) "Lacks start key")
    (is (not (edge? {:start {:x nil :y 0.0 :z 0.0} :finish {:x 3 :y 4 :z 0.0}})) "Lacks end key")
    (is (not (edge? {:start {:x "zero" :y 0.0 :z 0.0} :end {:x 3 :y 4 :z 0.0}})) "Value of x in start is not a number")
    ))

(deftest length-test
  (testing "length of an edge"
    (is (= (length {:start {:x 0.0 :y 0.0 :z 0.0} :end {:x 3.0 :y 4.0 :z 0.0}}) 5.0))))

(deftest unit-vector-test
  (testing "deriving the unit vector"
    (is (=
          (unit-vector {:start {:x 0.0 :y 0.0 :z 0.0} :end {:x 3 :y 4 :z 0.0}})
          {:x 0.6, :y 0.8, :z 0.0}))
    (is (=
          (unit-vector {:start {:x 1.0 :y 2.0 :z 3.5} :end {:x 4.0 :y 6.0 :z 3.5}})
          {:x 0.6, :y 0.8, :z 0.0}))))

(deftest parallel-test
  (testing "parallelism"
    (is (parallel? {:start {:x 0.0 :y 0.0 :z 0.0} :end {:x 3 :y 4 :z 0.0}}
                   {:start {:x 1.0 :y 2.0 :z 3.5} :end {:x 4.0 :y 6.0 :z 3.5}})
        "Should be")
    (is (not
          (parallel? {:start {:x 0.0 :y 0.0 :z 0.0} :end {:x 3 :y 4 :z 0.0}}
                     {:start {:x 1.0 :y 2.0 :z 3.5} :end {:x 4.0 :y 6.0 :z 3.49}}))
        "Should not be!")))

(deftest collinear-test
  (testing "collinearity"
    (is (collinear? {:start {:x 0.0 :y 0.0 :z 0.0} :end {:x 3.0 :y 4.0 :z 0.0}}
                    {:start {:x 3.0 :y 4.0 :z 0.0} :end {:x 9.0 :y 12.0 :z 0.0}})
        "Should be")
    (is (not
          (collinear? {:start {:x 0.0 :y 0.0 :z 0.0} :end {:x 3 :y 4 :z 0.0}}
                      {:start {:x 1.0 :y 2.0 :z 3.5} :end {:x 4.0 :y 6.0 :z 3.5}}))
        "Should not be!")))
