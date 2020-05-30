(ns walkmap.geometry-test
  (:require [clojure.test :refer :all]
            [walkmap.geometry :refer :all]))

(deftest =ish-tests
  (testing "Rough equality"
    (is (=ish 5.00000001 5.00000002) "Close enough.")
    (is (=ish 5 5) "Perfect.")
    (is (not (=ish 5.01 5.02)) "Not close enough.")
    (is (=ish 22/7 3.142857) "We hope so!")
    (is (=ish 0 0.0) "Tricky conrer case!")
    (is (=ish :foo :foo) "Fails over to plain old equals for non-numbers.")
    (is (=ish 6 5 10000) "If tolerance is wide enough, anything can be equal.")
    (is (not (=ish "hello" "goodbye" 10000)) "Well, except non-numbers, of course.")))
