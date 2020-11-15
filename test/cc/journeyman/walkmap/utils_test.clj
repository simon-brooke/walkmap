(ns cc.journeyman.walkmap.utils-test
  (:require [clojure.test :refer :all]
            [cc.journeyman.walkmap.utils :refer [=ish check-kind-type check-kind-type-seq kind-type truncate]]
            [cc.journeyman.walkmap.vertex :refer [vertex vertex?]]))

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

(deftest truncate-tests
  (testing "String truncation"
    (is (= (truncate "The quick brown fox jumped over the lazy dog" 19)
           "The quick brown fox")
        "If it's a sting, and longer than the desired length, it should be
        truncated.")
    (is (= (truncate "The quick brown fox jumped over the lazy dog" 100)
           "The quick brown fox jumped over the lazy dog")
        "If it's a sting, and shorter than the desired length, it should not be
        truncated.")
    (is (= (truncate :the-quick-brown-fox 10) :the-quick-brown-fox)
        "If it's not a string, it should not be truncated, regardless.")))


(deftest kind-type-tests
  (testing "Type identification."
    (is (= (kind-type {:kind :test}) :test)
        "Maps with a value for `:kind` return that as their kind.")
    (is (= (kind-type {:dnik :test}) clojure.lang.PersistentArrayMap)
        "Maps with no value for `:kind` are just maps.")
    (is (= (kind-type nil) "nil")
        "As a special case, the kind of `nil` is the string \"nil\".")
    (is (= (kind-type "Fred") java.lang.String)
        "The kind-type of anything else is just its Java class.")))

(deftest check-kind-type-tests
  (testing "Exception thrown if kind not as expected."
    (let [v {:kind :test}]
      (is (= (check-kind-type v :test) v)
          "If the check passes, the object is returned."))
    (let [v "test"]
      (is (= (check-kind-type v java.lang.String) v)
          "If the check passes, the object is returned."))
    (let [v "test"]
      (is (= (check-kind-type v string? java.lang.String) v)
          "If the check passes, the object is returned."))
    (let [v (vertex 1 1 1)]
      (is (= (check-kind-type v :vertex) v)
          "If the check passes, the object is returned."))
    (let [v (vertex 1 1 1)]
      (is (= (check-kind-type v vertex? :vertex) v)
          "If the check passes, the object is returned."))
    (let [v "test"]
      (is (thrown? IllegalArgumentException
                   (check-kind-type v :test))
          "If the check doesn't pass, an exception is thrown."))
    (let [v {:kind :test}]
      (is (thrown? IllegalArgumentException
                   (check-kind-type v vertex? :vertex))
          "If the check doesn't pass, an exception is thrown."))))

(deftest check-kind-type-seq-tests
  (testing "Exception thrown if kind not as expected: sequence variant."
    (let [v [{:kind :test} {:kind :test}]]
      (is (= (check-kind-type-seq v :test) v)
          "If the check passes, the object is returned."))
    (let [v (list "another" "test")]
      (is (= (check-kind-type-seq v java.lang.String) v)
          "If the check passes, the object is returned."))
    (let [v ["more" "test" "strings"]]
      (is (= (check-kind-type-seq v string? java.lang.String) v)
          "If the check passes, the object is returned."))
    (let [v (list (vertex 1 1 1) (vertex 2 2 2) (vertex 3 3 3))]
      (is (= (check-kind-type-seq v :vertex) v)
          "If the check passes, the object is returned."))
    (let [v (list (vertex 1 1 1))]
      (is (= (check-kind-type-seq v vertex? :vertex) v)
          "If the check passes, the object is returned."))
    (let [v :test]
      (is (thrown? IllegalArgumentException
                   (check-kind-type-seq v :test))
          "If the arg isn't a sequence, an exception is thrown."))
    (let [v (list (vertex 1 1 1) "test" (vertex 3 3 3))]
      (is (thrown? IllegalArgumentException
                   (check-kind-type-seq v :test))
          "If the check doesn't pass for any item, an exception is thrown."))
    (let [v (list (vertex 1 1 1) (vertex 2 2 2) "test")]
      (is (thrown? IllegalArgumentException
                   (check-kind-type-seq v vertex? :vertex))
          "If the check doesn't pass, an exception is thrown."))))




