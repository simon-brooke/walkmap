(ns walkmap.tag-test
  (:require [clojure.test :refer :all]
            [walkmap.tag :refer :all]))

(deftest tag-tests
  (testing "Tagging"
    (is (set? (:walkmap.tag/tags (tag {} :foo :bar :ban :froboz)))
        "The value of `:walkmap.tag/tags` should be a set.")
    (is (= (count (:walkmap.tag/tags (tag {} :foo :bar :ban :froboz))) 4)
        "All the tags passed should be added.")
    (is (:walkmap.tag/tags (tag {} :foo :bar :ban :froboz) :ban)
        "`:ban` should be present in the set, and, as it is a set, it
        should be valid to apply it to a keyword.")
    (is (not ((:walkmap.tag/tags (tag {} :foo :bar :ban :froboz)) :cornflakes))
        "`:cornflakes should not be present.")
    (is (true? (tagged? (tag {} :foo :bar :ban :froboz) :bar))
        "`tagged?` should return an explicit `true`, not any other value.")
    (is (tagged? (tag {} :foo :bar :ban :froboz) :bar :froboz)
        "We should be able to test for the presence of more than one tag")
    (is (false? (tagged? {} :foo))
        "A missing `:walkmap.tag/tags` should not cause an error.")
    (is (= (tagged? (tag {} :foo :bar :ban :froboz) :bar :cornflakes) false)
        "If any of the queried tags is missing, false should be returned")
    (is (tagged? (tag (tag {} :foo) :bar) :foo :bar)
        "We should be able to add tags to an already tagged object")
    (is (false? (tagged? (tag {} :foo :bar) :cornflakes))
        "`tagged?` should return an explicit `false` if a queried tag is missing.")
    (is (= (tags (tag {} :foo)) #{:foo})
        "`tags` should return the tags on the object, if any.")
    (is (every? nil? (map #(tags %) [1 :one "one" [:one] {:one 1}]))
        "Things which don't have tags don't have tags, and that's not a problem.")
    (let [object (tag {} :foo :bar :ban :froboz)]
      (is (= (untag object :cornflakes) object)
          "Removing a missing tag should have no effect.")
      (is (tagged? (untag object :foo) :bar :ban :froboz)
          "All tags not explicitly removed should still be present.")
      (is (false? (tagged? (untag object :bar) :bar))
          "But the tag which has been removed should be removed."))
    (is (thrown? IllegalArgumentException (tag [] :foo))
        "An exception should be thrown if `object` is not a map: `tag`.")
    (is (thrown? IllegalArgumentException (tagged? [] :foo))
        "An exception should be thrown if `object` is not a map: `tagged?`.")
    (is (thrown? IllegalArgumentException (untag [] :foo))
        "An exception should be thrown if `object` is not a map: `untag`.")
    (is (thrown? IllegalArgumentException (tag {} :foo "bar" :ban))
        "An exception should be thrown if any of `tags` is not a keyword: `tag`.")
    (is (thrown? IllegalArgumentException (tagged? {} :foo "bar" :ban))
        "An exception should be thrown if any of `tags` is not a keyword: `tagged?`.")
    (is (thrown? IllegalArgumentException (untag {} :foo "bar" :ban))
        "An exception should be thrown if any of `tags` is not a keywordp: `untag`.")
    (let [o (tag {} :foo '(:bar :ban) :froboz)]
      (is (tagged? o :ban :bar :foo :froboz)
          "It's now allowed to include lists of tags in the arg list for `tag`."))))

