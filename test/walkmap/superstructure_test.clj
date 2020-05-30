(ns walkmap.superstructure-test
  (:require [clojure.set :refer [subset?]]
            [clojure.test :refer :all]
            [walkmap.path :as p]
            [walkmap.polygon :as q]
            [walkmap.superstructure :refer :all]
            [walkmap.utils :as u]
            [walkmap.vertex :as v]))

(deftest store-test
  (testing "Object storage"
    (let [p (p/path
              (v/vertex (rand) (rand) (rand))
              (v/vertex (rand) (rand) (rand))
              (v/vertex (rand) (rand) (rand))
              (v/vertex (rand) (rand) (rand)))
          id (:walkmap.id/id p)
          s (store p)
          r (id s)]
      (is (= (:walkmap.id/id r) id)
          "A representation should be stored in `s` keyed by `id`, and the id of that representation should be `id`.")
      (is (= (:kind r) (:kind p))
          "The representation should have the same value for `:kind`.")
      (is (= (count (:vertices p)) (count (:vertices r)))
          "The representation of `p` in `s` should have the same number of vertices as `p`.")
      (is (every? v/vertex? (:vertices p))
          "Every vertex of `p` should be a vertex.")
      (is (every? keyword? (:vertices r))
          "Every vertex of the representation of `p` in `s` should be a keyword.")
      (is (every? v/vertex? (map #(s %) (:vertices r)))
          "The value in `s` of every vertex of the representation of `p` in `s`
          should be a vertex.")
      (is (subset? (set (:vertices r)) (set (keys (vertex-index s))))
          "All the keys which are vertices of the  representation of `p` in `s`
          should be present as keys in the vertex-index of `s`.")
      (is (every?
            #(s (% id))
            (map #(set (keys (% (vertex-index s)))) (:vertices r)))
          "The value in the vertex-index in `s` for each keyword in the
          vertexes of the representation of `p` in `s` should include,
          as a key, the `id` of `p`."))))

(deftest retrieve-test
  (testing "Object retrieval"
    ;; the value of `s` here is hand-typed; think of it as a specification
    (let [s {:path1 {:walkmap.id/id :path1
                     :kind :path
                     :vertices [:vert_0_0_0
                                :vert_0_0_1
                                :vert_1_0_0]}
             :vert_0_0_0 {:walkmap.id/id :vert_0_0_0
                          :kind :vertex
                          :x 0
                          :y 0
                          :z 0}
             :vert_0_0_1 {:walkmap.id/id :vert_0_0_1
                          :kind :vertex
                          :x 0
                          :y 0
                          :z 1}
             :vert_1_0_0 {:walkmap.id/id :vert_1_0_0
                          :kind :vertex
                          :x 1
                          :y 0
                          :z 0}
             :walkmap.superstructure/vertex-index {:vert_0_0_0 {:path1 :vert_0_0_0}
                                                   :vert_0_0_1 {:path1 :vert_0_0_1}
                                                   :vert_1_0_0 {:path1 :vert_1_0_0}}}
          expected {:kind :path,
                    :vertices
                    ({:kind :vertex, :x 0, :y 0, :z 0, :walkmap.id/id :vert_0_0_0}
                            {:kind :vertex, :x 0, :y 0, :z 1, :walkmap.id/id :vert_0_0_1}
                            {:kind :vertex, :x 1, :y 0, :z 0, :walkmap.id/id :vert_1_0_0}),
                    :walkmap.id/id :path1}]
      (is (= (retrieve :path1 s) expected)
          "The object reconstructed from the superstructure."))))
