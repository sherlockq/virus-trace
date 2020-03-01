(ns virus-trace-test
  (:require [clojure.test :refer :all]
            [virus-trace :refer :all]))

(def input_source_as_target
  {:source     "ABCD"
   :target     "ABCD"
   :dictionary ["ABCD"]})

(deftest virus-trace-should
  (testing "return if have solution"
    (is (contains? (virus-trace/solve input_source_as_target) :solved)))
  (testing "has steps"
    (is (seqable? (:steps (virus-trace/solve input_source_as_target))))))

(deftest virus-trace-solves
  (testing "simple case one"
    (is (=
          {:solved true
           :steps  ["ABC"
                    "ADC"]}
          (solve {
                  :source     "ABC"
                  :target     "ADD"
                  :dictionary ["ABC"
                               "ADD"
                               "ADC"]}))))
  (testing "simple case one with noises"
    (is (=
          {:solved true
           :steps  ["ABC"
                    "ADC"]}
          (solve {
                  :source     "ABC"
                  :target     "ADD"
                  :dictionary ["ABC"
                               "ADD"
                               "ADC"
                               "ABWAS@ASD"
                               "BSWAS"
                               "ACC"
                               "CBC"]}))))
  (testing "Unsolvable case"
    (is (=
          {:solved false}
          (solve {
                  :source     "ABC"
                  :target     "ADD"
                  :dictionary ["ABC"
                               "ADD"]}))))
  (testing "Another case"
    (is (=
          {:solved true
           :steps  ["cat" "cot" "cog"]}
          (solve {
                  :source     "cat"
                  :target     "dog"
                  :dictionary ["cat"
                               "dog"
                               "cog"
                               "cot"
                               "bat"
                               "big"]})))))

(deftest neighbour-check
  (testing "only one character away is neighbour"
    (is (true? (neighbour? "A" "B")))
    (is (false? (neighbour? "AB" "BC")))
    (is (true? (neighbour? "AB" "AC")))
    (is (true? (neighbour? "ABABD" "ACABD")))
    (is (false? (neighbour? "ABABD" "ACACD"))))
  (testing "same word is not neighbour"
    (is (false? (neighbour? "A" "A"))))
  (testing "different length should never be neighbour"
    (is (false? (neighbour? "A" "CC")))))

;// outside: visited
;// in: start point, whole dictionary, distances, steps
;// distances, steps
;// if target in distances, and lowest distance, finish




(deftest update-point-test
  (testing "update distance if no value yet"
    (is (= 1
           (get-in (update-point "B" 1 nil {:distances {}})
                   [:distances "B"]))))
  (testing "update distance if new value smaller"
    (is (= 1
           (get-in (update-point "B" 1 nil {:distances {"B" 100}})
                   [:distances "B"]))))
  (testing "don't update distance if new value bigger"
    (is (= 100
           (get-in (update-point "B" 105 nil {:distances {"B" 100}})
                   [:distances "B"]))))
  (testing "update steps if no steps yet"
    (is (= ["A"]
           (get-in (update-point "B" 105 ["A"] {:distances {}})
                   [:steps "B"]))))
  (testing "update steps if new distance smaller"
    (is (= ["A"]
           (get-in (update-point "B" 1 ["A"] {:distances {"B" 100}})
                   [:steps "B"]))))
  (testing "don't update steps if new distance bigger"
    (is (= ["Somewhere"]
           (get-in (update-point "B" 105 ["A"]
                                 {:distances {"B" 100}
                                  :steps     {"B" ["Somewhere"]}})
                   [:steps "B"])))))



(deftest visit-test
  (testing "Visit one in only two points"
    (is (= {:distances {"A" 10
                        "B" 11}
            :steps     {"A" []
                        "B" ["A"]}}
           (visit {
                   :start      "A"
                   :dictionary ["A" "B"]
                   :distances  {"A" 10}
                   :steps      {"A" []}}))))
  (testing "Visit one in three points, one not neighbour"
    (is (= {:distances {"A" 10
                        "B" 11}
            :steps     {"A" []
                        "B" ["A"]}}
           (visit {
                   :start      "A"
                   :dictionary ["A" "B" "CC"]
                   :distances  {"A" 10}
                   :steps      {"A" []}}))))
  (testing "Won't change if no any neighbours"
    (is (= {:distances {"A" 10
                        "B" 11}
            :steps     {"A" []
                        "B" ["A"]}}
           (visit {
                   :start      "A"
                   :dictionary ["A" "B" "CC"]
                   :distances  {"A" 10
                                "B" 11}
                   :steps      {"A" []
                                "B" ["A"]}})))))

(deftest is-solved-after-visit-test
  (testing "If target-point is only unvisited point with distances"
    (is (= true (solved? "A" #{} {:distances {"A" 1}}))))
  (testing "If target-point has lowest known unvisited distance"
    (is (= true (solved? "A" #{} {:distances {"A" 1 "B" 2}}))))
  (testing "If target-point has same lowest known unvisited distance"
    (is (= true (solved? "A" #{} {:distances {"A" 1 "B" 1}}))))
  (testing "If target-point is lowest among unvisited"
    (is (= true (solved? "A" #{"B"} {:distances {"A" 10 "B" 2}})))))

(deftest next-visit-test
  (testing "If only point unvisited"
    (is (= "A" (next-visit #{} {"A" 1}))))
  ;(testing "If no point unvisited"
  ;  (is (= nil (next-visit #{"A"} {"A" 1}))))
  (testing "Visit lowest unvisited point"
    (is (= "A" (next-visit #{"B"} {"A" 1 "B" 0}))))
  )