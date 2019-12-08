(ns intcode-test
  (:require [clojure.test :refer :all]
            [intcode :as intcode]

            [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as stest]))

(stest/instrument `run)

(deftest intcode-day02
  (testing "Examples"
    (let [result (intcode/execute [1,9,10,3,2,3,11,0,99,30,40,50])]
      (is (= 3500 (-> result :values first)))))
  (testing "Solutions"
    (let [result (-> (intcode/read-input-file "day02/input")
                     (assoc 1 12 2 2)
                     intcode/execute)]
      (is (= 3654868 (-> result :values first))))
    (let [result (-> (intcode/read-input-file "day02/input")
                     (assoc 1 70 2 14)
                     intcode/execute)]
      (is (= 19690720 (-> result :values first))))))

(deftest intcode-day05
  (testing "Read modes"
    (is (= 1002 (-> (intcode/execute [1002,4,3,4,33]) :values first)))
    (is (= 1101 (-> (intcode/execute [1101,100,-1,4,0]) :values first))))
  (testing "Input/Output"
    (is (= 0 (-> (intcode/execute [3,9,8,9,10,9,4,9,99,-1,8] 1) :outputs last)))
    (is (= 1 (-> (intcode/execute [3,9,8,9,10,9,4,9,99,-1,8] 8) :outputs last)))
    (is (= 1 (-> (intcode/execute [3,3,1108,-1,8,3,4,3,99] 8) :outputs last)))
    (is (= 0 (-> (intcode/execute [3,3,1108,-1,8,3,4,3,99] 25) :outputs last))))
  (testing "Solutions"
    (let [result (intcode/execute (intcode/read-input-file "day05/input") 1)]
      (is (= 7286649 (-> result :outputs last))))
    (let [result (intcode/execute (intcode/read-input-file "day05/input") 5)]
      (is (= 15724522 (-> result :outputs last))))))
