(ns clock.bird-test
  (:require [clojure.test :refer :all]
            [clock.vector :refer :all]
            [clock.bird :refer :all]))

(def flock (mapv (partial apply make-bird)
                     [[0 0 0 0 5 5]
                      [8 0 2 3 5 5]
                      [0 8 5 4 5 5]
                      [8 8 3 4 5 5]]))
(deftest birds
  (testing "creation and acccess"
    (let [bird-position (make-vector 0 0)
          bird-velocity (make-vector 8 0)
          bird (make-bird bird-position
                          bird-velocity 5 5)]
      (is (= (bird :position) bird-position))
      (is (= (bird :velocity) bird-velocity))))
  
  (testing "neighbours"
    (let [bird (make-bird 1 1 0 0 0 0)]
      (is (= (neighbours bird flock 4)
             [(flock 0)]))
      (is (= (neighbours bird flock 64)
             [(flock 0) (flock 1) (flock 2)]))
      (is (= (neighbours bird flock 100)
             flock)))))
