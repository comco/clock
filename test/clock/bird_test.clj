(ns clock.bird-test
  (:require [clojure.test :refer :all]
            [clock.vector :refer :all]
            [clock.bird :refer :all]))

(def flock (mapv (partial apply make-bird)
                     [[0 0 0 0]
                      [8 0 2 3]
                      [0 8 5 4]
                      [8 8 3 4]]))
(deftest birds
  (testing "creation and acccess"
    (let [bird-position (make-vector 0 0)
          bird-velocity (make-vector 8 0)
          bird (make-bird bird-position
                          bird-velocity)]
      (is (= (position bird) bird-position))
      (is (= (velocity bird) bird-velocity))))
  
  (testing "neighbours"
    (let [bird (make-bird 1 1 0 0)]
      (is (= (neighbours bird flock 2)
             [(flock 0)]))
      (is (= (neighbours bird flock 8)
             [(flock 0) (flock 1) (flock 2)]))
      (is (= (neighbours bird flock 10)
             flock)))))
