(ns clock.world-test
  (:require [clojure.test :refer :all]
            [clock.world :refer :all]
            [clock.vector :refer :all]
            [clock.bird :refer :all]))

(deftest shapes
  (testing "doughnut shape"
    (is (= (doughnut-world-shape 10 10
                                 (make-vector 0 0)
                                 (make-vector -2 12))
           (make-vector 8 2)))))
