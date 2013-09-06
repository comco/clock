(ns clock.world-test
  (:require [clojure.test :refer :all]
            [clock.world :refer :all]
            [clock.vector :refer :all]
            [clock.bird :refer :all]))

(deftest shapes
  (testing "box reflection"
    (is (= (reflect-off 10 0 8) 6))
    (is (= (reflect-off -1 0 8) 1))
    (is (= (reflect-off 10 0 12) 10)))

  (testing "2d box reflection"
    (is (= (reflective-world-shape 10 10
                                   (make-vector 0 0)
                                   (make-vector -2 12))
           (make-vector 2 8))))

  (testing "doughnut shape"
    (is (= (doughnut-world-shape 10 10
                                 (make-vector 0 0)
                                 (make-vector -2 12))
           (make-vector 8 2)))))
