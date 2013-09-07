(ns clock.world-test
  (:require [clojure.test :refer :all]
            [clock.world :refer :all]
            [clock.vector :refer :all]
            [clock.bird :refer :all]))

(def test-world (make-world 200 200 [] 10 11
                            1 2 3 4 5 6))
(deftest world
  (testing "world creation"
    (is (= test-world 
           {:width 200
            :height 200
            :flock []
            :max-speed 10
            :max-power 11
            :separation-distance 1
            :separation-coeff 2
            :alignment-distance 3
            :alignment-coeff 4
            :cohesion-distance 5
            :cohesion-coeff 6})))
  
  (testing "world positioning"
    (let [bird (make-bird -10 220 0 0 0 0)
          new-bird (position-bird bird test-world)]
      (is (= (new-bird :position)
             (make-vector 190 20))))))
