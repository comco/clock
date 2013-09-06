(ns clock.vector-test
  (:require [clojure.test :refer :all]
            [clock.vector :refer :all]))

(defn having-coordinates?
  [v [expected-x expected-y]]
  (and (= (x v) expected-x)
       (= (y v) expected-y)))

(deftest vectors
  (testing "creation and acccess"
    (let [v (make-vector 2 1)]
      (is (having-coordinates? v [2 1]))))

  (testing "addition"
    (let [u (make-vector 2 1)
          v (make-vector 6 8)]
      (is (having-coordinates? (plus u v) [8 9]))))

  (testing "subtraction"
    (let [u (make-vector 9 5)
          v (make-vector 4 3)]
      (is (having-coordinates? (minus u v) [5 2]))))
  
  (testing "between"
    (let [u (make-vector 0 0)
          v (make-vector 4 6)]
      (is (having-coordinates? (between u v) [4 6]))))

  (testing "length"
    (let [v (make-vector 3 4)]
      (is (= (length-sqr v) 25))
      (is (= (length v) 5))))

  (testing "distance"
    (let [u (make-vector 0 0)
          v (make-vector 3 4)]
      (is (= (distance u v) 5))))

  (testing "scale"
    (let [v (make-vector 3 4)]
      (is (having-coordinates? (scale v 2) [6 8]))))

  (testing "center"
    (let [coll [(make-vector 0 0)
                (make-vector 0 8)
                (make-vector 8 0)
                (make-vector 8 8)]]
      (is (having-coordinates? (center coll) [4 4])))))
