; Euclidean vector primitives and operations.
(ns clock.vector
  (:require [clojure.contrib.math :as math]))

(defn make-vector
  "Constructs a new vector."
  [x y]
  {:x x
   :y y})

(def zero-vector
  (make-vector 0.0 0.0))

(defn plus
  "Adds two vectors."
  [u v]
  (make-vector (+ (u :x) (v :x))
               (+ (u :y) (v :y))))

(defn minus
  "Subtracts two vectors."
  [u v]
  (make-vector (- (u :x) (v :x))
               (- (u :y) (v :y))))

(defn length-sqr
  "Calculates the squared length of a vector."
  [v]
  (+ (* (v :x) (v :x))
     (* (v :y) (v :y))))

(defn length
  "Calculates the length of a vector."
  [v]
  (math/sqrt (length-sqr v)))

(defn between
  "Calculates the vector between two points."
  [u v]
  (minus v u))

(defn distance
  "Calculates the distance between vectors."
  [u v]
  (length (between u v)))

(defn distance-sqr
    "Calculates the squared distance between vectors."
    [u v]
    (length-sqr (between u v)))

(defn center
  "Calculates the average of vectors."
  [coll]
  (let [sum-x (reduce + (map :x coll))
        sum-y (reduce + (map :y coll))
        size  (count coll)]
    (if (> size 0)
      (make-vector (/ sum-x size)
                   (/ sum-y size))
      zero-vector)))

(defn scale
  "Scales a vector."
  [v s]
  (make-vector (* s (v :x))
               (* s (v :y))))

(defn unit
  "Unit vector with the direction of this vector."
  [v]
  (let [v-length-sqr (length-sqr v)]
    (if (< 0.0 v-length-sqr)
      (scale v (/ 1.0 (math/sqrt v-length-sqr)))
      zero-vector)))

(defn of-length
  "Calculated a vector with the same direction with a given magnitude"
  [v mag]
  (scale (unit v) mag))

(defn limit
  "Limits the length of a vector to a given value."
  [v mag]
  (if (< mag (length v))
    (of-length v mag)
    v))
