; Euclidean vector primitives and operations.
(ns clock.vector
  (:require [clojure.contrib.math :as math]))

(defn make-vector
  "Constructs a new vector."
  [x y]
  {:x x
   :y y})

(defn x
  "Extracts the x-coordinate of a vector."
  [v]
  (v :x))

(defn y
  "Extracts the y-coordinate of a vector."
  [v]
  (v :y))

(defn plus
  "Adds two vectors."
  [u v]
  (make-vector (+ (x u) (x v))
               (+ (y u) (y v))))

(defn minus
  "Subtracts two vectors."
  [u v]
  (make-vector (- (x u) (x v))
               (- (y u) (y v))))

(defn length-sqr
  "Calculates the squared length of a vector."
  [v]
  (+ (* (x v) (x v))
     (* (y v) (y v))))

(defn length
  "Calculates the length of a vector."
  [v]
  (math/sqrt (length-sqr v)))

(defn displacement
  "Calculates the vector between two points."
  [u v]
  (minus v u))

(defn distance
  "Calculated the distance between vectors."
  [u v]
  (length (displacement u v)))

(defn center
  "Calculates the average of vectors."
  [coll]
  (let [sum-x (reduce + (map x coll))
        sum-y (reduce + (map y coll))
        size  (count coll)]
    (make-vector (/ sum-x size)
                 (/ sum-y size))))


