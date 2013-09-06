(ns clock.bird
  (:require [clojure.contrib.math :as math])
  (:require [clock.vector :as vect]))

(defn make-bird
  "Constructs a new bird."
  ([position velocity]
   {:position position
    :velocity velocity})
  ([position-x position-y velocity-x velocity-y]
   (make-bird (vect/make-vector position-x position-y)
              (vect/make-vector velocity-x velocity-y))))

(defn position
  "Extracts the position of a bird."
  [bird]
  (bird :position))

(defn velocity
  "Extracts the velocity of a bird."
  [bird]
  (bird :velocity))

(defn neighbours
  "Returns the birds from the flock within a given distance from a bird."
  [bird flock distance]
  (filterv #(<= (vect/distance (position bird)
                               (position %))
                distance)
           flock))

(defn force-law
  "Calculates the force between two objects."
  [v coeff power]
  (let [length (vect/length v)
        power-coeff (math/expt length (- (+ 1 power)))]
    (vect/scale v (* coeff power-coeff))))

(defn separation-force
  "Calculates the force on a bird induced by the separation rule."
  [bird flock distance coeff power]
  (let [neighbours (neighbours bird flock distance)
        center (vect/center (map position neighbours))
        v (vect/between center (position bird))]
    (force-law v coeff power)))

(defn alignment-force
  "Calculates the force on a bird induced by the alignment rule."
  [bird flock distance coeff power]
  (let [neighbours (neighbours bird flock distance)
        average-velocity (vect/center (map velocity neighbours))
        v (vect/between (velocity bird) average-velocity)]
    (force-law v coeff power)))

(defn cohesion-force
  "Calculates the force on a bird induced by the cohesion rule."
  [bird flock distance coeff power]
  (let [neighbours (neighbours bird flock distance)
        center (vect/center (map position neighbours))
        v (vect/between (position bird) center)]
    (force-law v coeff power)))
