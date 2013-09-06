(ns clock.bird
  (:require [clock.vector :as vect]))

(defn make-bird
  "Constructs a new bird."
  [species position velocity]
  {:species species
   :position position
   :velocity velocity})

(defn species
  "Extracts the species of a bird."
  [bird]
  (bird :species))

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
  (filter #(<= (vect/distance (position bird)
                              (position %))
               distance)))

(defn separation-force
  "Calculates the force on a bird induced by the separation rule."
  [bird flock distance]
  (let [neighbours (neighbours bird flock distance)
        center (vect/center (map position neighbours))]
    (vect/between center (position bird))))

(defn alignment-force
  "Calculates the force on a bird induced by the alignment rule."
  [bird flock distance]
  (let [neighbours (neighbours bird flock distance)
        average-velocity (vect/center (map velocity neighbours))]
    (vect/between (velocity bird) average-velocity)))

(defn cohesion-force
  "Calculates the force on a bird induced by the cohesion rule."
  [bird flock distance]
  (let [neighbours (neighbours bird flock distance)
        center (vect/center (map position neighbours))]
    (vect/between (position bird) center)))
