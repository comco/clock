(ns clock.bird
  (:require [clojure.contrib.math :as math])
  (:require [clock.vector :as vect]))

(defn make-bird
  "Creates a new bird."
  ([position velocity max-speed max-force]
   {:position position
    :velocity velocity
    :max-speed max-speed
    :max-force max-force})
  ([position-x position-y velocity-x velocity-y max-speed max-force]
   (make-bird (vect/make-vector position-x position-y)
              (vect/make-vector velocity-x velocity-y)
              max-speed max-force)))

(defn update-bird-velocity
  "Updates the bird velocity by a given acceleration."
  [bird acceleration]
  (assoc bird :velocity (vect/limit (vect/plus (bird :velocity) acceleration)
                                    (bird :max-speed))))
(defn update-bird-position
  [bird]
  (assoc bird :position (vect/plus (bird :position) (bird :velocity))))

(defn update-bird
  "Updates the bird state using a an acceleration."
  [bird acceleration]
  (-> bird
      (update-bird-velocity acceleration)
      (update-bird-position)))

(defn steer
  "Computes the steer needed for a bird to go to a location."
  [bird direction]
  (let [desired (vect/of-length direction (bird :max-speed))]
    (vect/limit (vect/between (bird :velocity) desired)
                (bird :max-force))))

(defn neighbours
  "Returns the birds from the flock within a given distance from a bird."
  [bird flock distance-sqr]
  (filterv #(<= 0.1
                (vect/distance-sqr (bird :position)
                                   (% :position))
                distance-sqr)
           flock))

(defn separation
  "Calculates the force on a bird induced by the separation rule."
  [bird flock distance-sqr]
  (let [bird-neighbours (neighbours bird flock distance-sqr)]
    (if (empty? bird-neighbours)
      vect/zero-vector
      (let [center (vect/center (map :position bird-neighbours))]
        (steer bird (vect/between center (bird :position)))))))

(defn alignment
  "Calculates the force on a bird induced by the alignment rule."
  [bird flock distance-sqr]
  (let [neighbours (neighbours bird flock distance-sqr)]
        (if (empty? neighbours)
          vect/zero-vector
          (let [average-velocity (vect/center (map :velocity neighbours))]
            (steer bird average-velocity)))))

(defn cohesion
  "Calculates the force on a bird induced by the cohesion rule."
  [bird flock distance-sqr]
  (let [neighbours (neighbours bird flock distance-sqr)]
    (if (empty? neighbours)
      vect/zero-vector
      (let [center (vect/center (map :position neighbours))]
        (steer bird (vect/between (bird :position) center))))))
