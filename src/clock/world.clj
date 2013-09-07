(ns clock.world
  (:require [clock.vector :as vect]
            [clock.bird :as birds])
  (:use clock.bird))

(defn make-world
  "Constructs a new world."
  [width height flock
   max-speed max-power
   separation-distance separation-coeff
   alignment-distance alignment-coeff
   cohesion-distance cohesion-coeff]
  {:width width
   :height height
   :flock flock
   :max-speed max-speed
   :max-power max-power
   :separation-distance separation-distance
   :separation-coeff separation-coeff
   :alignment-distance alignment-distance
   :alignment-coeff alignment-coeff
   :cohesion-distance cohesion-distance
   :cohesion-coeff cohesion-coeff})

(defn position-bird
  "Computes the new position of a moving bird inside the world."
  [bird world]
  (let [old-position (bird :position)
        new-position-x (mod (old-position :x) (world :width))
        new-position-y (mod (old-position :y) (world :height))]
    (assoc bird :position (vect/make-vector new-position-x
                                            new-position-y))))

(defn move-bird
  "Moves a bird inside a world, using the flocking rules."
  [bird world]
  (let [separation (vect/scale (birds/separation bird
                                                 (world :flock)
                                                 (world :separation-distance))
                               (world :separation-coeff))
        alignment (vect/scale (birds/alignment bird
                                               (world :flock)
                                               (world :alignment-distance))
                              (world :alignment-coeff))
        cohesion (vect/scale (birds/cohesion bird
                                             (world :flock)
                                             (world :cohesion-distance))
                             (world :cohesion-coeff))
        old-position (bird :position)
        old-velocity (bird :velocity)
        acceleration (reduce vect/plus [separation
                                        alignment
                                        cohesion])]
    (-> bird
        (update-bird acceleration)
        (position-bird world))))

(defn update-world
  "Moves the birds in the world."
  [world]
  (let [old-flock (world :flock)
        new-flock (doall (pmap #(move-bird % world) old-flock))]
    (assoc world :flock new-flock)))
