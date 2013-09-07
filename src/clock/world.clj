(ns clock.world
  (:require [clock.vector :as vect]
            [clock.bird :as birds])
  (:use clock.bird))

(defn make-world
  "Constructs a new world."
  [width height shape flock
   max-speed max-power
   separation-distance separation-coeff
   alignment-distance alignment-coeff
   cohesion-distance cohesion-coeff]
  {:width width
   :height height
   :shape shape
   :flock flock
   :max-speed max-speed
   :max-power max-power
   :separation-distance separation-distance
   :separation-coeff separation-coeff
   :alignment-distance alignment-distance
   :alignment-coeff alignment-coeff
   :cohesion-distance cohesion-distance
   :cohesion-coeff cohesion-coeff})

(defn world-position
  "Computes the new position of a moving object inside the world."
  [world old-position displacement]
  (let [shape-fn (world :shape)]
    (shape-fn (world :width)
              (world :height)
              old-position
              displacement)))

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
        new-position (world-position world old-position old-velocity)
        new-velocity (reduce vect/plus [old-velocity
                                        separation
                                        alignment
                                        cohesion])]
    (assoc bird :position new-position :velocity new-velocity)))

(defn doughnut-world-shape
  "A doughnut world shape"
  [width height position translation]
  (let [new-position (vect/plus position translation)]
    (vect/make-vector (mod (new-position :x) width)
                      (mod (new-position :y) height))))
