(ns clock.world
  (:require [clock.vector :as vect]
            [clock.bird])
  (:use clock.bird))

(defn make-world
  "Constructs a new world."
  [width height shape flock max-speed
   separation-distance separation-coeff separation-power
   alignment-distance alignment-coeff alignment-power
   cohesion-distance cohesion-coeff cohesion-power]
  {:width width
   :height height
   :shape shape
   :flock flock
   :max-speed max-speed
   :separation-distance separation-distance
   :separation-coeff separation-coeff
   :separation-power separation-power
   :alignment-distance alignment-distance
   :alignment-coeff alignment-coeff
   :alignment-power alignment-power
   :cohesion-distance cohesion-distance
   :cohesion-coeff cohesion-coeff
   :cohesion-power cohesion-power})

(defn width [world]
  (world :width))

(defn height [world]
  (world :height))

(defn shape [world]
  (world :shape))

(defn flock [world]
  (world :flock))

(defn separation-distance [world]
  (world :separation-distance))

(defn separation-coeff [world]
  (world :separation-coeff))

(defn separation-power [world]
  (world :separation-power))

(defn alignment-distance [world]
  (world :alignment-distance))

(defn alignment-coeff [world]
  (world :alignment-coeff))

(defn alignment-power [world]
  (world :alignment-power))

(defn cohesion-distance [world]
  (world :cohesion-distance))

(defn cohesion-coeff [world]
  (world :cohesion-coeff))

(defn cohesion-power [world]
  (world :cohesion-power))

(defn max-speed [world]
  (world :max-speed))

(defn new-position
  "Computes the new position of an object inside the world."
  [world old-position v t]
  (apply (shape world)
         (width world)
         (height world)
         old-position
         (vect/scale v t)))

(defn new-velocity
  "Computes the new velocity of an object inside the world."
  [world old-velocity f t]
  (let [v (+ old-velocity (vect/scale f t))
        v-length (vect/length v)]
    (if (> v-length (max-speed world))
      (vect/scale v (/ max-speed v-length))
      v)))

(defn reflect-off
  "Takes a point to an interval by reflection."
  [x a b]
  (cond
    (< x a) (+ a (- a x))
    (> x b) (- b (- x b))
    :else x))

(defn reflective-world-shape
  "A world shape in which the birds bounce off the corners."
  [width height position translation]
  (let [new-position (vect/plus position translation)
        x (vect/x new-position)
        y (vect/y new-position)]
   (vect/make-vector (reflect-off x 0 width)
                     (reflect-off y 0 height))))

(defn doughnut-world-shape
  "A doughnut world shape"
  [width height position translation]
  (let [new-position (vect/plus position translation)
        x (vect/x new-position)
        y (vect/y new-position)]
    (vect/make-vector (mod x width)
                      (mod y height))))

(defn move-bird
  "Moves a bird inside a world, using the flocking rules."
  [bird world t]
  (let [separation (separation-force bird
                                     (flock world)
                                     (separation-distance world)
                                     (separation-coeff world)
                                     (separation-power world))
        alignment (alignment-force bird
                                   (flock world)
                                   (alignment-distance world)
                                   (alignment-coeff world)
                                   (alignment-power world))
        cohesion (cohesion-force bird
                                 (flock world)
                                 (cohesion-distance world)
                                 (cohesion-coeff world)
                                 (cohesion-power world))
        total-force (reduce vect/plus [separation
                                       alignment
                                       cohesion])
        new-v (new-velocity world (velocity bird) total-force t)
        new-p (new-position world (position bird) new-v t)]
    (make-bird new-p new-v)))
