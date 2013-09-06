(ns clock.world
  (:require [clock.vector :as vect]
            [clock.bird]))

(defn make-world
  "Constructs a new world."
  [width height shape flock]
  {:width width
   :height height
   :shape shape
   :flock flock})

(defn width [world]
  (world :width))

(defn height [world]
  (world :height))

(defn shape [world]
  (world :shape))

(defn flock [world]
  (world :flock))

(defn new-position
  "Computes the new position of an object inside the world."
  [world old-position translation]
  (apply (shape world)
         (width world)
         (height world)
         old-position
         translation))

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
