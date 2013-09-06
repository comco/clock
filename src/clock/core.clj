(ns clock.core
  (:require [quil.core :as qc])
  (:require [clojure.contrib.math :as math])
  (:gen-class))

; Points and vectors operations.
(defn vt
  "Constructs a new vector from displacements."
  [dx dy]
  {:dx dx
   :dy dy})

(defn vt-dx
  "Extracts the displacement along the x-coordinate of a vector."
  [v]
  (v :dx))

(defn vt-dy
  "Extracts the displacement along the y-coordinate of a vector."
  [v]
  (v :dy))

(defn pt
  "Constructs a new point from coordinates."
  [x y]
  {:x x
   :y y})

(defn pt-x
  "Extracts the x-coordinate of a point."
  [p]
  (p :x))

(defn pt-y
  "Extracts the y-coordinate of a point."
  [p]
  (p :y))

(defn vt-between
  "Constructs the vector between two points."
  [p q]
  (vt (- (pt-x q) (pt-x p))
      (- (pt-y q) (pt-y p))))

(defn vt-add
  "Adds two vectors."
  [v w]
  (vt (+ (vt-dx v) (vt-dx w))
      (+ (vt-dy v) (vt-dy w))))

(defn vt-scale
  "Scales a vector by a number."
  [v s]
  (vt (* s (vt-dx v))
      (* s (vt-dy v))))

(defn vt-length
  "Calculates the length of a vector."
  [v]
  (math/sqrt (+ (* (vt-dx v) (vt-dx v))
                (* (vt-dy v) (vt-dy v)))))

(defn pt-distance
  "Calculates the distance between two points."
  [p q]
  (vt-length (vt-between p q)))

(defn pt-move
  "Calculates a point, translated by a vector."
  [p v]
  (pt (+ (pt-x p) (vt-dx v))
      (+ (pt-y p) (vt-dy v))))

(defn pt-center
  "Calculates the geometric center of a sequence of points."
  [ps]
  (let [x-sum (reduce + (map pt-x ps))
        y-sum (reduce + (map pt-y ps))
        p-size (count ps)]
    (pt (/ x-sum p-size) (/ y-sum p-size))))

; Birds.
(defn bird
  "Creates a new bird."
  [species position velocity]
  {:species species
   :position position
   :velocity velocity})

(defn bird-species
  "Extracts the species of a bird."
  [b]
  (b :species))

(defn bird-position
  "Extracts the position of a bird."
  [b]
  (b :position))

(defn bird-velocity
  "Extracts the velocity of a bird."
  [b]
  (b :velocity))

(defn bird-neighbours
  "Returns only birds within a given distance of a bird."
  [bird flock distance]
  (filter #(<= (pt-distance bird %) distance)))

(defn bird-separation
  "Calculates the force on a bird induced by the separation rule."
  [bird flock distance]
  (let [neighbours (bird-neighbours bird flock distance)
        center (pt-center (map bird-position neighbours))]
    (vt-between center (bird-position bird))))

(defn bird-alignment
  "Calculates the force on a bird induced by the alignment rule."
  [bird flock distance]
  (let [neighbours (bird-neighbours bird flock distance)
        average-velocity (vt-average (map bird-velocity neighbours))]
    (vt-between (bird-velocity bird) average-velocity)))

(defn bird-cohesion
  "Calculates the force on a bird induced by the cohesion rule."
  [bird flock distance]
  (let [neighbours (bird-neighbours bird flock distance)
        center (pt-center (map bird-position neighbours))]
    (vt-between (bird-position bird) center)))

(defn move-bird
  "Moves a bird."
  [bird flock t
   separation-distance
   separation-coefficient
   alignment-distance
   alignment-coefficient
   cohesion-distance
   cohesion-coefficient]
  (let [separation (vt-scale (bird-separation bird flock 
                                              separation-distance)
                             separation-coefficient)
        alinment (vt-scale (bird-alignment bird flock
                                           alignment-distance)
                           alignment-coefficient)
        cohesion (vt-scale (bird-alignment bird flock
                                           cohesion-distance)
                           cohesion-coefficient)]
    (new-bird 

(def birds [(pt 20 20)
            (pt 20 80)
            (pt 80 20)
            (pt 80 80)])

(def world {:birds birds})



(defn draw []
  (qc/background-float 0)
  (qc/stroke-float 255)
  (qc/stroke-weight 5)
  (qc/with-translation [(/ 200 2) (/ 200 2)]
    (qc/with-rotation [qc/QUARTER-PI]
      (qc/begin-shape :points)
      (doseq [bird (world :birds)]
        (qc/vertex (bird :x) (bird :y)))
      (qc/end-shape)))
  (qc/display-filter :invert))

(defn setup []
  (qc/smooth)
  (qc/no-stroke)
  (qc/fill 226)
  (qc/frame-rate 24))

(defn -main
  [& args]
  ;; work around dangerous default behaviour in Clojure
  (alter-var-root #'*read-eval* (constantly false))
  (qc/defsketch world-view
    :title "Clock"
    :setup setup
    :draw draw
    :size [800 600]))
