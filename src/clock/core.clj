(ns clock.core
  (:require [quil.core :as qc])
  (:require [clock.vector :as vect])
  (:require [clock.bird])
  (:require [clock.world :as worlds])
  (:use [clock.bird])
  (:gen-class))

(def width 800)
(def height 600)
(def flock (mapv (partial apply make-bird)
                     [[30.0 30.0 0.0 0.0]
                      [35.0 35.0 0.0 0.0]
                      [45.3 48.5 0.0 0.0]
                      [26.0 45.0 0.0 0.0]
                      [73.0 83.9 0.0 0.0]
                      ]))

(def world
  (atom {:width width
         :height height
         :shape worlds/doughnut-world-shape
         :flock flock
         :max-speed 10.0
         :max-power 10.0
         :separation-distance 50.0
         :separation-coeff 3.0
         :alignment-distance 50.0
         :alignment-coeff 3.0
         :cohesion-distance 300
         :cohesion-coeff 2.0}))

(defn draw-vertex [position]
  (qc/vertex (position :x) (position :y)))

(defn update-world []
  (let [old-flock (@world :flock)
        new-flock (mapv #(worlds/move-bird % @world) old-flock)]
    (swap! world assoc :flock new-flock)))

(defn draw []
  (qc/background-float 0)
  (qc/stroke-float 255)
  (qc/stroke-weight 5)
  (qc/begin-shape :points)
  (doseq [bird (@world :flock)]
    (draw-vertex (:position bird)))
  (qc/end-shape)
  (qc/display-filter :invert)
  (update-world))

(defn setup []
  (qc/smooth)
  (qc/no-stroke)
  (qc/fill 226)
  (qc/frame-rate 10))

(defn mouse-pressed []
  (let [x (+ 0.0 (qc/mouse-x))
        y (+ 0.0 (qc/mouse-y))
        vx (- 5.0 (rand-int 10))
        vy (- 5.0 (rand-int 10))
        max-speed (@world :max-speed)
        max-power (@world :max-power)]
    (swap! world assoc :flock (conj (@world :flock)
                                    (make-bird x y
                                               vx vy
                                               max-speed
                                               max-power)))))

(defn -main
  [& args]
  ;; work around dangerous default behaviour in Clojure
  (alter-var-root #'*read-eval* (constantly false))
  (qc/defsketch world-view
    :title "Clock"
    :setup setup
    :draw draw
    :mouse-pressed mouse-pressed
    :size [800 600]))
