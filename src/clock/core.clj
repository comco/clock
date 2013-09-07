(ns clock.core
  (:require [quil.core :as qc])
  (:require [clock.vector :as vect])
  (:require [clock.bird])
  (:require [clock.world :as worlds])
  (:use [clock.bird])
  (:gen-class))

; World size.
(def width 800)
(def height 600)

; Initial flock.
(def flock (mapv (partial apply make-bird)
                     [[30.0 30.0 0.0 0.0 2.0 0.03]
                      [35.0 35.0 0.0 0.0 2.0 0.03]
                      [45.3 48.5 0.0 0.0 2.0 0.03]
                      [26.0 45.0 0.0 0.0 2.0 0.03]
                      [73.0 83.9 0.0 0.0 2.0 0.03]]))

(def world
  (atom {:width width
         :height height
         :flock flock
         :max-speed 2.0
         :max-power 0.03
         :separation-distance (* 25.0 25.0)
         :separation-coeff 1.0
         :alignment-distance (* 50.0 50.0)
         :alignment-coeff 1.0
         :cohesion-distance (* 50.0 50.0)
         :cohesion-coeff 1.0}))

(defn draw-bird [bird]
  (let [p (bird :position)
        v (vect/scale (bird :velocity) 2.0)]
    (qc/fill 200 100)
    (qc/stroke 255)
    (qc/push-matrix)
    (qc/translate (p :x) (p :y))
    (qc/begin-shape :lines)
    (qc/vertex 0 0)
    (qc/vertex (v :x) (v :y))
    (qc/end-shape)
    (qc/pop-matrix))) 

(defn update-world []
    (swap! world worlds/update-world))

(defn settings-string []
  (format 
"separation: distance (keys 's'/'S') = %.2f, coefficient (keys = 'e'/'E') %.2f
alignment: distance (keys 'a'/'A') = %.2f, coefficient (keys = 'l'/'L') = %.2f
cohesiton: distance (keys 'c'/'C') = %.2f, coefficient (keys = 'o'/'O') = %.2f
birds: %d"
    (@world :separation-distance)
    (@world :separation-coeff)
    (@world :alignment-distance)
    (@world :alignment-coeff)
    (@world :cohesion-distance)
    (@world :cohesion-coeff)
    (count (@world :flock))))

; Keys map
(def valid-keys {\s {:key :separation-distance
                     :amount -20.0}
                 \S {:key :separation-distance 
                     :amount +20.0}
                 \e {:key :separation-coeff
                     :amount -0.1}
                 \E {:key :separation-coeff
                     :amount +0.1}
                 \a {:key :alignment-distance
                     :amount -20.0}
                 \A {:key :alignment-distance
                     :amount +20.0}
                 \l {:key :alignment-coeff
                     :amount -0.1}
                 \L {:key :alignment-coeff
                     :amount +0.1}
                 \c {:key :cohesion-distance
                     :amount -20.0}
                 \C {:key :cohesion-distance
                     :amount +20.0}
                 \o {:key :cohesion-coeff
                     :amount -0.1}
                 \O {:key :cohesion-coeff
                     :amount +0.1}})

(defn update-by-setting
  "Updates a world setting by a given amount"
  ([settings]
   (let [old-value (@world (settings :key))
         new-value (+ old-value (settings :amount))]
     (swap! world assoc (settings :key) new-value))))

; Change the settings on a key press
(defn key-pressed []
  (let [key-code (qc/raw-key)
        setting (valid-keys key-code)]
    (if setting
      (update-by-setting setting))))

(defn draw []
  (qc/background-float 0)
  (qc/stroke-float 255)
  (qc/stroke-weight 5)
  (doseq [bird (@world :flock)]
    (draw-bird bird))
  (qc/text-size 12)
  (qc/text (settings-string) 20 20)
  (qc/display-filter :invert)
  (update-world))

(defn setup []
  (qc/smooth)
  (qc/no-stroke)
  (qc/fill 226)
  (qc/frame-rate 20))

; Create a new bird on a mouse press
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
  (qc/sketch
    :title "Clock"
    :setup setup
    :draw draw
    :mouse-pressed mouse-pressed
    :key-pressed key-pressed
    :size [800 600]))
