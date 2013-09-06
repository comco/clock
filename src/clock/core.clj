(ns clock.core
  (:require [quil.core :as qc])
  (:gen-class))

(def birds [{:x 10 :y 20}
            {:x 32.56 :y 38}
            {:x 14 :y 24}])

(def world {:birds birds})

(defn draw []
  (qc/background-float 0)
  (qc/stroke-float 255)
  (qc/stroke-weight 5)
  (with-translation [(/ 200 2) (/ 200 2)]
    (with-rotation [qc/QUARTER-PI]
      (begin-shape :points)
      (doseq [bird (world :birds)]
        (vertex (bird :x) (bird :y)))
      (end-shape)))
  (display-filter :invert))

(defn setup []
  (smooth)
  (no-stroke)
  (fill 226)
  (frame-rate 24))

(defn -main
  [& args]
  ;; work around dangerous default behaviour in Clojure
  (alter-var-root #'*read-eval* (constantly false))
  (defsketch world-view
    :title "Clock"
    :setup setup
    :draw draw
    :size [800 600]))
