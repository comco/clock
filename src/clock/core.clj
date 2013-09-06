(ns clock.core
  (:require [quil.core :as qc])
  (:gen-class))

(def birds [{:x 10 :y 10}
            {:x 10 :y 20}
            {:x 20 :y 10}
            {:x 20 :y 20}])

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
