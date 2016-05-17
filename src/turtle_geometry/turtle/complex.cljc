(ns turtle-geometry.turtle.complex
  "a turtle implementation using complex numbers
  providing inexact representation for rotations in any angle"
  (:require [turtle-geometry.protocols :as p]
            [turtle-geometry.turtle :as t]
            [turtle-geometry.geometry :as g]
            [turtle-geometry.number :as n]
            [turtle-geometry.number.complex :as complex :refer [zero]]
            [turtle-geometry.number.units.polar :as units :refer [unit]])
  (:import  [turtle_geometry.geometry Heading]))

(defn turtle
  "twenty-four-fold turtle constructor"
  ([] (turtle (g/position zero) (g/heading (unit 0)) (g/orientation)))
  ([point] (turtle point (g/heading (unit 0)) (g/orientation)))
  ([point heading] (turtle point heading (g/orientation)))
  ([point heading orientation]
   (t/->Turtle point heading orientation)))

(def initial-turtle (turtle))

(comment
  (require '[turtle-geometry.turtle.complex] :reload)
  (in-ns 'turtle-geometry.turtle.complex)

  (g/heading (unit 0))
  (p/angle (g/heading (unit 0)))
  (p/length (g/heading (unit 0)))
  (p/vector (g/heading (unit 0)))
  (p/equals? complex/one (p/vector (g/heading (unit 0))))

  (t/display-turtle initial-turtle)
  (-> (turtle)
      (p/turn 15)
      :heading p/vector)

  (n/rad->deg
   (units/angle (-> (turtle)
                    (p/turn 15)
                    :heading p/vector)))
  ;;=> 15.0

  (units/length (-> (turtle)
                    (p/turn 15)
                    :heading p/vector))
  ;;=> 1.0

  ;; equals? and almost-equals?
  (p/equals? initial-turtle initial-turtle)
  (p/almost-equals? initial-turtle initial-turtle 1E-10)
  ;;=> true
  (p/almost-equals? initial-turtle (-> initial-turtle (p/move 0.00001)) 1E-10)
  ;;=> false
  (p/almost-equals? initial-turtle (-> initial-turtle (p/move 1E-11)) 1E-10)
  ;;=> true
  (p/equals? initial-turtle (-> initial-turtle (p/move 1E-11)))
  ;;=> false

  (clojure.pprint/pprint
   (let [t0 initial-turtle
         t1 (-> initial-turtle (p/turn 3) (p/move 1))
         t2 (last (take 25
                        (iterate #(p/turn % 15) t1)))
         t3 (-> t2 (p/move -1) (p/turn -3))]
     [(t/display-turtle t2)
      (t/display-turtle t3)
      (p/almost-equals? (:position t0) (:position t3) 1E-10)
      (p/almost-equals? (:heading t0) (:heading t3) 1E-10)]))
  )
