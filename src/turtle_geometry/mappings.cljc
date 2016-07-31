(ns turtle-geometry.mappings
  (:require
   [turtle-geometry.number :as n]
   [turtle-geometry.geometry :as g]))

(defn mapping [resolution fraction]
  (let [k (/ resolution fraction)
        m (/ resolution 2)
        v (n/complex m m)]
    (g/compose
     (g/reflection)
     (g/dilation k)
     (g/translation v))))

(defn eigth [resolution]
  (mapping resolution 8))

(comment
  (require '[turtle-geometry.mappings] :reload)
  (in-ns 'turtle-geometry.mappings)
  )
