(ns turtle-geometry.devcards.spec
  "specs for number geometry turtle"
  (:require [cljs.spec :as s]
            [cljs.spec.impl.gen :as gen]))

;; number
(s/def :turtle-geometry.number/number
  number?)

(s/def :turtle-geometry.number/complex
  (s/cat :x :turtle-geometry.number/number
         :y :turtle-geometry.number/number))

;; geometry
(s/def :turtle-geometry.geometry/position
  (s/cat :keyword #{:position}
         :complex (s/spec :turtle-geometry.number/complex)))

(s/def :turtle-geometry.geometry/vector
  (s/cat :keyword #{:vector}
         :complex (s/spec :turtle-geometry.number/complex)))

(s/def :turtle-geometry.geometry/point-point
  (s/cat :p1 (s/spec :turtle-geometry.geometry/position)
         :p2 (s/spec :turtle-geometry.geometry/position)))

(s/def :turtle-geometry.geometry/point-vector
  (s/cat :p (s/spec :turtle-geometry.geometry/position)
         :v (s/spec :turtle-geometry.geometry/vector)))

(s/def :turtle-geometry.geometry/line
  (s/cat
   :keyword #{:line}
   :data (s/or :point-point :turtle-geometry.geometry/point-point
               :point-vector :turtle-geometry.geometry/point-vector)))

(s/def :turtle-geometry.geometry/circle
  (s/cat
   :keyword #{:circle}
   :data (s/or :point-point :turtle-geometry.geometry/point-point
               :point-vector :turtle-geometry.geometry/point-vector)))

(s/def :turtle-geometry.geometry/generalized-circle
  (s/cat
   :keyword #{:generalized-circle}
   :p1 (s/spec :turtle-geometry.geometry/position)
   :p2 (s/spec :turtle-geometry.geometry/position)
   :p3 (s/spec :turtle-geometry.geometry/position)))

(comment
  (in-ns 'turtle-geometry.devcards.spec)

  (s/conform :turtle-geometry.number/complex [(/ 2) 0])
  ;;=> {:x 0.5, :y 0}

  (s/conform :turtle-geometry.geometry/position [:position [0 0]])
  ;;=> {:keyword :position, :complex {:x 0, :y 0}}

  (s/conform :turtle-geometry.geometry/vector [:vector [1 0]])
  ;;=> {:keyword :vector, :complex {:x 1, :y 0}}

  (def point-point [[:position [0 0]] [:position [1 0]]])
  (def point-vector [[:position [0 0]] [:vector [1 0]]])

  (s/conform :turtle-geometry.geometry/point-point point-point)

  (s/conform :turtle-geometry.geometry/point-vector point-vector)

  (s/conform :turtle-geometry.geometry/line [:line point-point])
  ;;=> {:keyword :line, :data [:point-point {:p1 {:keyword :position, :complex {:x 0, :y 0}}, :p2 {:keyword :position, :complex {:x 1, :y 0}}}]}

  (s/conform :turtle-geometry.geometry/line [:line point-vector])

  (s/conform :turtle-geometry.geometry/circle [:circle point-point])
  (s/conform :turtle-geometry.geometry/circle [:circle point-vector])

  (s/conform :turtle-geometry.geometry/generalized-circle
             [:generalized-circle [:position [0 0]] [:position [1 0]] [:position [0 1]]])
  )
