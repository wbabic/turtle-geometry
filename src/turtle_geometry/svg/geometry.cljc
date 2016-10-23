(ns turtle-geometry.svg.geometry
  (:require
   [turtle-geometry.svg.svg :as svg]
   #?(:clj [turtle-geometry.geometry :as g]
      :cljs [turtle-geometry.geometry :as g :refer [LineSegment Position Polygon Circle]])
   #?(:clj [turtle-geometry.turtle :as t]
      :cljs [turtle-geometry.turtle :as t :refer [Turtle]])
   [turtle-geometry.number :as n]
   [turtle-geometry.protocols :as p])
  #?(:clj
     (:import
      [turtle_geometry.geometry LineSegment Position Polygon Circle]
      [turtle_geometry.turtle Turtle])))

(defn round
  "round given number"
  [n]
  (if (integer? n)
    n
    (Math/round n)))

(defn round-c
  "round a given vector"
  [c]
  (mapv round c))

(def to-screen
  "maps a complex number into rounded coordinates"
  (comp round-c p/evaluate))

(defn point-str [[x y]]
  (str x "," y))

(defn view [resolution class-name & elements]
  (into
   [:svg {:width resolution :height resolution :class class-name}]
   elements))

(defn group [id & elements]
  (into
   (svg/group-svg id)
   elements))

(defn render [object & {stroke :stroke fill :fill :as style :or {stroke "black" fill "grey"}}]
  (condp instance? object
    LineSegment
    (let [{:keys [p1 p2]} object]
      (svg/line (to-screen (:complex p1))
                (to-screen (:complex p2))
                stroke))

    Position
    (svg/circle (to-screen (:complex object)) 3 stroke fill)

    Polygon
    (let [positions (:positions object)
          point-string (map (comp point-str to-screen :complex) positions)]
      (svg/polygon point-string style))

    Circle
    (let [{:keys [center radius]} object]
      (svg/circle (-> center p/point to-screen)
                  radius
                  stroke
                  fill))

    Turtle
    (let [{:keys [position heading orientation]} object]
      (group :turtle
             (render (t/shell object) :stroke stroke :fill fill)
             (render position)
             (render (t/heading object) :stroke "blue")
             (render (t/heading-perp object) :stroke "green")))))

(comment
  (require '[turtle-geometry.svg.geometry] :reload)
  (in-ns 'turtle-geometry.svg.geometry)
  (use 'clojure.repl)

  (render (g/line-segment (g/position n/zero) (g/position n/one)))
  (render (g/line-segment (g/position n/zero) (g/position n/one)) :stroke "grey")
  (render (g/position n/zero))
  (render (g/position n/zero) :stroke "red")
  (render (g/position n/zero) :stroke "red" :fill "blue")
  (render (g/polygon (map g/position [n/zero n/one n/i])))
  (g/circle (g/position n/zero) 1)
  (= (g/circle (g/position n/zero) 1) g/unit-circle)
  (render (g/circle (g/position n/zero) 1))
  (render g/unit-circle :stroke "red" :fill "orange")
  (= (t/shell t/initial-turtle) g/unit-circle)
  (render t/initial-turtle)

  (let [p1 (n/complex (/ 3 8) (/ 4))
        p2 (n/complex 0 (/ 5 8))
        p3 (n/complex (/ 3 8) 1)
        positions (map g/position [p1 p2 p3])]
    (render (g/polygon positions)
            :class "backward-arrow"
            :on-click :send))
  )
