(ns turtle-geometry.svg.utils
  (:require
   [turtle-geometry.svg.svg :as svg]
   [turtle-geometry.geometry :as g]
   [turtle-geometry.number :as n]
   [turtle-geometry.protocols :as p]))

(defn round
  "round given number"
  [n]
  (Math/round n))

(defn round-c
  "round a given complex number"
  [c]
  (mapv round c))


(def to-screen
  "maps a complex number into rounded coordinates"
  (comp round-c p/evaluate))

(defn view [resolution class-name & elements]
  (into
   [:svg {:width resolution :height resolution :class class-name}]
   elements))

(defn group [id & elements]
  (into
   (svg/group-svg id)
   elements))

(defn point-str [[x y]]
  (str x "," y))

(defn render-heading [position heading stroke]
  (svg/line (to-screen (:complex position))
            (to-screen (p/add (:complex position) (p/vector heading)))
            stroke))

(defn render-position [position fill]
  (svg/circle (to-screen (:complex position)) 3 "black" fill))

(defn rotate [heading orientation]
  (let [value (:value orientation)
        {:keys [length unit]} heading
        angle (:angle unit)]
    (g/heading (n/unit (+ (* value 90) angle)) length)))

(defn render-shell [turtle stroke fill]
  (let [center (to-screen (-> turtle :position :complex))
        radius (-> turtle :heading :length)]
    (svg/circle center radius stroke fill)))

(defn render-turtle [turtle color-map]
  (let [{:keys [stroke fill]} color-map
        {:keys [position heading orientation]} turtle]
    (group :turtle
           (render-shell turtle stroke fill)
           (render-position position "orange")
           (render-heading position heading "blue")
           (render-heading position
                           (rotate heading orientation)
                           "green"))))

(defn render-polygon [polygon options]
  (let [positions (:positions polygon)
        point-string (map (comp point-str to-screen :complex) positions)]
    (svg/polygon point-string options)))

(defn render-line [p1 p2 stroke]
  (svg/line (to-screen (:complex p1))
            (to-screen (:complex p2))
            stroke))

(defn render-line-segment [line-segment options]
  (let [{:keys [p1 p2]} line-segment]
    (group :line
           (render-position p1 "red")
           (render-position p2 "red")
           (render-line p1 p2 "purple"))))

(defn render-circle [circle options]
  (let [{:keys [center radius]} circle]
    (svg/circle (-> center p/point to-screen)
                (-> radius p/length)
                "black"
                "rgba(0,0,0,0)")))
