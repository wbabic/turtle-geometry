(ns turtle-geometry.svg.svg
  (:require [clojure.string :as s]))

(defn line [p1 p2 stroke]
  (let [[x1 y1] p1
        [x2 y2] p2]
    [:line {:x1 x1 :y1 y1 :x2 x2 :y2 y2 :stroke stroke}]))

(defn circle [center radius stroke fill]
  (let [[cx cy] center]
    [:circle {:cx cx :cy cy :r radius :fill fill :stroke stroke}]))

(defn square
  [class-name position base]
  (let [[x y] position]
    [:rect {:class class-name
            :x x :y y
            :width base :height base}]))

(defn polygon
  [point-string options]
  [:polygon
   (merge {:points point-string}
          options)])

(defn group-svg [id & elements]
  (into [:g {:id id}] elements))

(defn path [path-string]
  [:path {:d path-string}])

(defn defs [& paths]
  (into [:defs] paths))

(defn transform-str [[x y] angle scale]
  (str "translate(" x "," y ") "
       "rotate(" angle ") "
       "scale(" scale ") "))

(defn use-path [id transform-str class-name]
  [:use {:xlink-href id
         :class class-name
         :transform transform-str
         :stroke "black"
         :fill "transparent"}])

;; svg path components
(defrecord M [point])
(defrecord m [vector])
(defrecord L [point])
(defrecord l [vector])
(defrecord Q [c-point point])
(defrecord q [c-vector vector])

(defprotocol Path
  (as->str [path-component]))

(defn point->str [[x y]]
  (str x " " y " "))

(extend-protocol Path
  M
  (as->str [{point :point}]
    (str "M " (point->str point)))
  m
  (as->str [{vector :vector}]
    (str "m " (point->str vector)))
  L
  (as->str [{point :point}]
    (str "L " (point->str point)))
  l
  (as->str [{vector :vector}]
    (str "l " (point->str vector)))
  Q
  (as->str [{c-point :c-point point :point}]
    (str "Q " (point->str c-point) " " (point->str point)))
  q
  (as->str [{c-vector :c-vector vector :vector}]
    (str "q " (point->str c-vector) " " (point->str vector))))

(defn reduce-path [& path-components]
  (s/join (map as->str path-components)))

(comment
  (require '[hello-devcards.svg] :reload)
  (in-ns 'hello-devcards.svg)

  )
