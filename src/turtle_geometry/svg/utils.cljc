(ns turtle-geometry.svg.utils
  (:require
   [turtle-geometry.svg.svg :as svg]))

(defn view [resolution class-name & elements]
  (into
   [:svg {:width resolution :height resolution :class class-name}]
   elements))

(defn group [id & elements]
  (into
   (svg/group-svg id)
   elements))
