(ns turtle-geometry.devcards.control-panel
  (:require
   [devcards.core]
   [reagent.core :as reagent]
   [cljs.core.async :as async :refer [>! <! put! chan alts! timeout]]
   [turtle-geometry.protocols :as p]
   [turtle-geometry.geometry :as g]
   [turtle-geometry.number :as n]
   [turtle-geometry.turtle :as t]
   [turtle-geometry.mappings :as m]
   [turtle-geometry.svg.utils :as svg])
  (:require-macros
   [devcards.core :as dc :refer [defcard deftest defcard-rg defcard-doc]]
   [cljs.core.async.macros :refer [go]]))

(def straight-arrow
  (let [l1 (p/multiply n/one (/ 5 8))
        h1 (p/multiply n/i (/ 8))
        h2 (p/multiply n/i (/ 3 8))
        p1 h1
        p2 (p/add l1 h1)
        p3 (p/add l1 h2)
        [p5 p6 p7] (mapv p/conjugate [p3 p2 p1])
        positions (map g/position [n/zero p1 p2 p3 n/one p5 p6 p7 n/zero])]
    (g/polygon positions)))

(def turn-arrow
  (let [p1 (n/complex (/ 3 8) (/ 4))
        p2 (n/complex 0 (/ 5 8))
        p3 (n/complex (/ 3 8) 1)
        positions (map g/position [p1 p2 p3])]
    (g/polygon positions)))

(defn send!
  "Send information from the user to the message queue.
  The message must be a record which implements the Processor protocol."
  [channel message]
  (fn [dom-event]
    (put! channel message)
    (.stopPropagation dom-event)))

(defn control-panel [resolution channel]
  (let [half (m/half resolution)
        m1 (g/prepend half (g/rotation (n/unit 180)))
        m2 (g/prepend half (g/reflection))]
    [:div {:class "control-panel"}
     (svg/view resolution "control-panel"
               (svg/render-polygon (p/transform turn-arrow half)
                                   {:class "left-arrow"
                                    :on-click (send! channel (t/->Turn 15))})
               (svg/render-polygon (p/transform turn-arrow m2)
                                   {:class "right-arrow"
                                    :on-click (send! channel (t/->Turn -15))})
               (svg/render-polygon (p/transform straight-arrow half)
                                   {:class "forward-arrow"
                                    :on-click (send! channel (t/->Forward 1))})
               (svg/render-polygon (p/transform straight-arrow m1)
                                   {:class "backward-arrow"
                                    :on-click (send! channel (t/->Forward -1))}))]))
