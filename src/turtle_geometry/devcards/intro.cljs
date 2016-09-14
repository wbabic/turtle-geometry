(ns turtle-geometry.devcards.intro
  (:require
   [devcards.core]
   [reagent.core :as reagent]
   [cljs.core.async :as async :refer [>! <! put! chan alts! timeout]]
   [turtle-geometry.protocols :as p]
   [turtle-geometry.geometry :as g]
   [turtle-geometry.animation :as a]
   [turtle-geometry.devcards.ticker :as ticker]
   [turtle-geometry.number :as n]
   [turtle-geometry.turtle :as t]
   [turtle-geometry.mappings :as m]
   [turtle-geometry.svg.utils :as svg]
   [turtle-geometry.devcards.control-panel :as control-panel]
   turtle-geometry.devcards.spec)
  (:require-macros
   [reagent.ratom :refer [reaction]]
   [devcards.core :as dc :refer [defcard deftest defcard-rg defcard-doc]]
   [cljs.core.async.macros :refer [go]]))

(defcard story
  "Introduction to turtle geometry.")

;; svg components
(defn turtle-svg-comp [turtle p-trans]
  (svg/render-turtle
   (p-trans turtle)
   {:stroke "yellow" :fill "hsla(330, 100%, 50%, 0.2)"}))

(defn line-comp [line-seg p-trans]
  (svg/render-line-segment
   (p-trans line-seg) nil))

(defn position-comp [position p-trans]
  (svg/render-position
   (p-trans position) nil))

(defn initial-app-state [resolution]
  {:perspective (m/eigth resolution)
   :turtle t/initial-turtle
   :line (g/line-segment (g/position (n/complex 2 0)) (g/position (n/complex 0 2)))
   :parameter 0.0
   :counter 0})

(defn process-command [turtle-channel app-state-atom key]
  (go (loop []
        (when-let [command (<! turtle-channel)]
          (println command)
          (swap! app-state-atom
                 (fn [app-state]
                   (update-in app-state key
                              #(t/process-command command %))))
          (recur))))
  turtle-channel)

(defn svg-turtle
  [app-state]
  (let [tick-chan (ticker/second-ticker (chan))
        animations [[[:parameter] a/sixteen-cycle]
                    [[:counter] inc]]
        turtle-channel (process-command (chan) app-state [:turtle])
        tick-chan (ticker/process-tick tick-chan app-state animations)
        turtle-cursor (reagent/cursor app-state [:turtle])
        line-cursor (reagent/cursor app-state [:line])
        param-cursor (reagent/cursor app-state [:parameter])
        perspective-cursor (reagent/cursor app-state [:perspective])
        perspective-fn #(p/transform % @perspective-cursor)
        transform-fn (reaction
                      (let [{:keys [position heading]} @turtle-cursor]
                        (g/inversion (p/point position) (p/length heading))))
        point (reaction
               (p/value-for @line-cursor @param-cursor))
        image-point (reaction (p/transform @point @transform-fn))]
    (fn [app-state]
      [:div {:class "svg-turtle"}
       (svg/view 640 "svg-turtle"
                 [turtle-svg-comp @turtle-cursor perspective-fn]
                 [line-comp @line-cursor perspective-fn]
                 [position-comp @point perspective-fn]
                 [position-comp @image-point perspective-fn])
       (control-panel/control-panel 100 turtle-channel)])))

(defonce app-state-atom (reagent/atom (initial-app-state 640)))

(defcard-rg svg-turtle-card
  "an svg turtle in a devcard"
  (fn [app _] [svg-turtle app])
  app-state-atom
  {:inspect-data true :history true})

(comment
  (in-ns 'turtle-geometry.devcards.intro)
  (let [f (m/eigth 640)]
    [(p/transform (g/position n/zero) f)
     (p/transform (g/position n/one) f)
     (p/transform (g/position n/i) f)])

  (let [f (m/eigth 640)
        m (comp #(p/transform % f) g/position)]
    (mapv m [n/zero n/one n/i]))

  (t/display-turtle t/initial-turtle)

  (let [f (m/eigth 640)]
    (t/display-turtle
     (p/transform t/initial-turtle f)))

  (let [turtle (p/transform t/initial-turtle (m/eigth 640))]
    [(t/display-turtle turtle)
     (-> turtle :position :complex)
     (p/vector (:heading turtle))])

  (let [turtle (p/transform t/initial-turtle (m/eigth 640))]
    (svg/view 640 "board"
              (svg/render-turtle turtle {:stroke "yellow" :fill "purple"})))
  [:svg {:width 640, :height 640, :class "board"}
   [:g {:id :turtle}
    [:circle {:cx 320, :cy 320, :r 80, :fill "purple", :stroke "yellow"}]
    [:circle {:cx 320, :cy 320, :r 3, :fill "orange", :stroke "black"}]
    [:line {:x1 320, :y1 320, :x2 400, :y2 320, :stroke "blue"}]
    [:line {:x1 320, :y1 320, :x2 320, :y2 240, :stroke "green"}]]]

  (p/transform control-panel/straight-arrow (m/half 100))

  (control-panel/control-panel 100 nil)

  (g/add (g/position n/one) (g/vector n/i))
  (g/add (g/vector n/one) (g/vector n/i))
  (g/difference (g/position n/one) (g/position n/i))

  (g/line (g/position n/one) (g/vector n/i))
  (g/on-line (g/line n/one n/i) n/i)
  (g/on-line (g/line n/one n/i) (n/complex 0.5 0.5))

  (g/on-line (g/perp-line n/one n/i) n/i)
  (g/on-line (g/perp-line n/one n/i) (n/complex 0.5 0.5))
  (g/on-line (g/perp-line n/one n/i) n/zero)

  (let [l1 (g/perp-line n/zero n/one)
        l2 (g/perp-line n/one n/i)
        l3 (g/perp-line n/i n/zero)
        c (g/circumcenter n/zero n/one n/i)]
    [(g/on-line l1 c)
     (g/on-line l2 c)
     (g/on-line l2 c)])

  (let [l (g/param-line n/one n/i)]
    [(l 0)
     (l 1)
     (l (/ 2))])

  (g/steps 10)
  (map (g/param-line n/one n/i) (g/steps 10))

  (let [l (g/line-segment (g/position n/one) (g/position n/i))]
    (p/transform l (g/translation n/one)))

  (g/circumcircle n/zero (n/complex (/ 2) 0) (n/complex 0 (/ 2)))
  (-> (g/circumcircle n/zero (n/complex (/ 2) 0) (n/complex 0 (/ 2)))
      :center
      p/point)
  (-> (g/circumcircle n/zero (n/complex (/ 2) 0) (n/complex 0 (/ 2)))
      :radius
      p/vector)

  (let [c (g/circumcircle n/zero (n/complex (/ 2) 0) (n/complex 0 (/ 2)))
        circle (p/transform c (g/translation n/one))]
    (svg/render-circle circle nil))

  (let [v (g/vector (n/complex (/ 4) 0))
        p-mapping (m/eigth 640)]
    (p/transform v p-mapping))

  (let [c (g/circumcircle n/zero (n/complex (/ 2) 0) (n/complex 0 (/ 2)))
        p-mapping (m/eigth 640)
        circle (p/transform c p-mapping)]
    (svg/render-circle circle nil))

  )
