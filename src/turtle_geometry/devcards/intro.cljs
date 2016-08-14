(ns turtle-geometry.devcards.intro
  (:require
   [devcards.core]
   [reagent.core :as reagent]
   [cljs.core.async :as async :refer [>! <! put! chan alts! timeout]]
   [turtle-geometry.protocols :as p]
   [turtle-geometry.geometry :as g]
   [turtle-geometry.number :as n]
   [turtle-geometry.turtle :as t]
   [turtle-geometry.mappings :as m]
   [turtle-geometry.svg.utils :as svg]
   [turtle-geometry.devcards.control-panel :as control-panel]
   [cljs.core.match :refer-macros [match]])
  (:require-macros
   [devcards.core :as dc :refer [defcard deftest defcard-rg defcard-doc]]
   [cljs.core.async.macros :refer [go]]))

(defcard story
  "intro to turtle geometry in clojurescript")

(defn initial-app-state [resolution]
  {:perspective (m/eigth resolution)
   :turtle t/initial-turtle
   :line (g/line-segment (g/position (n/complex 2 0)) (g/position (n/complex 0 2)))})

(defn process-channel [channel path app-state]
  (go (loop []
        (when-let [command (<! channel)]
          (println command)
          (swap! app-state
                 (fn [state]
                   (t/process-command command path state)))
          (recur)))))

(defn svg-turtle
  [app-state]
  (let [app @app-state
        turtle (p/transform (:turtle app) (:perspective app))
        l (:line app)
        {:keys [p1 p2]} l
        center-of-inversion (-> (:turtle app) :position p/point)
        i (g/inversion center-of-inversion
                       (-> (:turtle app) :heading p/length))
        q1 (p/transform p1 i)
        q2 (p/transform p2 i)
        line2 (g/line-segment q1 q2)
        c (g/circumcircle center-of-inversion (p/point q1) (p/point q2))
        line (p/transform (:line app) (:perspective app))
        circle (p/transform c (:perspective app))
        channel (chan)
        _ (process-channel channel [:turtle] app-state)]
    [:div {:class "svg-turtle"}
     (svg/view 640 "svg-turtle"
               (svg/render-turtle turtle {:stroke "yellow" :fill "hsla(330, 100%, 50%, 0.2)"})
               (svg/render-line-segment line nil)
               (svg/render-line-segment (p/transform line2 (:perspective app)) nil)
               (svg/render-circle circle nil))
     (control-panel/control-panel 100 channel)]))

(defcard-rg svg-turtle-card
  "an svg turtle in a devcard"
  (fn [app _] [svg-turtle app])
  (reagent/atom (initial-app-state 640))
  {:inspect-data true :history true})

(comment
  (in-ns 'turtle-geometry.devcards.intro)
  (let [f (m/eigth 640)]
    [(p/transform (g/position n/zero) f)
     (p/transform (g/position n/one) f)
     (p/transform (g/position n/i) f)])

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

  (t/process-command (t/->Forward 1) [:turtle] {:turtle (t/turtle)})

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
