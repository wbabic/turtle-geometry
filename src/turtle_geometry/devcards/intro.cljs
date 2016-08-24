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
   [devcards.core :as dc :refer [defcard deftest defcard-rg defcard-doc]]
   [cljs.core.async.macros :refer [go]]))

(defcard story
  "intro to turtle geometry in clojurescript")

(defn initial-app-state [resolution]
  {:perspective (m/eigth resolution)
   :turtle t/initial-turtle
   :line (g/line-segment (g/position (n/complex 2 0)) (g/position (n/complex 0 2)))
   :parameter 0.0
   :counter 0})

(defn process-command [channel path app-state]
  (go (loop []
        (when-let [command (<! channel)]
          (println command)
          (swap! app-state
                 (fn [state]
                   (t/process-command command path state)))
          (recur)))))

(defn render-map [state]
  (let [{:keys [turtle line parameter perspective]} state
        center-of-inversion (-> turtle :position p/point)
        i (g/inversion center-of-inversion
                       (-> turtle :heading p/length))
        t-fn #(p/transform % i)
        p1 (:p1 line)
        p2 (:p2 line)
        line-image (t-fn line)
        q1 (t-fn p1)
        q2 (t-fn p2)
        c (g/circumcircle center-of-inversion (p/point q1) (p/point q2))]
    {:turtle turtle
     :line line
     :endpoints [p1 p2]
     :image-endpoints [q1 q2]
     :image-line c}))

(defn svg-turtle
  [app-state]
  (let [app @app-state
        p-trans #(p/transform % (:perspective app))
        turtle (p-trans (:turtle app))
        l (:line app)
        {:keys [p1 p2]} l
        center-of-inversion (-> (:turtle app) :position p/point)
        i (g/inversion center-of-inversion
                       (-> (:turtle app) :heading p/length))
        q1 (p/transform p1 i)
        q2 (p/transform p2 i)
        pq1 (p-trans q1)
        pq2 (p-trans q2)
        c (g/circumcircle center-of-inversion (p/point q1) (p/point q2))
        line (p-trans (:line app))
        circle (p-trans c)
        channel (chan)
        _ (process-command channel [:turtle] app-state)]
    [:div {:class "svg-turtle"}
     (svg/view 640 "svg-turtle"
               (svg/render-turtle turtle {:stroke "yellow" :fill "hsla(330, 100%, 50%, 0.2)"})
               (svg/render-line-segment line nil)
               (svg/render-circle circle nil)
               (svg/render-position pq1 "yellow")
               (svg/render-position pq2 "yellow"))
     (control-panel/control-panel 100 channel)]))

(defonce app-state-atom (reagent/atom (initial-app-state 640)))

(defcard-rg svg-turtle-card
  "an svg turtle in a devcard"
  (fn [app _] [svg-turtle app])
  app-state-atom
  {:inspect-data true :history true})

(let [tick-chan (ticker/second-ticker (chan))
      animations [[[:parameter] a/sixteen-cycle]
                  [[:counter] inc]]]
  (ticker/process-tick tick-chan app-state-atom animations))

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
