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
   :turtle t/initial-turtle})

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
        channel (chan)
        _ (process-channel channel [:turtle] app-state)]
    [:div {:class "svg-turtle"}
     (svg/view 640 "svg-turtle"
               (svg/render-turtle turtle {:stroke "yellow" :fill "hsla(330, 100%, 50%, 0.5)"}))
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
  )
