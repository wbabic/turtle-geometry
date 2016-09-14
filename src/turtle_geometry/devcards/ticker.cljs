(ns turtle-geometry.devcards.ticker
  (:require
   [turtle-geometry.animation :as a]
   [cljs.core.async :as async :refer [>! <! put! chan alts! timeout]])
  (:require-macros
   [cljs.core.async.macros :refer [go]]))


(defn apply-animations [animations]
  (fn [state]
    (let [rf (fn [state [key function]]
               (update-in state key function))]
      (reduce rf state animations))))

(defn process-tick [tick-chan state-atom animations]
  (go (loop []
        (let [tick (<! tick-chan)]
          (swap! state-atom (apply-animations animations))
          (recur))))
  tick-chan)

(defn second-ticker [ticker-chan]
  (go (loop []
        (<! (timeout 1000))
        (>! ticker-chan :tick)
        (recur)))
  ticker-chan)

(comment
  (in-ns 'turtle-geometry.devcards.ticker)

  (let [s2 {:turtle :t2 :parameter 0}
        a2 [[[:parameter] a/sixteen-cycle]]]
    ((apply-animations a2) s2))
  )
