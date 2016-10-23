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

(defn interval-ticker [ticker-chan msecs]
  (go (loop []
        (<! (timeout msecs))
        (>! ticker-chan :tick)
        (recur)))
  ticker-chan)

(defn second-ticker [ticker-chan]
  (interval-ticker ticker-chan 1000))

(comment
  (in-ns 'turtle-geometry.devcards.ticker)

  (let [s2 {:turtle :t2 :parameter 0}
        a2 [[[:parameter] a/sixteen-cycle]]]
    ((apply-animations a2) s2))
  )
