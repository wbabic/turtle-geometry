(ns turtle-geometry.devcards.intro
  (:require
   [devcards.core]
   [reagent.core :as reagent]
   [cljs.core.async :as async :refer [>! <! put! chan alts! timeout]]
   [turtle-geometry.protocols :as p]
   [turtle-geometry.geometry :as g]
   [turtle-geometry.number :as n]
   [turtle-geometry.turtle :as turtle]
   [turtle-geometry.mappings :as m]
   [turtle-geometry.svg.utils :as svg])
  (:require-macros
   [devcards.core :as dc :refer [defcard deftest defcard-rg defcard-doc]]
   [cljs.core.async.macros :refer [go]]))

(defcard story
  "intro to turtle geometry in clojurescript")

(comment
  (in-ns 'turtle-geometry.devcards.intro)
  (let [f (m/mapping-transform 640 8)]
    [(p/transform (g/position n/zero) f)
     (p/transform (g/position n/one) f)
     (p/transform (g/position n/i) f)])

  (svg/view 640 "board" :test1 :test2)
  (svg/group "test" :element1 :element2)
  )
