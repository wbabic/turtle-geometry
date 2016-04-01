(ns pages.core
  (:require
   [devcards.core])
  (:require-macros
   [devcards.core :as dc :refer [defcard deftest defcard-rg defcard-doc]]))

(devcards.core/start-devcard-ui!)
