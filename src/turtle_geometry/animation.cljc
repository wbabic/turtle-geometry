(ns turtle-geometry.animation)

(defn cycle-one [num-steps]
  (let [step-size (/ num-steps)]
    (fn [p]
      (mod (+ step-size p) 1))))

(def ten-cycle (cycle-one 10))
(def twelve-cycle (cycle-one 12))
(def sixteen-cycle (cycle-one 16))
(def twentyfour-cycle (cycle-one 24))

(comment
  (require '[turtle-geometry.animation] :reload)
  (in-ns 'turtle-geometry.animation)
  (use 'clojure.repl)

  (take 12 (iterate ten-cycle 0))
  (take 14 (iterate twelve-cycle 0))
  (take 18 (iterate sixteen-cycle 0))
  (take 26 (iterate twentyfour-cycle 0))
  )
