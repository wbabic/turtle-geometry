(ns turtle-geometry.turtle
  "A transformable turtle making use of geometric transformations"
  (:require
   [turtle-geometry.protocols :as p]
   [turtle-geometry.number :as n :refer [one zero unit]]
   [turtle-geometry.geometry :as g]))

(defrecord Turtle [position heading orientation]
  p/Turtle
  (p/move [turtle distance]
    (let [v (p/multiply (p/angle->complex (:unit heading))
                        (* (:length heading) distance))]
      (update-in turtle [:position :complex] #(p/add % v))))
  (p/turn [turtle angle]
    (update-in turtle [:heading :unit :angle] #(+ % (* angle (:value orientation)))))
  (p/resize [turtle ratio]
    (update-in turtle [:heading :length] #(* % ratio)))
  (p/reflect [turtle]
    (-> turtle
        (update-in  [:heading :unit :angle] #(- %))
        (update-in  [:orientation :value] #(- %))))
  (p/invert [turtle]
    ;; todo
    turtle)

  p/Transformable
  (p/transform [turtle transformation]
    (-> turtle
        (update-in [:position]    #(p/transform % transformation))
        (update-in [:heading]     #(p/transform % transformation))
        (update-in [:orientation] #(p/transform % transformation))))

  p/Equality
  (equals? [_ turtle]
    (and (p/equals? position (:position turtle))
         (p/equals? heading (:heading turtle))
         (p/equals? orientation (:orientation turtle))))
  (almost-equals? [_ turtle epsilon]
    (and (p/almost-equals? heading (:heading turtle) epsilon)
         (p/almost-equals? position (:position turtle) epsilon)
         (p/equals? orientation (:orientation turtle)))))

(defn turtle
  "turtle constructor"
  ([] (turtle (g/position zero) (g/heading (unit 0)) (g/orientation)))
  ([point] (turtle point (g/heading (unit 0)) (g/orientation)))
  ([point heading] (turtle point heading (g/orientation)))
  ([point heading orientation]
   (->Turtle point heading orientation)))

(def initial-turtle (turtle))

(defn display-turtle
  [{:keys [position heading orientation]}]
  {:position (p/evaluate (p/point position))
   :heading {:length (p/length heading) :angle (p/angle heading)}
   :orientation (p/keyword orientation)})

(defn home->turtle
  "the transformation that brings the home turtle to the given turtle"
  [{:keys [position heading orientation]}]
  (let [ts (list
            (g/rotation    (:unit heading))
            (g/dilation    (:length heading))
            (g/translation (:complex position)))]
    (if (= :counter-clockwise (p/keyword orientation))
      (apply g/compose ts)
      (apply g/compose (g/reflection) ts))))

(defn turtle->home
  "the transformation that brings the given turtle home"
  [turtle]
  (p/inverse (home->turtle turtle)))

(defn turtle-centric-transformation
  "perform given transformation
  wrt given turtle"
  [turtle trans]
  (g/conjugate (home->turtle turtle) trans))

(defprotocol CommandProcessor
  (process-command [command turtle]))

(defrecord Forward [d])
(defrecord Turn [a])
(defrecord Resize [s])
(defrecord Reflect [])
(defrecord Invert [])

(extend-protocol CommandProcessor
  Forward
  (process-command [{d :d} turtle]
    (p/move turtle d))

  Turn
  (process-command [{a :a} turtle]
    (p/turn turtle a))

  Resize
  (process-command [{s :s} turtle]
    (p/resize turtle s))

  Reflect
  (process-command [_ turtle]
    (p/reflect turtle))

  Invert
  (process-command [_ turtle]
    (p/invert turtle))
  )

(comment
  (require '[turtle-geometry.turtle] :reload)
  (in-ns 'turtle-geometry.turtle)

  (p/equals? n/zero (:complex (g/position n/zero)))
  (g/heading (unit 0))
  (p/angle (g/heading (unit 0)))
  (p/length (g/heading (unit 0)))
  (p/vector (g/heading (unit 0)))
  (p/equals? one (p/vector (g/heading (unit 0))))
  (p/equals? n/one (p/vector (g/heading (n/unit 0))))
  (p/equals? (n/complex 2 0) (p/vector (g/heading (n/unit 0) 2)))

  (display-turtle initial-turtle)
  (-> (turtle)
      (p/turn 15)
      :heading p/vector)
  (= 15
     (-> (t/turtle)
         (p/turn 15)
         :heading
         :unit
         :angle))

  (n/rad->deg
   (n/angle (-> (t/turtle)
                    (p/turn 15)
                    :heading p/vector)))
  ;;=> 15.0

  (n/length (-> (turtle)
                (p/turn 15)
                :heading p/vector))
  ;;=> 1.0

  ;; equals? and almost-equals?
  (p/equals? initial-turtle initial-turtle)
  (p/almost-equals? initial-turtle initial-turtle 1E-10)
  ;;=> true
  (p/almost-equals? initial-turtle (-> initial-turtle (p/move 0.00001)) 1E-10)
  ;;=> false
  (p/almost-equals? initial-turtle (-> initial-turtle (p/move 1E-11)) 1E-10)
  ;;=> true
  (p/equals? initial-turtle (-> initial-turtle (p/move 1E-11)))
  ;;=> false

  (clojure.pprint/pprint
   (let [t0 initial-turtle
         t1 (-> initial-turtle (p/turn 3) (p/move 1))
         t2 (last (take 25
                        (iterate #(p/turn % 15) t1)))
         t3 (-> t2 (p/move -1) (p/turn -3))]
     [(display-turtle t2)
      (display-turtle t3)
      (p/almost-equals? (:position t0) (:position t3) 1E-10)
      (p/almost-equals? (:heading t0) (:heading t3) 1E-10)]))


  )
