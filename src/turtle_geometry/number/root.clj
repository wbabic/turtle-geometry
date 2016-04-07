(ns turtle-geometry.number.root
  "square root of positive integers"
  (:require [turtle-geometry.protocols :as p]))

;; constructor functions
(declare root)
(declare rat-roots)
(declare mult-by-root)
(declare mult-by-ratio)

(defrecord Root [base multiplier]
  p/Evaluate
  (p/evaluate [_]
    (* (Math/sqrt base) multiplier))

  p/Equality
  (equals? [x y]
    (if (instance? Root y)
      (and (== base (:base y))
           (== multiplier (:multiplier y)))
      (if (satisfies? p/Equality y)
        (p/equals? y x)
        false)))

  p/Addition
  (p/add [x y]
    (cond
      (number? y) (rat-roots y x)
      (instance? Root y)
      (if (== base (:base y))
        (root base (+ multiplier (:multiplier y)))
        (rat-roots 0 x y))
      :else (p/add y x)))
  (p/negative [_]
    (root base (- multiplier)))
  (p/zero? [_]
    (zero? multiplier))

  p/Conjugate
  (p/conjugate [_]
    (root base (- multiplier)))

  p/Multiplication
  (multiply [x y]
    (mult-by-root x y))
  (one? [_] (and (== 1 base) (== 1 multiplier)))
  (reciprocal [_] (let [denom (* multiplier base)]
                    (if (zero? denom)
                      :infinity
                      (root base (/ denom))))))

;; root constructor
(defn root
  ([base] (root base 1))
  ([base multiplier]
   (if (zero? multiplier)
     0
     (->Root base multiplier))))

(def rt2 (root 2))
(def rt3 (root 3))
(def rt5 (root 5))
(def omega (root 3 (/ 2)))

(defn collect-roots
  "collect like bases of given sequence of roots
  ignoring zero multipliers"
  ([] nil)
  ([& roots]
   (let [reduced (reduce
                  (fn [result root]
                    (let [base (:base root)
                          multiplier (:multiplier root)]
                      (update-in result [base]
                                 (fnil (fn [m]
                                         (+ m multiplier))
                                       0))))
                  (sorted-map)
                  roots)]
     (map (fn [[b m]] (root b m))
          (filter (fn [[b m]]
                    (not (zero? m)))
                  reduced)))))

(defn collect-ratios-roots
  "collect like bases of given sequence of roots and ratios
  filtering out non-zero multipliers and returning a
  ratio, Root, or RationalRoot"
  ([] 0)
  ([& ratios-and-roots]
   (let [initial {:ratio 0 :roots (sorted-map)}
         reduced (reduce
                  (fn [result ratio-or-root]
                    (cond
                      (or (integer? ratio-or-root) (ratio? ratio-or-root))
                      (update-in result [:ratio] #(+ % ratio-or-root))

                      (instance? Root ratio-or-root)
                      (let [{:keys [base multiplier]} ratio-or-root]
                        (if (== 1 base)
                          (update-in result [:ratio] (fnil (fn [n] (+ multiplier n)) 0))
                          (update-in result [:roots base]
                                                   (fnil (fn [m]
                                                           (+ m multiplier))
                                                         0))))))
                  initial
                  ratios-and-roots)]
     (apply rat-roots
            (:ratio reduced)
            (map (fn [[b m]] (root b m))
                 (filter (fn [[b m]]
                           (not (zero? m)))
                         (:roots reduced)))))))

(defn factors [n]
  (reverse
   (sort
    (loop [j 1 res []]
      (if (> (* j j) n) res
          (recur (inc j) (if (zero? (rem n j))
                           (conj res (/ n j) j)
                           res)))))))

(def squares (map #(* % %)))

(defn less-than [n]
  (take-while #(<= % n)))

(defn squares-less-than [k]
  (into #{} (comp squares (less-than k)) (range)))

(defn largest-square-factor [n]
  (let [squares (squares-less-than n)
        factors (factors n)]
    (loop [factors factors]
      (let [f (first factors)]
        (if (squares f) f
            (recur (rest factors)))))))

(defrecord RationalRoot [ratio roots]
  p/Evaluate
  (p/evaluate [_]
    (reduce + ratio (map p/evaluate roots)))

  p/Addition
  (p/add [x y]
    (cond
      (or (integer? y) (ratio? y))
      (apply rat-roots (+ ratio y) roots)

      (instance? Root y)
      (apply rat-roots ratio (apply collect-roots y roots))

      (instance? RationalRoot y)
      (apply rat-roots
             (+ ratio (:ratio y))
             (apply collect-roots (concat roots (:roots y))))))

  (p/negative [_]
    (apply rat-roots (- ratio) (map p/negative roots)))

  (p/zero? [_]
    (and (zero? ratio)
         (every? p/zero? roots)))

  p/Conjugate
  (p/conjugate [_]
    (apply rat-roots ratio (map p/conjugate roots)))

  p/Equality
  (p/equals? [_ y]
    (cond
      (instance? RationalRoot y)
      (and (== ratio (:ratio y))
           (every? true?
                   (map p/equals?
                        (apply collect-roots roots)
                        (apply collect-roots (:roots y)))))))
  p/Multiplication
  (p/multiply [{:keys [root ratio]} y]
    (reduce p/add
     (mult-by-ratio ratio y)
     (map #(mult-by-root % y) roots)))

  (p/one? [_]
    (and (== 1 ratio)
         (every? p/zero? roots)))

  (p/reciprocal [_]
    (let [root-count (count roots)]
      (condp = root-count
          0 (rat-roots (/ ratio))
          1 (let [first-root (first roots)
                  b (:base first-root)
                  m (:multiplier first-root)]
              (if (== 0 ratio)
                (p/reciprocal first-root)
                (if (== m 0)
                  (/ ratio)
                  (let [k (/ (- (* ratio ratio) (* m m b)))]
                    (rat-roots (* ratio k) (root b (* (- m) k)))))))))))

(defn rat-roots
  [num & roots]
  (cond
    (nil? roots)
    num
    (and (zero? num)
         (= 1 (count roots)))
    (first roots)
    :else
    (->RationalRoot num roots)))

(defn reduce-root [base multiplier]
  (let [lsf (largest-square-factor base)]
    (cond
      (= lsf 1) (root base multiplier)
      (= lsf base) (* (int (Math/sqrt base)) multiplier)
      :else (root (/ base lsf) (* multiplier (int (Math/sqrt lsf)))))))

(defn mult-by-ratio
  "multiply x by a ratio
  x can be a Number, a Root, or a RationalRoot"
  [ratio x]
  (if (zero? ratio) 0
      (cond
        (number? x) (* ratio x)

        (instance? Root x)
        (root (:base x) (* (:multiplier x) ratio))

        (instance? RationalRoot x)
        (apply rat-roots (* ratio (:ratio x))
               (map #(mult-by-ratio ratio %) (:roots x))))))

(defn mult-by-root
  "multiply x by a pure root rt"
  [rt x]
  (assert (instance? Root rt))
  (cond
    (number? x) (mult-by-ratio x rt)

    (instance? Root x)
    (let [b1 (:base rt)
          m1 (:multiplier rt)
          b2 (:base x)
          m2 (:multiplier x)]
      (if (== b1 b2)
        (* b1 m1 m2)
        (reduce-root (* b1 b2) (* m1 m2))))

    (instance? RationalRoot x)
    (reduce
     p/add
     (mult-by-root rt (:ratio x))
     (map #(mult-by-root rt %) (:roots x)))))

(defn root? [x]
  (or (instance? Root x)
      (instance? RationalRoot x)))

(comment
  (require '[turtle-geometry.number.root] :reload)
  (in-ns 'turtle-geometry.number.root)

  ;; property: a number plus it's negative is zero
  (let [w (rat-roots 1 rt5 omega)
        v (rat-roots 1 omega rt5)]
    (p/zero? (p/add w (p/negative w))))
  ;;=> true

  ;; multiply by a root
  (mult-by-root (root 5) (/ 2))
  (mult-by-root (root 5) (root 5))
  (mult-by-root (root 5) (rat-roots (/ 2) (root 5)))
  ;;=>
  #turtle_geometry.number.root.RationalRoot
  {:ratio 5,
   :roots (#turtle_geometry.number.root.Root
           {:base 5, :multiplier 1/2})}

  (mult-by-root (root 5) (rat-roots (/ 2) (root 5) (root 2)))
  ;;=>
  #turtle_geometry.number.root.RationalRoot
  {:ratio 5,
   :roots (#turtle_geometry.number.root.Root
           {:base 5, :multiplier 1/2}
           #turtle_geometry.number.root.Root
           {:base 10, :multiplier 1})}
  (p/equals?
   (rat-roots 5
              (root 5 (/ 2))
              (root 10))
   (mult-by-root (root 5)
                 (rat-roots (/ 2) (root 5) (root 2))))

  ;; adding two roots does not yet have a reciprocal
  (p/add rt5 omega)

  (rat-roots 0 rt5)
  (rat-roots 1)
  (rat-roots 1 rt5)
  (p/equals?
   (rat-roots 5
              (root 5 (/ 2))
              (root 10))
   (mult-by-root (root 5) (rat-roots (/ 2) (root 5) (root 2))))
  ;;=> true

  (let [x1 (root 2 (/ 2))
        y1 (p/negative x1)
        x2 (/ 2)
        y2 (root 3 (/ 2))
        x3 (p/add (p/multiply x1 x2)
                  (p/negative (p/multiply y1 y2)))
        y3 (p/add (p/multiply x1 y2)
                  (p/multiply x2 y1))]
    [x3 y3])
  [#turtle_geometry.number.root.RationalRoot
   {:ratio 0,
    :roots (#turtle_geometry.number.root.Root
            {:base 2, :multiplier 1/4}
            #turtle_geometry.number.root.Root
            {:base 6, :multiplier 1/4})}
   #turtle_geometry.number.root.RationalRoot
   {:ratio 0,
    :roots (#turtle_geometry.number.root.Root
            {:base 6, :multiplier 1/4}
            #turtle_geometry.number.root.Root
            {:base 2, :multiplier -1/4})}]

  (let [base 0
        r1 (root 6 (/ 4))
        r2 (root 2 (/ 4))
        r (rat-roots 0 r1 r2)]
    [(mult-by-ratio (:ratio r) r)
     (p/multiply r1 r)
     (p/multiply r2 r)])
  [0
   #turtle_geometry.number.root.RationalRoot
   {:ratio 3/8,
    :roots (#turtle_geometry.number.root.Root
            {:base 3, :multiplier 1/8})}
   #turtle_geometry.number.root.RationalRoot
   {:ratio 1/8,
    :roots (#turtle_geometry.number.root.Root
            {:base 3, :multiplier 1/8})}]

  (let [base 0
        r1 (root 6 (/ 4))
        r2 (root 2 (/ 4))
        r (rat-roots 0 r1 r2)]
    (p/multiply r r))
  #turtle_geometry.number.root.RationalRoot
  {:ratio 1/2,
   :roots (#turtle_geometry.number.root.Root
           {:base 3, :multiplier 1/4})}
  )
