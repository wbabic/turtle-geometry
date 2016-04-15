(ns turtle-geometry.number.root-new
  "square root of positive integers
  where base itself can be made of roots"
  (:require [turtle-geometry.protocols :as p]
            [turtle-geometry.number.real :as real]))

(declare root)
(declare rat-roots)
(declare root?)

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

(defn reduce-root [base multiplier]
  (if (number? base)
    (let [lsf (largest-square-factor base)]
      (cond
        (= lsf 1) (root base multiplier)
        (= lsf base) (* (int (Math/sqrt base)) multiplier)
        :else (root (/ base lsf) (* multiplier (int (Math/sqrt lsf))))))
    (root base multiplier)))

(defn collect-roots
  "return a list of roots with like bases collected and zero multipliers removed"
  ([] nil)
  ([& roots]
   (assert (every? #(instance? Root %) roots))
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

                      (root? ratio-or-root)
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

(defrecord Root [base multiplier]
  p/Evaluate
  (p/evaluate [_]
    (* (Math/sqrt (p/evaluate base)) multiplier))

  p/Equality
  (equals? [_ y]
    (if (instance? Root y)
      (and (p/equals? base (:base y))
           (== multiplier (:multiplier y)))
      false))

  p/Addition
  (p/add [x y]
    (cond
      (number? y) (rat-roots y x)
      (instance? Root y) (rat-roots 0 x y)
      :else (p/add y x)))
  (p/negative [_]
    (root base (- multiplier)))
  (p/zero? [_]
    (zero? multiplier))

  p/Multiplication
  (multiply [x y]
    (cond
      (number? y) (root base (* multiplier y))
      (instance? Root y) (reduce-root
                          (p/multiply base (:base y))
                          (* multiplier (:multiplier y)))
      :else (p/multiply y x)))
  (one? [_] (and (p/one? base) (p/one? multiplier)))
  (reciprocal [_] (let [denom (* multiplier base)]
                    (if (zero? denom)
                      :infinity
                      (root base (/ denom))))))

(defrecord RationalRoot [ratio roots]
  p/Evaluate
  (p/evaluate [_]
    (reduce + ratio (map p/evaluate roots)))

  p/Addition
  (p/add [x y]
    (cond
      (number? y)
      (apply rat-roots (+ ratio y) roots)

      (instance? Root y)
      (apply rat-roots ratio (conj roots y))

      (instance? RationalRoot y)
      (apply rat-roots
             (+ ratio (:ratio y))
             (concat roots (:roots y)))))

  (p/negative [_]
    (apply rat-roots (- ratio) (map p/negative roots)))

  (p/zero? [_]
    (and (zero? ratio)
         (every? p/zero? roots)))

  p/Multiplication
  (p/multiply [{:keys [root ratio]} y]
    (reduce p/add
            (p/multiply ratio y)
            (map #(p/multiply % y) roots)))

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

(defn root? [x]
  (or (instance? Root x)
      (instance? RationalRoot x)))

(defn root
  ([base] (root base 1))
  ([base multiplier]
   (cond
     (or (p/zero? multiplier) (p/zero? base))
     0

     (p/one? base)
     multiplier

     :else
     (->Root base multiplier))))

(defn rat-roots
  [num & roots]
  (let [roots (filter (comp not p/zero?) roots)
        ;; roots (apply collect-roots roots)
        ]
    (cond
      (empty? roots)
      num
      (and (zero? num)
           (= 1 (count roots)))
      (first roots)
      :else
      (->RationalRoot num roots))))

(def rt5 (root 5))
(def Phi (p/multiply (p/add rt5 1) (/ 2)))

(def phi (p/multiply (p/add rt5 -1) (/ 2)))

(def cos-18
  (let [r (p/add Phi 2)]
    (root r (/ 2))))

(def sin-18
  (p/multiply phi (/ 2)))

(comment
  (require '[turtle-geometry.number.root-new] :reload)
  (in-ns 'turtle-geometry.number.root-new)

  (p/multiply rt5 (/ 2))
  (p/multiply (/ 2) rt5)
  ;;=> #turtle_geometry.number.root_new.Root{:base 5, :multiplier 1/2}
  (p/multiply rt5 rt5)
  ;;=> 5

  (p/one? (p/multiply phi Phi))
  ;;=> true
  (clojure.pprint/pprint sin-18)
  (clojure.pprint/pprint cos-18)
  (clojure.pprint/pprint (p/multiply sin-18 cos-18))
  (clojure.pprint/pprint (p/multiply cos-18 sin-18))
  (p/multiply sin-18 cos-18)
  (p/multiply cos-18 sin-18)
  )
