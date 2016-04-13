(ns turtle-geometry.number.root-test
  "tests for Root and RationalRoot"
  (:require [turtle-geometry.protocols :as p]
            [turtle-geometry.number.real :as real]
            [turtle-geometry.number.root :as root]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            #?@(:clj
                [[clojure.test :refer :all]
                 [clojure.test.check.clojure-test :refer [defspec]]
                 [clojure.test.check.properties :as prop]]
                :cljs
                [[cljs.test :as text :refer-macros [is deftest are testing run-tests]]
                 [clojure.test.check.clojure-test :refer-macros [defspec]]
                 [clojure.test.check.properties :as prop :include-macros true]])))

(deftest roots
  (testing "root constructor"
    (is (= 0 (root/root 2 0)) "a root with a zero multiplier should be 0")
    (let [r (root/root 2)]
      (is (= 2 (:base r)) "a root has a base")
      (is (= 1 (:multiplier r)) "a root has a multiplier"))))

(deftest rat-roots
  (testing "rat-roots constructor does the right thing"
    (is (= 0 (root/rat-roots 0)))
    (is (= 1 (root/rat-roots 1)))
    (is (= 2 (root/rat-roots 2 (root/root 5 0))))
    (is (p/equals? (root/root 5)
                   (root/rat-roots 0 (root/root 5))))
    (let [r (root/rat-roots 1 (root/root 2) (root/root 3))]
      (is (= 1 (:ratio r)) "a rat-root has a ratio")
      (is (= 2 (count (:roots r))) "a rat-root has one or more roots"))
    (let [r (root/rat-roots 1 (root/root 2) (root/root 2))]
      (is (= 1 (:ratio r)) "a rat-root has a ratio")
      (is (= 1 (count (:roots r))) "constructor collects roots with like bases"))))

(deftest mult-by-root
  (testing "multiply by root yields a Number, Root or RationalRoot"
    (is (p/equals? 5
                   (root/mult-by-root (root/root 5) (root/root 5))))
    (is (p/equals? (root/root 5 (/ 2))
                   (root/mult-by-root (root/root 5) (/ 2))))
    (is (p/equals? (root/root 6)
                   (root/mult-by-root (root/root 2) (root/root 3))))
    (is (p/equals?
         (root/rat-roots 5 (root/root 5 (/ 2)) (root/root 10))
         (root/mult-by-root (root/root 5)
                            (root/rat-roots (/ 2) (root/root 5) (root/root 2)))))))

;; generator for root
(def root-gen
  (gen/fmap (partial apply root/root)
            (gen/vector gen/nat 2)))

(def additive-inverse-root-prop
  (prop/for-all [r root-gen]
                (p/zero? (p/add r (p/negative r)))))

(defspec additive-inverse-root additive-inverse-root-prop)

(def non-zero-root-gen
  (gen/fmap (partial apply root/root)
            (gen/vector gen/s-pos-int 2)))

(def multiplicative-inverse-root-prop
  (prop/for-all [r non-zero-root-gen]
                (p/one? (p/multiply r (p/reciprocal r)))))

(defspec multiplicative-inverse multiplicative-inverse-root-prop)

(deftest phi
  (is (p/one? (p/multiply root/Phi root/phi)))
  (is (p/one? (p/add root/Phi (p/negative root/phi))))
  (is (p/equals? root/Phi (p/reciprocal root/phi)))
  (is (p/equals? root/phi (p/reciprocal root/Phi)))
  (is (p/equals? (p/multiply root/Phi root/Phi) (p/add root/Phi 1)))
  (is (p/equals? (p/multiply root/phi root/phi) (p/add (p/negative root/phi) 1))))

(comment
  (require '[turtle-geometry.number.root-test] :reload)
  (in-ns 'turtle-geometry.number.root-test)
  (clojure.test/run-tests)
  (tc/quick-check 100 additive-inverse-root-prop)
  (tc/quick-check 100 multiplicative-inverse-root-prop)

  )
