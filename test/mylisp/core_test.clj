(ns mylisp.core-test
  (:require [mylisp.core :as core]
            [clojure.test :refer :all]))

(deftest test-eval
  (testing "Special forms should work as expected -"
    (testing "Integer arithmetic should be supported"
      (is (= 4 (core/eval [] [:+ 1 3])))
      (is (= 20 (core/eval [] [:* 4 5])))
      (is (= 29 (core/eval [] [:+ [:* 4 5] 9])))
      (is (= 9 (core/eval [{:a 4}] [:+ :a 5])))
      (is (= 21 (core/eval [{:a 4 :b 5}] [:+ [:* :a :a] :b]))))
    (testing "Lambda expressions should capture closures from contexts"
      (is (= 29 (core/eval [{:y 4}] [[:lambda [:x] [:+ [:* :x :x] :y]] 5])))
      (is (= 29 (core/eval [] [[[:lambda [:y] [:lambda [:x] [:+ [:* :x :x] :y]]] 4] 5]))))
    (testing "Quote stopes evaluation of expression"
      (is (= [:+ 1 2 3] (core/eval [] [:quote [:+ 1 2 3]]))))
    (testing "List manipulation forms (cons, car, cdr) should be supported"
      (is (= [1 2 3 4] (core/eval [] [:cons 1 [:quote [2 3 4]]])))
      (is (= 1 (core/eval [] [:car [:quote [1 2 3 4]]])))
      (is (= [2 3 4]) (core/eval [] [:cdr [:quote [1 2 3 4]]]))
      (is (= 1 (core/eval [] [:car [:cons 1 [:quote [2 3 4]]]])))
      (is (= [2 3 4] (core/eval [] [:cdr [:cons 1 [:quote [2 3 4]]]]))))))
