(ns mylisp.core-test
  (:require [mylisp.core :as core]
            [clojure.test :refer :all]))

(comment
  (eval-expr nil '(* (+ 2 3) (+ 1 2 3)))
  (eval-expr nil '((lambda (x) (+ 1 x)) 4))
  (eval-expr nil '(do (def inc (lambda (x) (+ 1 x))) (inc 4)))
  )


(deftest test-eval
  (testing "Special forms should work as expected -"
    (testing "Atoms evaluate to themselves"
      ;; integer
      (is (= (core/eval-expr nil 2)
            [nil 2]))
      (let [ctx [{'a 4}]]
        (is (= (core/eval-expr ctx 'a)
              [ctx 4]))))
    (testing "Integer arithmetic should be supported"
      (is (= [nil 4] (core/eval-expr nil '[+ 1 3])))
      (is (= [nil 20] (core/eval-expr nil '[* 4 5])))
      (is (= [nil 29] (core/eval-expr nil '[+ [* 4 5] 9])))
      ;; evaluating nested expressions
      (is (= [nil 30] (core/eval-expr nil '(* (+ 2 3) (+ 1 2 3)))))
      ;; lookup from context
      (let [ctx [{'a 4}]]
        (is (= [ctx 9] (core/eval-expr ctx '[+ a 5]))))
      ;; advanced lookup from context
      (let [ctx [{'a 4 'b 5}]]
        (is (= [ctx 21] (core/eval-expr ctx '[+ [* a a] b])))))
    (testing "Lambda expressions should capture closures from contexts"
      (is (= (core/eval-expr nil '((lambda (x) (+ 1 x)) 4))
            [nil 5]))
      (let [ctx [{'y 4}]]
        (is (= (core/eval-expr ctx '[[lambda [x] [+ [* x x] y]] 5])
              [ctx 29])))
      (is (= (core/eval-expr [] '[[[lambda [y] [lambda [x] [+ [* x x] y]]] 4] 5])
             [nil 29])))
    (testing "Quote stopes evaluation of expression"
      (is (= [nil '[+ 1 2 3]] (core/eval-expr nil '[quote [+ 1 2 3]]))))
    #_
    (testing "List manipulation forms (cons, car, cdr) should be supported"
      (is (= [1 2 3 4] (core/eval-expr nil '[cons 1 [quote [2 3 4]]])))
      (is (= 1 (core/eval-expr nil '[car [quote [1 2 3 4]]])))
      (is (= [2 3 4]) (core/eval-expr nil '[cdr [quote [1 2 3 4]]]))
      (is (= 1 (core/eval-expr nil '[car [cons 1 [quote [2 3 4]]]])))
      (is (= [2 3 4] (core/eval-expr nil '[cdr [cons 1 [quote [2 3 4]]]]))))
    (testing "Def statements should return update environment"
      (is (= (core/eval-expr nil '[def x 4])
            '[[{x 4 }] x])))
    (testing "'Do' statements should monadicaly propagate context changes"
      (is (= (core/eval-expr nil '(do (def x 4) (def y 5)))
            '[[{y 5} {x 4}] y]))
      (is (= (core/eval-expr nil '(do (def inc (lambda (x) (+ 1 x))) (inc 4)))
            [nil 5])))))
