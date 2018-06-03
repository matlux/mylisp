(ns mylisp.core-test
  (:require [mylisp.core :as core]
            [clojure.test :refer :all])
  (:import [clojure.lang ExceptionInfo]))

(comment
  (eval-expr nil '(* (+ 2 3) (+ 1 2 3)))
  (eval-expr nil '((lambda (x) (+ 1 x)) 4))
  (eval-expr nil '(do (def inc (lambda (x) (+ 1 x))) (inc 4)))
  )

(defmacro unbound-sym [sym]
  `(re-pattern (str "Unbound symbol: " ~(name sym))))

(deftest test-eval
  (testing "Special forms should work as expected"
    (testing ">> atoms evaluate to themselves"
      ;; integer
      (is (= (core/eval-expr nil 2)
            [nil 2]))
      (let [ctx [{'a 4}]]
        (is (= (core/eval-expr ctx 'a)
              [ctx 4]))))
    (testing ">> integer arithmetic should be supported"
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
    (testing ">> lambda expressions should capture closures from contexts"
      (is (= (core/eval-expr nil '((lambda (x) (+ 1 x)) 4))
            [nil 5]))
      (let [ctx [{'y 4}]]
        (is (= (core/eval-expr ctx '[[lambda [x] [+ [* x x] y]] 5])
              [ctx 29])))
      (is (= (core/eval-expr nil '[[[lambda [y] [lambda [x] [+ [* x x] y]]] 4] 5])
            [nil 29])))
    (testing ">> if expressions work on arithmetics"
      (is (= (core/eval-expr nil '(if (= 2 (- 4 2)) 42 666))
            [nil 42]))
      (is (= (core/eval-expr nil '(if (= (* (+ 2 4) (- 9 1)) (* 6 6)) 12 42))
            [nil 42])))
    (testing ">> lambda expressions with varargs is supported"
      (is (= [nil [1 2 3 4 5]]
            (core/eval-expr nil
              '((lambda (head & tail) (cons head tail)) 1 2 3 4 (+ 2 3)))))
      (is (= [nil 6]
            (core/eval-expr nil '((lambda (& args) (apply + args)) 1 2 3)))))
    (testing ">> quote stops evaluation of expression"
      (is (= [nil '[+ 1 2 3]] (core/eval-expr nil '[quote [+ 1 2 3]]))))
    (testing ">> List manipulation forms (cons, car, cdr) should be supported"
      (is (= [nil [1 2 3 4]] (core/eval-expr nil '[cons 1 [quote [2 3 4]]])))
      (is (= [nil 1] (core/eval-expr nil '[car [quote [1 2 3 4]]])))
      (is (= [nil [2 3 4]] (core/eval-expr nil '[cdr [quote [1 2 3 4]]])))
      (is (= [nil nil] (core/eval-expr nil '[car 42])))
      (is (= [nil nil] (core/eval-expr nil '[car nil])))
      (is (= [nil 1] (core/eval-expr nil '[car [cons 1 nil]])))
      (is (= [nil nil] (core/eval-expr nil '[cdr 42])))
      (is (= [nil nil] (core/eval-expr nil '[cdr nil])))
      (is (= [nil nil] (core/eval-expr nil '[cdr [cons 1 nil]])))
      (is (= [nil 1] (core/eval-expr nil '[car [cons 1 [quote [2 3 4]]]])))
      (is (= [nil [2 3 4]] (core/eval-expr nil '[cdr [cons 1 [quote [2 3 4]]]]))))
    (testing ">> def statements should return update environment"
      (is (= (core/eval-expr nil '[def x 4])
            '[[{x 4 }] x])))
    (testing ">> eval uses eager evaluation"
      (core/eval-expr nil '[eval [cons + [quote [1 2 3]]]]))
    ;; check unbound variable throws error
    (testing ">> referencing undefined variable throws exception"
      (is (thrown-with-msg? ExceptionInfo (unbound-sym y)
            (core/eval-expr nil 'y)))
      (is (thrown-with-msg? ExceptionInfo (unbound-sym y)
            (core/eval-expr nil '((lambda (x) (+ x y)) 5))))
      (is (thrown-with-msg? ExceptionInfo (unbound-sym z)
            (core/eval-expr nil
              '((((lambda (f) (lambda (y z) (f y)))
                  ((lambda (x) (lambda (y) (+ x y z))) 1)) 2 3)))))
      #_
      (testing ">> even while creating new closures"
        (is (thrown-with-msg? ExceptionInfo (unbound-sym z)
              (core/eval-expr {'x 1} '(lambda (y) (+ x y z)))))))
    (testing ">> do statements should monadicaly propagate context changes"
      (is (= (core/eval-expr nil '(do (def x 4) (def y 5)))
            '[[{y 5} {x 4}] y]))
      (is (= (core/eval-expr nil '(do (def inc (lambda (x) (+ 1 x))) (inc 4)))
            (let [[inc-ctx inc-closure] (core/eval-expr nil '(lambda (x) (+ 1 x)))
                  expected-ctx (list {'inc inc-closure})]
              [expected-ctx 5]))))))

(def recursion-forms
  (quote
    [[def Y
      [lambda [f]
       [[lambda [x] [f [x x]]]
        [lambda [x] [f [lambda [& args] [apply [x x] args]]]]]]]

     [def fact*
      (lambda [recurse]
        [lambda [n]
         [if [= n 0] 1 [* n [recurse [- n 1]]]]])]

     [def fact [Y fact*]]]))

(deftest test-recursion
  (let [[ctx _] (core/eval-expr nil (cons 'do recursion-forms))]
    (is (= [ctx 1] (core/eval-expr ctx '(fact 0))))
    (is (= [ctx 120] (core/eval-expr ctx '(fact 5))))))
