(ns mylisp.specs
  (:refer-clojure :exclude [eval apply resolve])
  (:require
   [clojure.edn :as edn]
   [clojure.spec.alpha :as s]))


(s/def ::symbol symbol?)
(s/def ::integer integer?)
(s/def ::macro? boolean?)

(s/def ::bindings
  (s/* (s/map-of ::symbol ::form)))

;(s/def ::fn-context ::bindings)
(s/def ::arglist (s/* ::symbol))
;(s/def ::fn-body (s/+ ::form))

(s/def ::closure
  (s/keys
    :req [::bindings ::arglist ::form]
    :opt [::macro?]))

(s/def ::list
  (s/or
    :nil (comp nil? seq)
    :cons (s/cat :head ::form :tail (s/* ::form))))

(s/def ::form
  (s/or
    :symbol ::symbol
    :integer ::integer
    :closure ::closure
    :list ::list))

(s/def ::special-form
  (set '[def quote lambda macro if do = + - * / cons car cdr]))

(s/def ::lambda-expr
  (s/cat
    :arglist (s/spec (s/* ::symbol))
    :body ::form))

(s/def ::if-expr
  (s/cat
    :check ::form
    :then ::form
    :else ::form))
