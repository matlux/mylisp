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

(s/def ::vararg ::symbol)

(s/def ::closure
  (s/keys
    :req [::bindings ::arglist ::form]
    :opt [::macro? ::vararg]))

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
  (set '[def quote lambda macro if do eval apply = + - * / cons list first rest]))

(s/def ::vararg-separator #{'&})

(defn antispec [spec]
  #(not (s/valid? spec %)))

(s/def ::lambda-expr
  (s/cat
    :arglist
    (s/spec
      (s/cat
        :required
        (s/* (s/and ::symbol (antispec ::vararg-separator)))
        :vararg
        (s/?
          (s/cat
            :separator ::vararg-separator
            :symbol ::symbol))))
    :body ::form))

(s/def ::if-expr
  (s/cat
    :check ::form
    :then ::form
    :else ::form))
