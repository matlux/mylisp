(ns mylisp.specs
  (:refer-clojure :exclude [eval apply resolve])
  (:require
   [clojure.edn :as edn]
   [clojure.spec.alpha :as s]))


(s/def ::symbol keyword?)
(s/def ::integer integer?)

(s/def ::bindings
  (s/* (s/map-of ::symbol ::form)))

;(s/def ::fn-context ::bindings)
(s/def ::arglist (s/* ::symbol))
;(s/def ::fn-body (s/+ ::form))

(s/def ::closure
  (s/keys :req [::bindings ::arglist ::form]))

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
  #{:def :quote :lambda :apply :let :if :do :+ :*})

(s/def ::lambda
  (s/cat
    :name (s/? ::symbol)
    :arglist (s/spec (s/cat :symbols (s/* ::symbol)))
    :body ::form))

