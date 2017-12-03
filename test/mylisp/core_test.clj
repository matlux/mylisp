(ns mylisp.core-test
  (:require [mylisp.core :as core]
            [clojure.test :refer :all]))

(deftest test-stuff
  (testing "The following examples should pass"
    (is (= 29 (core/eval [{:y 4}] [[:lambda [:x] [:+ [:* :x :x] :y]] 5])))))
