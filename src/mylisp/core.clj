(ns mylisp.core
  (:refer-clojure :exclude [read print eval resolve])
  (:require
   [clojure.spec.alpha :as s]
   [clojure.edn :as edn]
   [mylisp.specs :as specs]))

(s/fdef ::read
  :args (s/cat :args string?)
  :ret ::specs/form)

(s/fdef ::resolve
  :args (s/cat :ctx ::specs/bindings :symbol ::specs/symbol)
  :ret ::specs/form)

(s/fdef ::eval
  :args (s/cat :ctx ::specs/bindings :form ::specs/form)
  :ret ::specs/form)

(defn read [s]
  (edn/read-string s))

(defn resolve [ctx sym]
  (if (seq ctx)
    (if-let [res (get (first ctx) sym)]
      res (resolve (rest ctx) sym))
    (throw (ex-info (str "Unbound symbol: " (name sym)) {:symbol sym}))))

(defn error-args [n]
  (throw (ex-info (str "Wrong number of arguments (" n ")"))))

(defn eval [ctx form]
  (if-let [[form-type form-content] (s/conform ::specs/form form)]
    (case form-type
      :integer form-content
      :symbol
      (if (s/valid? ::specs/special-form form-content)
        form-content
        (resolve ctx form-content))
      :closure form-content
      :list
      (let [[list-type list-content] form-content]
        (case list-type
          :nil nil
          :cons
          (let [{:keys [head tail]} list-content
                [head-type head-content] head
                head (s/unform ::specs/form head)
                head (eval ctx head)
                [head-type head-content] (s/conform ::specs/form head)
                params (map (partial s/unform ::specs/form) tail)]
            (case head-type
              :symbol
              (case head-content
                :quote
                (let [param-count (count params)]
                  (if (= 1 param-count)
                    (first params)
                    (error-args param-count)))
                :lambda
                (let [{:keys [:name :arglist :body]}
                      (s/conform ::specs/lambda-expr params)
                      arglist (:symbols arglist)
                      body (map #(s/unform ::specs/form %) body)]
                  {::specs/bindings ctx
                   ::specs/arglist arglist
                   ::specs/form (cons :do body)})
                :macro
                (let [macro-expr (eval ctx (cons :lambda params))
                      macro-expr (assoc macro-expr ::specs/macro? true)]
                  (eval ctx macro-expr))
                :do (last (for [param params] (eval ctx param)))
                :if
                (let [{:keys [:check :then :else]}
                      (s/conform ::specs/if-expr params)
                      check (eval ctx (s/unform ::specs/form check))
                      [check-type check-content] (s/conform ::specs/form check)]
                  (case check-type
                    :integer
                    (if (zero? check-content)
                      (eval ctx (s/unform ::specs/form else))
                      (eval ctx (s/unform ::specs/form then)))
                    :list
                    (let [[list-type list-content] check-content]
                      (case list-type
                        :nil (eval ctx (s/unform ::specs/form else))
                        :cons (eval ctx (s/unform ::specs/form then))))))
                :. "THIS IS AN INTEROP STATEMENT"
                :cons
                (let [params (map #(eval ctx %) params)
                      {:keys [head tail]}
                      (s/conform (s/cat :head ::specs/form :tail ::specs/form) params)
                      head (s/unform ::specs/form head)
                      head (eval ctx head)
                      tail (s/unform ::specs/form tail)
                      tail (map #(eval ctx %) tail)]
                  (cons head tail))
                :car
                (if (= 1 (count params))
                  (let [param (eval ctx (first params))
                        [param-type param-content] (s/conform ::specs/form param)]
                    (case param-type
                      :list
                      (let [[list-type list-content] param-content]
                        (case list-type
                          :nil nil
                          :cons
                          (let [{:keys [head tail]} list-content]
                            (s/unform ::specs/form head)))))))
                :cdr
                (if (= 1 (count params))
                  (let [param (eval ctx (first params))
                        [param-type param-content] (s/conform ::specs/form param)]
                    (case param-type
                      :list
                      (let [[list-type list-content] param-content]
                        (case list-type
                          :nil nil
                          :cons
                          (let [{:keys [head tail]} list-content]
                            (map #(s/unform ::specs/form %) tail)))))))
                :+
                (let [params (map #(eval ctx %) params)]
                  (clojure.core/apply + params))
                :*
                (let [params (map #(eval ctx %) params)]
                  (clojure.core/apply * params)))
              :closure
              (let [{:keys [::specs/bindings ::specs/arglist ::specs/form ::specs/macro?]}
                    (s/unform ::specs/closure head-content)]
                (if (= (count arglist) (count params))
                  (if macro?
                    (let [arg-ctx (zipmap arglist params)]
                      (eval (cons arg-ctx bindings) form))
                    (let [params (map #(eval ctx %) params)
                          arg-ctx (zipmap arglist params)]
                      (eval (cons arg-ctx bindings) form))))))))))))

