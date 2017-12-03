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
                      (s/conform ::specs/lambda params)
                      arglist (:symbols arglist)
                      body (s/unform ::specs/form body)]
                  {::specs/bindings ctx
                   ::specs/arglist arglist
                   ::specs/form body}) 
                :if "THIS IS AN IF STATEMENT"
                :do "THIS IS A DO STATEMENT"
                :. "THIS IS AN INTEROP STATEMENT"
                :+
                (let [params (map #(eval ctx %) params)]
                  (clojure.core/apply + params))
                :*
                (let [params (map #(eval ctx %) params)]
                  (clojure.core/apply * params)))
              :closure
              (let [{:keys [::specs/bindings ::specs/arglist ::specs/form]}
                    (s/unform ::specs/closure head-content)]
                (if (= (count arglist) (count params))
                  (let [arg-ctx (zipmap arglist params)]
                    (eval (cons arg-ctx bindings) form)))))))))))

