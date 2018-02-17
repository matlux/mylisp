(ns mylisp.core
  (:require
   [clojure.spec.alpha :as s]
   [clojure.edn :as edn]
   [clojure.pprint :refer [pprint]]
   [mylisp.specs :as specs])
  (:gen-class))

(s/fdef ::read-expr
  :args (s/cat :args string?)
  :ret ::specs/form)

(s/fdef ::resolve-symbol
  :args (s/cat :ctx ::specs/bindings :symbol ::specs/symbol)
  :ret ::specs/form)

(s/fdef ::eval-expr
  :args (s/cat :ctx ::specs/bindings :form ::specs/form)
  :ret ::specs/form)

(defn read-expr [s]
  (try
    (edn/read-string s)
    (catch Exception ex
      (println (.getMessage ex)))))

(defn resolve-symbol [ctx sym]
  (if (seq ctx)
    (if-let [res (get (first ctx) sym)]
      res (resolve-symbol (rest ctx) sym))
    (throw (ex-info (str "Unbound symbol: " (name sym)) {:symbol sym}))))

(defn error-args [n]
  (throw (ex-info (str "Wrong number of arguments (" n ")"))))

(defn eval-expr [ctx form]
  (if-let [[form-type form-content] (s/conform ::specs/form form)]
    (case form-type
      :integer form-content
      :symbol
      (if (s/valid? ::specs/special-form form-content)
        form-content
        (resolve-symbol ctx form-content))
      :closure form-content
      :list
      (let [[list-type list-content] form-content]
        (case list-type
          :nil nil
          :cons
          (let [{:keys [head tail]} list-content
                [head-type head-content] head
                head (s/unform ::specs/form head)
                head (eval-expr ctx head)
                [head-type head-content] (s/conform ::specs/form head)
                params (map (partial s/unform ::specs/form) tail)]
            (case head-type
              :symbol
              (case head-content
                quote
                (let [param-count (count params)]
                  (if (= 1 param-count)
                    (first params)
                    (error-args param-count)))
                lambda
                (let [{:keys [:arglist :body]}
                      (s/conform ::specs/lambda-expr params)
                      arglist (:symbols arglist)
                      body (map (partial s/unform ::specs/form) body)]
                  {::specs/bindings ctx
                   ::specs/arglist arglist
                   ::specs/form body})
                macro
                (let [macro-expr (eval-expr ctx (cons :lambda params))
                      macro-expr (assoc macro-expr ::specs/macro? true)]
                  (eval-expr ctx macro-expr))
                do (last (for [param params] (eval-expr ctx param)))
                if
                (let [{:keys [:check :then :else]}
                      (s/conform ::specs/if-expr params)
                      check (eval-expr ctx (s/unform ::specs/form check))
                      [check-type check-content] (s/conform ::specs/form check)]
                  (case check-type
                    :integer
                    (if (zero? check-content)
                      (eval-expr ctx (s/unform ::specs/form else))
                      (eval-expr ctx (s/unform ::specs/form then)))
                    :list
                    (let [[list-type list-content] check-content]
                      (case list-type
                        :nil (eval-expr ctx (s/unform ::specs/form else))
                        :cons (eval-expr ctx (s/unform ::specs/form then))))))
                :. "THIS IS AN INTEROP STATEMENT"
                cons
                (let [params (map (partial eval-expr ctx) params)
                      {:keys [head tail]}
                      (s/conform (s/cat :head ::specs/form :tail ::specs/form) params)
                      head (s/unform ::specs/form head)
                      head (eval-expr ctx head)
                      tail (s/unform ::specs/form tail)
                      tail (map (partial eval-expr ctx) tail)]
                  (cons head tail))
                car
                (if (= 1 (count params))
                  (let [param (eval-expr ctx (first params))
                        [param-type param-content] (s/conform ::specs/form param)]
                    (case param-type
                      :list
                      (let [[list-type list-content] param-content]
                        (case list-type
                          :nil nil
                          :cons
                          (let [{:keys [head tail]} list-content]
                            (s/unform ::specs/form head)))))))
                cdr
                (if (= 1 (count params))
                  (let [param (eval-expr ctx (first params))
                        [param-type param-content] (s/conform ::specs/form param)]
                    (case param-type
                      :list
                      (let [[list-type list-content] param-content]
                        (case list-type
                          :nil nil
                          :cons
                          (let [{:keys [head tail]} list-content]
                            (map (partial s/unform ::specs/form) tail)))))))
                +
                (let [params (map (partial eval-expr ctx) params)]
                  (clojure.core/apply + params))
                *
                (let [params (map #(eval-expr ctx %) params)]
                  (clojure.core/apply * params)))
              :closure
              (let [{:keys
                     [::specs/bindings
                      ::specs/arglist
                      ::specs/form
                      ::specs/macro?]}
                    (s/unform ::specs/closure head-content)]
                (if (= (count arglist) (count params))
                  (if macro?
                    (let [arg-ctx (zipmap arglist params)
                          ctx (cons arg-ctx bindings)]
                      (eval-expr ctx form))
                    (let [params (map (partial eval-expr ctx) params)
                          arg-ctx (zipmap arglist params)
                          ctx (cons arg-ctx bindings)]
                      (eval-expr ctx form))))))))))))

(defn -main [& args]
  (println "Type your expression to be evaluated:")
  (loop [ctx [] line (read-line)]
    (if-let [expr (read-expr line)]
      (let [[ctx res] (eval-expr ctx expr)]
        (pprint res)
        (recur ctx (read-line))))))
