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
  :ret (s/cat :ctx ::specs/bindings :form ::specs/form))

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

(declare eval-expr)

(defn eval-params [ctx params]
  (reduce
    (fn [[ctx old-res] param]
      (let [[ctx new-res] (eval-expr ctx param)]
        [ctx (conj old-res new-res)]))
    [ctx []] params))

(defn eval-expr [ctx form]
  (if-let [[form-type form-content] (s/conform ::specs/form form)]
    (case form-type
      :integer [ctx form-content]
      :closure [ctx form-content]
      :symbol
      (if (s/valid? ::specs/special-form form-content)
        [ctx form-content]
        (let [res (resolve-symbol ctx form-content)]
          [ctx res]))
      :list
      (let [[list-type list-content] form-content]
        (case list-type
          :nil [ctx nil]
          :cons
          (let [{:keys [head tail]} list-content
                [head-type head-content] head
                head (s/unform ::specs/form head)
                [ctx head] (eval-expr ctx head)
                [head-type head-content] (s/conform ::specs/form head)
                params (map (partial s/unform ::specs/form) tail)]
            (case head-type
              :symbol
              (case head-content
                quote
                (if (= 1 (count params))
                  [ctx (first params)]
                  (error-args params))
                lambda
                (let [{:keys [:arglist :body]}
                      (s/conform ::specs/lambda-expr params)
                      arglist (:symbols arglist)
                      body (s/unform ::specs/form body)]
                  [ctx
                   {::specs/bindings ctx
                    ::specs/arglist arglist
                    ::specs/form body}])
                macro
                (let [[ctx macro-expr] (eval-expr ctx (cons :lambda params))
                      macro-expr (assoc macro-expr ::specs/macro? true)]
                  (eval-expr ctx macro-expr))
                do
                (reduce
                  (fn [[ctx res] param] (eval-expr ctx param))
                  [ctx nil] params)
                if
                (let [{:keys [:check :then :else]}
                      (s/conform ::specs/if-expr params)
                      check-form (s/unform ::specs/form check)
                      then-form (s/unform ::specs/form then)
                      else-form (s/unform ::specs/form else)
                      [ctx check-res] (eval-expr ctx check-form)]
                  (if check-res
                    (eval-expr ctx else-form)
                    (eval-expr ctx then-form)))
                . (println "THIS IS AN INTEROP STATEMENT!!!")
                def
                (if (= 2 (count params))
                  (let [[sym expr] params
                        [new-ctx val] (eval-expr ctx expr)
                        new-ctx (cons {sym val} new-ctx)]
                    [new-ctx sym])
                  (error-args params))
                +
                (let [[ctx params] (eval-params ctx params)
                      res (clojure.core/apply + params)]
                  [ctx res])
                *
                (let [[ctx params] (eval-params ctx params)
                      res (clojure.core/apply * params)]
                  [ctx res]))
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
                    (let [[ctx params] (eval-params ctx params)
                          arg-ctx (zipmap arglist params)
                          ctx (cons arg-ctx bindings)
                          [ctx res] (eval-expr ctx form)
                          ctx (seq (rest ctx))]
                      [ctx res]))
                  (error-args params))))))))))

(comment
  ;; necessary special forms
  ;; somehow causes errors with #dbg
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
              (s/unform ::specs/form head))))))
    (error-args params))
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
              (map (partial s/unform ::specs/form) tail))))))
    (error-args params)))

(defn -main [& args]
  (println "Type your expression to be evaluated:")
  (loop [ctx [] line (read-line)]
    (if-let [expr (read-expr line)]
      (let [[ctx res] (eval-expr ctx expr)]
        (pprint res)
        (recur ctx (read-line))))))
