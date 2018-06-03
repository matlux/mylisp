(ns mylisp.core
  (:require
   [clojure.java.io :as jio]
   [clojure.string :as str]
   [clojure.spec.alpha :as s]
   [clojure.edn :as edn]
   [clojure.pprint :refer [pprint]]
   [mylisp.specs :as specs])
  (:import
   [java.io PushbackReader])
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

(defn resolve-symbol [ctx sym]
  (if (seq ctx)
    (if-let [res (get (first ctx) sym)]
      res (resolve-symbol (rest ctx) sym))
    (throw (ex-info (str "Unbound symbol: " (name sym)) {:symbol sym}))))

(defn error-args [apply args]
  (throw (ex-info (str "Wrong number of arguments (" (count args) ")")
           {:apply apply :args args})))

(declare eval-expr)

(defn eval-params [ctx params]
  (reduce
    (fn [[old-ctx acc] param]
      (let [[new-ctx res] (eval-expr old-ctx param)]
        [new-ctx (conj acc res)]))
    [ctx []]
    params))

(defn conform [spec form]
  (if (s/valid? spec form)
    (s/conform spec form)
    (throw
      (ex-info "Invalid form!"
        (s/explain-data spec form)))))

(defn eval-expr [ctx form]
  (if-let [[form-type form-content] (conform ::specs/form form)]
    (condp = form-type
      :integer [ctx form-content]
      :closure [ctx (s/unform ::specs/closure form-content)]
      :symbol
      (if (s/valid? ::specs/special-form form-content)
        [ctx form-content]
        (let [res (resolve-symbol ctx form-content)]
          [ctx res]))
      :list
      (let [[list-type list-content] form-content]
        (condp = list-type
          :nil [ctx nil]
          :cons
          (let [{:keys [head tail]} list-content
                [head-type head-content] head
                head (s/unform ::specs/form head)
                [ctx head] (eval-expr ctx head)
                [head-type head-content] (conform ::specs/form head)
                params (map (partial s/unform ::specs/form) tail)]
            (condp = head-type
              :symbol
              (condp = head-content
                'quote
                (if (= 1 (count params))
                  [ctx (first params)]
                  (error-args "quote" params))
                'eval
                (if (= 1 (count params))
                  (let [[ctx [expr]] (eval-params ctx params)]
                    (eval-expr ctx expr))
                  (error-args "eval" params))
                'apply
                (if (= 2 (count params))
                  (let [[ctx params] (eval-params ctx params)
                        [f args] params]
                    (eval-expr ctx (cons f args)))
                  (error-args "apply" params))
                'lambda
                (let [{:keys [arglist body]}
                      (conform ::specs/lambda-expr params)
                      {:keys [required vararg]} arglist
                      {vararg :symbol} vararg
                      form (s/unform ::specs/form body)
                      closure
                      {::specs/bindings ctx
                       ::specs/arglist required
                       ::specs/form form}]
                  (if vararg
                    [ctx
                     (assoc closure
                       ::specs/vararg
                       vararg)]
                    [ctx closure]))
                'macro
                (let [[ctx macro-expr] (eval-expr ctx (cons 'lambda params))
                      macro-expr (assoc macro-expr ::specs/macro? true)]
                  [ctx macro-expr])
                'do
                (reduce
                  (fn [[ctx res] param] (eval-expr ctx param))
                  [ctx nil] params)
                'if
                (if (<= 2 (count params) 3)
                  (let [{:keys [:check :then :else]}
                        (conform ::specs/if-expr params)
                        check-form (s/unform ::specs/form check)
                        then-form (s/unform ::specs/form then)
                        else-form (s/unform ::specs/form else)
                        [ctx check-res] (eval-expr ctx check-form)]
                    (if check-res
                      (eval-expr ctx then-form)
                      (eval-expr ctx else-form)))
                  (error-args "if" params))
                'def
                (if (= 2 (count params))
                  (let [[sym expr] params
                        [new-ctx val] (eval-expr ctx expr)
                        new-ctx (cons {sym val} new-ctx)]
                    [new-ctx sym])
                  (error-args "=" params))
                'cons
                (if (= 2 (count params))
                  (let [[ctx [head tail]] (eval-params ctx params)
                        res (cons head tail)]
                    [ctx res])
                  (error-args "cons" params))
                'car
                (if (= 1 (count params))
                  (let [[ctx param] (eval-expr ctx (first params))
                        [param-type param-content] (conform ::specs/form param)]
                    (condp = param-type
                      :list
                      (let [[list-type list-content] param-content]
                        (case list-type
                          :nil [ctx nil]
                          :cons
                          (let [{:keys [head tail]} list-content
                                res (s/unform ::specs/form head)]
                            [ctx res])))
                      [ctx nil]))
                  (error-args "car" params))
                'cdr
                (if (= 1 (count params))
                  (let [[ctx param] (eval-expr ctx (first params))
                        [param-type param-content] (conform ::specs/form param)]
                    (case param-type
                      :list
                      (let [[list-type list-content] param-content]
                        (condp = list-type
                          :nil [ctx nil]
                          :cons
                          (let [{:keys [head tail]} list-content
                                res (map (partial s/unform ::specs/form) tail)]
                            [ctx (seq res)])))
                      [ctx nil]))
                  (error-args "cdr" params))
                '=
                (let [[ctx params] (eval-params ctx params)]
                  (if (clojure.core/apply = params)
                    [ctx true]
                    [ctx false]))
                '+
                (let [[ctx params] (eval-params ctx params)
                      res (clojure.core/apply + params)]
                  [ctx res])
                '-
                (let [[ctx params] (eval-params ctx params)
                      res (clojure.core/apply - params)]
                  [ctx res])
                '*
                (let [[ctx params] (eval-params ctx params)
                      res (clojure.core/apply * params)]
                  [ctx res])
                '/
                (let [[ctx params] (eval-params ctx params)
                      res (clojure.core/apply / params)]
                  [ctx res]))
              :closure
              (let [{:keys
                     [::specs/bindings
                      ::specs/arglist
                      ::specs/vararg
                      ::specs/form
                      ::specs/macro?]}
                    (s/unform ::specs/closure head-content)]
                (if (or (= (count arglist) (count params))
                      (and vararg (< (count arglist) (count params))))
                  (if macro?
                    (let [arg-ctx (zipmap arglist params)
                          arg-ctx
                          (if vararg
                            (assoc arg-ctx vararg
                              (drop (count arglist) params))
                            arg-ctx)
                          arg-ctx (cons arg-ctx bindings)
                          [res-ctx res] (eval-expr arg-ctx form)]
                      (eval-expr ctx res))
                    (let [[ctx params] (eval-params ctx params)
                          arg-ctx (zipmap arglist params)
                          arg-ctx
                          (if vararg
                            (assoc arg-ctx vararg
                              (drop (count arglist) params))
                            arg-ctx)
                          arg-ctx (cons arg-ctx bindings)
                          [result-ctx res] (eval-expr arg-ctx form)]
                      [ctx res]))
                  (error-args "closure" params))))))))))

(defn read-expr [s]
  (try
    (edn/read-string s)
    (catch Exception ex
      (println (.getMessage ex))
      nil)))

(defn try-eval-expr [ctx expr]
  (try
    (eval-expr ctx expr)
    (catch Exception e
      (println "ERROR:"(.getMessage e) (ex-data e))
      [ctx nil])))

(defn read-edn [filename]
  (let [rdr (PushbackReader. (jio/reader (jio/resource filename)))
        forms (edn/read rdr)]
    (.close rdr)
    forms))

(defn -main [& args]
  (let [init-forms (read-edn "init.edn")
       [ctx res] (eval-expr nil (cons 'do init-forms))]
    (println "REPL is ready! Type an expression to be evaluated:")
    (loop [ctx ctx
           line (read-line)]
      (if-let [expr (read-expr line)]
        (let [[new-ctx res] (try-eval-expr ctx expr)]
          (println "=>" (pr-str res))
          (println)
          (println "Type another expression:")
          (recur new-ctx (read-line)))
        (do
          (println)
          (println "Type another expression:")
          (recur ctx (read-line)))))))
