(ns myjvm.lisp
  (:require
   [clojure.test :refer :all]
   [clojure.java.io :as jio])
  (:import
   [java.io InputStream]))

;; CORE TYPES

(declare add-bindings lisp-eval)

(defn resolve-symbol
  [ctx name]
  (or (peek (get ctx name))
      (throw (ex-info (str "No bindings for symbol: " name) {}))))

(defn list-form-seq
  [form]
  (case (:type form)
    :list
    (lazy-seq
     (cons (:head form)
           (list-form-seq (:tail form))))
    :nil nil))

(defn push-binding
  [ctx name form]
  (let [bind-stack (get ctx name [])]
    (assoc ctx name (conj bind-stack form))))

(defn push-all-bindings
  [ctx bindings]
  (if-not (odd? (count bindings))
    (reduce (fn [m [sym expr]]
              (if-not (= :symbol (:type sym))
                (throw (ex-info "Cannot bind to anything but symbols") {})
                (push-binding m (:name sym) (lisp-eval ctx expr))))
            ctx
            (partition 2 bindings))
    (throw (ex-info "Bindings should be symbol/expression pairs" {}))))

(defn lisp-eval-fn
  [{:keys [ctx params body]} args]
  (if (= (count args)
         (count params))
    (let [bindings (interleave params args)
          local-ctx (push-all-bindings ctx bindings)]
      (lisp-eval local-ctx body))))

(defn lisp-eval
  [ctx form]
  (case (:type form)
    :symbol
    (resolve-symbol ctx (:name form))
    :list
    (let [head-form (:head form)]
      (if (and (= :symbol (:type head-form))
               (contains? #{"quote" "let" "if" "fn"} (:name head-form)))
        (case (:name head-form)
          "quote" (:tail form)
          "let"
          (let [tail-forms (list-form-seq (:tail form))]
            (if (= 2 (count tail-forms))
              (if-let [bindings (list-form-seq (first tail-forms))]
                (let [local-ctx (push-all-bindings ctx bindings)]
                  (lisp-eval local-ctx (second tail-forms)))
                (throw (ex-info "Bindings should be a list" {})))
              (throw (ex-info "Let statement has to have bindings and an expression." {}))))
          "if"
          (let [tail-forms (list-form-seq (:tail form))]
            (if (= 3 (count tail-forms))
              (if (not= :nil (:type (lisp-eval ctx (nth tail-forms 0))))
                (lisp-eval ctx (nth tail-forms 1))
                (lisp-eval ctx (nth tail-forms 2)))
              (throw (ex-info "If statement has to have 3 parts"))))
          "fn"
          (let [tail-forms (list-form-seq (:tail form))]
            (if (and (= 2 (count tail-forms))
                     (every? (fn [f] (= :symbol (:type f)))
                             (first tail-forms)))
              {:type :func
               :ctx ctx
               :params (first tail-forms)
               :body (second tail-forms)})))
        (let [evaled-hf (lisp-eval ctx head-form)]
          (case (:type evaled-hf)
            :func
            (let [arg-forms (list-form-seq (:tail form))]
              (lisp-eval-fn
               evaled-hf
               (vec (map #(lisp-eval ctx %) arg-forms))))
            :macro
            (let [arg-forms (list-form-seq (:tail form))
                  macro-expand-form (lisp-eval-fn evaled-hf arg-forms)]
              (lisp-eval ctx macro-expand-form))))))
    form))

(deftest test-lisp-eval
  (is (= {:type :int :value 1})
      (lisp-eval
       {}
       {:type :list
        :head {:type :symbol :name "let"}
        :tail {:type :list
               :head {:type :list
                      :head {:type :symbol :name "test"}
                      :tail {:type :list
                             :head {:type :int :value 1}
                             :tail {:type :list
                                    :head {:type :symbol :name "false"}
                                    :tail {:type :list
                                           :head {:type :nil}
                                           :tail {:type :nil}}}}}
               :tail {:type :list
                      :head {:type :list
                             :head {:type :symbol :name "if"}
                             :tail {:type :list
                                    :head {:type :symbol :name "false"}
                                    :tail {:type :list
                                           :head {:type :symbol :name "test"}
                                           :tail {:type :list
                                                  :head {:type :int :value -1}
                                                  :tail {:type :nil}}}}}
                      :tail {:type :nil}}}})))








;;===========================

(defmacro update!
  [what f & args]
  (list 'set! what (apply list f what args)))

(defprotocol Reader
  (read-char [reader]))

(deftype StringReader
    [^String s s-len ^:unsynchronized-mutable s-pos]
  Reader
  (read-char [reader]
    (when (> s-len s-pos)
      (let [ch (nth s s-pos)]
        (update! s-pos inc)
        ch))))

(deftype InputStreamReader
    [^InputStream is]
  Reader
  (read-char [reader]
    (let [ch (.read is)]
      (when (>= ch 0)
        (char ch)))))

(defn string-reader
  [s]
  (->StringReader s (count s) 0))

(defn input-stream-reader
  [is]
  (->InputStreamReader is))

;;=========================

(defn whitespace?
  [^Character ch]
  (or (Character/isWhitespace ch)
      (identical? \, ch)))

(defn parse-next-form
  [chars]
  ())

(defn read-form
  [rdr]
  (when-let [ch (read-char rdr)]))

(defn read-seq
  [rdr]
  (lazy-seq
   (when-let [form (read-form rdr)]
     (cons form (read-seq rdr)))))

(deftest test-read-seq
  (testing "parses integer literal"
    (are [input values] (= (for [v values] {:type :int :value v})
                           (read-seq (string-reader input)))
         "13" [13]
         "-1" [-1]
         "-0" [0]
         "2147483647" [Integer/MAX_VALUE]
         "-2147483648" [Integer/MIN_VALUE]
         "1 2 3" [1 2 3])))
