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
  [ctx binding-form]
  (if-let [bindings (list-form-seq binding-form)]
    (if-not (odd? (count bindings))
      (reduce (fn [m [sym expr]]
                (if-not (= :symbol (:type sym))
                  (throw (ex-info "Cannot bind to anything but symbols") {})
                  (push-binding m (:name sym) (lisp-eval ctx expr))))
              ctx
              (partition 2 bindings))
      (throw (ex-info "Bindings should be symbol/expression pairs" {})))
    (throw (ex-info "Binding should be a list" {}))))

(defn lisp-eval
  [ctx form]
  (case (:type form)
    :symbol
    (resolve-symbol ctx (:name form))
    :list
    (let [head-form (:head form)]
      (if (= :symbol (:type head-form))
        (case (:name head-form)
          "quote" (:tail form)
          "let"
          (let [tail-forms (list-form-seq (:tail form))]
            (if (= 2 (count tail-forms))
              (let [new-ctx (push-all-bindings ctx (first tail-forms))]
                (lisp-eval new-ctx (second tail-forms)))
              (throw (ex-info "Let statement has to have bindings and an expression." {}))))
          "if" (throw (ex-info "Unimplemented" {}))
          "fn" (throw (ex-info "Unimplemented" {})))))
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
                             :tail {:type :nil}}}
               :tail {:type :list
                      :head {:type :symbol :name "test"}
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
