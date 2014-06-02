(ns myjvm.reader
  (:import
   [java.io InputStream]))

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


;; UTIL FUNCTIONS

(defn whitespace?
  [^Character ch]
  (or (Character/isWhitespace ch)
      (identical? \, ch)))

(defn digit?
  [^Character ch]
  (Character/isDigit ch))

(defn sign?
  [^Character ch]
  (if (or (= \+ ch) (= \- ch))
    true false))

;; READER DISPATCH

(declare dispatch-context
         read-comment-context)

(defn read-whitespace-context
  [rdr forms]
  (if-let [ch (read-char rdr)]
    (if-not (whitespace? ch)
      (dispatch-context rdr forms ch)
      (recur rdr forms))))

(defn read-comment-context
  [rdr forms]
  (if-let [ch (read-char rdr)]
    (if (= \newline ch)
      (dispatch-context rdr forms (read-char rdr))
      (recur rdr forms))))

(defn read-numeric-context
  [rdr forms buff]
  (let [ch (read-char rdr)]
    (cond
     (digit? ch)
     (recur rdr forms (conj buff ch))
     
     (or (whitespace? ch) (nil? ch))
     (let [int-val (Integer/parseInt (apply str buff))
           forms (conj forms {:type :int :value int-val})]
       (dispatch-context rdr forms ch)))))

(defn read-sign-context
  [rdr forms buff]
  (let [ch (read-char rdr)]
    (cond
     (digit? ch) (read-numeric-context rdr forms (conj buff ch)))))

(defn dispatch-context
  [rdr forms initch]
  (cond
   (nil? initch) forms
   (whitespace? initch) (recur rdr forms (read-char rdr))
   (= \; initch) (read-comment-context rdr forms initch)
   (digit? initch) (read-integer-context rdr [initch])
   (sign? initch) (read-sign-context rdr forms [initch])))

(defn read-form
  [rdr])

(defn form-seq
  [rdr]
  (lazy-seq
   (when-let [form (read-form rdr)]
     (cons form (form-seq rdr)))))

(deftest test-form-seq
  (testing "parses integer literal"
    (are [input values] (= (for [v values] {:type :int :value v})
                           (form-seq (string-reader input)))
         "13" [13]
         "-1" [-1]
         "-0" [0]
         "2147483647" [Integer/MAX_VALUE]
         "-2147483648" [Integer/MIN_VALUE]
         "1 2 3" [1 2 3])))

