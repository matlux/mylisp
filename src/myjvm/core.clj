(ns myjvm.core
  (:gen-class))

(def var1 "string")
(def var2 123)
(def var3 123.456)
(def var4 true)

(defn -main
  [& args]
  (doseq [v [var1 var2 var3 var4]]
    (println v)))

