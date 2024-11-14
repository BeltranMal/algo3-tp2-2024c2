(ns befunge-93.core
    (:require [befunge-93.stack :refer :all])
    (:require [befunge-93.PC :refer :all])
  (:gen-class))



(defn crear-matriz []
  (vec (repeat filas (vec (repeat columnas \space)))))


(defn read-program [file-path]
  "Lee el archivo Befunge-93 y lo convierte en una grilla de 80x25."
  (let [lines (with-open [rdr (clojure.java.io/reader file-path)]
                (doall (line-seq rdr)))]
    (vec (map-indexed (fn [y line]
                        (vec (concat (mapv identity (take columnas line))
                                     (repeat (- columnas (count line)) \space))))
                      (take filas lines)))))


(defn -main [& args]
  "I don't do a whole lot ... yet."

      (println "Hello, World!")
      (println (read-program (first args)))
      "llamar a pila y hacer 2 push y luego llamar a add"
      (push 6)
      (push 6)
      (push 5)
      (add)
      (multiply)
      (push 1)
      (resta)
      (output-char)


      (println "la pila queda de la siguiente forma: " @pila)
  )

