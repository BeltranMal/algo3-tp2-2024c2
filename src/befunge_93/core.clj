(ns befunge-93.core
    (:require [befunge-93.stack :refer :all])
    (:require [befunge-93.PC :refer :all])
    (:require [befunge-93.entrada :refer :all])
  (:gen-class))

(defn read-program [file-path elementos]
  "Lee el archivo Befunge-93 y lo convierte en una grilla de 80x25 y lo carga en elementos."
  (let [lines (with-open [rdr (clojure.java.io/reader file-path)]
                (doall (line-seq rdr)))
        matriz (vec (map-indexed (fn [y line]
                        (vec (concat (mapv identity (take columnas line))
                                     (repeat (- columnas (count line)) \space))))
                                 (take filas lines)))]
    (assoc elementos :matriz matriz)))



(defn interpretar-cmd [elemento elementos]
      "Interpreta un comando de Befunge-93"
      (let [elementos (if (= elemento \")
                        (toggle-stringmode elementos)
                        elementos)
            nuevos-elementos (cond
                               (:string-mode elementos) (if (not= elemento \")
                                                          (agregar (int elemento) elementos)
                                                          elementos)
                               :else (case elemento
                                           \0 (agregar 0 elementos)
                                           \1 (agregar 1 elementos)
                                           \2 (agregar 2 elementos)
                                           \3 (agregar 3 elementos)
                                           \4 (agregar 4 elementos)
                                           \5 (agregar 5 elementos)
                                           \6 (agregar 6 elementos)
                                           \7 (agregar 7 elementos)
                                           \8 (agregar 8 elementos)
                                           \9 (agregar 9 elementos)
                                            \+ (add elementos)
                                            \- (resta elementos)
                                            \* (multiply elementos)
                                            \/ (divide elementos)
                                            \% (modulo elementos)
                                            \! (not-f elementos)
                                            \`(greater elementos)
                                           \\ (swap elementos)
                                            \_ (horizontal-if elementos)
                                            \| (vertical-if elementos)
                                            \: (dup elementos)
                                           \$ (pop-f elementos)
                                           \# (bridge elementos)
                                           \. (output-int elementos)
                                           \, (output-char elementos)
                                           \> (assoc elementos :pc (assoc (:pc elementos) :direction :right))
                                            \< (assoc elementos :pc (assoc (:pc elementos) :direction :left))
                                            \^ (assoc elementos :pc (assoc (:pc elementos) :direction :up))
                                            \v (assoc elementos :pc (assoc (:pc elementos) :direction :down))
                                            \? (assoc elementos :pc (assoc (:pc elementos) :direction (rand-nth [:right :left :up :down])))
                                           \g (g-cmd elementos)
                                            \p (p-cmd elementos)
                                            \& (input-int elementos)
                                            \~ (input-char elementos)
                                           elementos))]
           nuevos-elementos)
      ) ; Retorna los nuevos elementos; Retorna los nuevos elementos

(defn get-current-command [elementos]
      "Obtiene el comando en la posición actual del PC"
      (let [y (get-in elementos [:pc :columna])
            x (get-in elementos [:pc :fila])]
           (get-in elementos [:matriz x y] \space)
          ))



(defn flujo-programa [elementos]
  "Ejecuta el flujo de un programa Befunge-93"
      (loop [elementos elementos]
            (let [comando (get-current-command elementos)
                  nuevos-elementos (interpretar-cmd comando elementos)]
                 (when (= comando \@)
                       (System/exit 0))
                 (recur (move-pc nuevos-elementos))))

      )

(defn -main [& args]
      "I don't do a whole lot ... yet."
      (let [elementos {:pc {:columna 0 :fila 0 :direction :right}
                       :stack []
                       :matriz [[]]
                       :string-mode false}]

           (let [updated-elementos (read-program (first args) elementos)]
                (flujo-programa updated-elementos))))








