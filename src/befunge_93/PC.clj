(ns befunge-93.PC
    (:require [befunge-93.stack :refer [quitar, wrap-coordinate]]))

(def filas 25)
(def columnas 80)

(defn move-pc [elementos]
      "Mueve el PC en la dirección especificada por el mapa de elementos"
      (let [x (:columna (:pc elementos))
            y (:fila (:pc elementos))
            direction (:direction (:pc elementos))
            [new-x new-y] (case direction
                                :right [(inc x) y]
                                :left  [(dec x) y]
                                :up    [x (dec y)]
                                :down  [x (inc y)])

            ]
           (assoc elementos :pc {:columna (wrap-coordinate new-x columnas)
                                 :fila (wrap-coordinate new-y filas)
                                 :direction direction})))

(defn horizontal-if [elementos]
      "PC->left if <value>, else PC->right"
      (let [[elementos valor] (quitar elementos)]
           (assoc elementos :pc
                  (assoc (:pc elementos)
                         :direction (if (zero? valor) :right :left)))))

(defn vertical-if [elementos]
      "PC->up if <value>, else PC->down"
      (let [[elementos valor] (quitar elementos)]
           (assoc elementos :pc
                  (assoc (:pc elementos)
                         :direction (if (zero? valor) :down :up)))))

(defn bridge [elementos]
      "Skips the next command by moving PC twice in current direction"
      (-> elementos
          move-pc))
