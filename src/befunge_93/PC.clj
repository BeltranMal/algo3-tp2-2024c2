(ns befunge-93.PC
    (:require [befunge-93.stack :refer [quitar]]))

(def pc (atom [0 0]))
(def filas 25)
(def columnas 80)


;;PC movement functions
(defn wrap-coordinate [coord max]
      "Implements toroidal wrapping for PC coordinates"
      (mod coord max))

(defn move-pc [direction]
      (swap! pc (fn [[x y]]
                    (let [[new-x new-y]
                          (case (if (= direction :random)
                                  (rand-nth [:right :left :up :down])
                                  direction)
                                :right  [(inc x) y]
                                :left   [(dec x) y]
                                :up     [x (dec y)]
                                :down   [x (inc y)])]
                         [(wrap-coordinate new-x columnas)
                          (wrap-coordinate new-y filas)]))))




"_ (horizontal if) <boolean value>   PC->left if <value>, else PC->right"
(defn _ []
      (let [valor (quitar)]
           (if (zero? valor)
             (move-pc :right)
             (move-pc :left))))

"| (vertical if)   <boolean value>       PC->up if <value>, else PC->down"
(defn | []
      (let [valor (quitar)]
           (if (zero? valor)
             (move-pc :up)
             (move-pc :down))))

"# (bridge)                              'jumps' PC one farther; skips"
(defn bridge [direccion]
      (move-pc direccion))


