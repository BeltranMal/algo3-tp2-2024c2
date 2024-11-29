(ns befunge-93.entrada
    (:gen-class)
    (:require [befunge-93.stack :refer [agregar]]))

"& (input int)                           <value user entered>"
(defn input-int [elementos]
    (let [valor (read-line)]
        (agregar (Integer/parseInt valor) elementos)))


(defn input-char [elementos]
    (let [valor (read-line)]
        (agregar (int (first valor)) elementos)))
