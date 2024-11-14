(ns befunge-93.stack
    (:gen-class))
;; Program Counter and Direction handling
 ; initialize PC to (0, 0)
(def pila (atom [])) ; initialize stack as an empty vector
(def stringmode (atom false))

;; Stack manipulation functions
(defn push [value]
      "Pushes a value onto the stack"
      (swap! pila conj value)) "conj es una built in function" ": \"Take the current value of the stack atom, add the new value to the end of it using conj, and then update the stack atom with the new value.\"\n"

(defn quitar []
      "Pops a value from the stack"
      (if (empty? @pila)
          0
          (let [value (peek @pila)]
               (swap! pila pop)
               value)))

"+ (add)         <value1> <value2>       <value1 + value2>"
(defn add []
      (let [value2 (quitar)
            value1 (quitar)]
           (push (+ value1 value2))))

"- (subtract)    <value1> <value2>       <value1 - value2>"
(defn resta []
      (let [value2 (quitar)
            value1 (quitar)]
            (push (- value1 value2))))

"* (multiply)    <value1> <value2>       <value1 * value2>"
(defn multiply []
      (let [value2 (quitar)
            value1 (quitar) ]
           (push (* value1 value2))))

"/ (divide)      <value1> <value2>       <value1 / value2> (nb. integer)"
(defn divide []

      (let [value2 (quitar)
            value1 (quitar)]
           (if (zero? value2)
             (println "No se puede dividir por cero")
             (push (quot value1 value2)))))

"% (modulo)      <value1> <value2>       <value1 mod value2>"
(defn modulo []
      (let [value2 (quitar)
            value1 (quitar) ]
           (if (zero? value2)
             (println "No se puede dividir por cero")
             (push (mod value1 value2)))))

"! (not)         <value>                 <0 if value non-zero, 1 otherwise>"
(defn not-func []
      (let [value (quitar)
            resultado (if (zero? value)
                        1
                        0)]
           (push resultado)))

"` (greater)     <value1> <value2>       <1 if value1 > value2, 0 otherwise>\n"
(defn greater []
      (let [value2 (quitar)
            value1 (quitar)
            resultado (if (> value1 value2)
                        1
                        0)]
           (push resultado)))

" (stringmode)                          Toggles 'stringmode'"
(defn toggle-stringmode []
      (swap! stringmode not))



"El comando : desapila un valor y lo apila dos veces.
: (dup)         <value>                 <value> <value>"
(defn dup []
      (let [valor (quitar)]
           (push valor)
           (push valor)))


"  (swap)        <value1> <value2>       <value2> <value1>
El comando  intercambia los dos primeros elementos de la pila. "

(defn swap []
      (let [value2 (quitar)
            value1 (quitar)]
           (push value2)
           (push value1)))

"$ (pop)         <value>                 pops <value> but does nothing"
(defn pop []
      (quitar))

". (output int)  <value>                 outputs <value> as integer
El comando . desapila un valor y lo imprime en la salida estándar como un valor decimal, seguido por un espacio"
(defn output-int []
      (let [valor (quitar)]
           (print valor)
           (println " ")))

", (output char) <value>                 outputs <value> as ASCII"
(defn output-char []
      (let [valor (quitar)]
           (println (char valor))))

