(ns befunge-93.stack
    (:gen-class))


;; Stack manipulation functions
(defn agregar [valor elementos]
      (assoc elementos :stack (conj (:stack elementos) valor)))

(defn quitar [elementos]
      (let [stack (:stack elementos)]
           (if (empty? stack)
             [elementos 0]
             [(assoc elementos :stack (vec (butlast stack))) (peek stack)])))

"+ (add)         <value1> <value2>       <value1 + value2>"
(defn add [elementos]
      (let [[elementos valor2] (quitar elementos)           ; declara valor2 y elementos / valor 2 es el primer elemento de la pila
            [elementos valor1] (quitar elementos)]          ; declara valor1 y elementos / valor 1 es el segundo elemento de la pila
           (agregar (+ valor1 valor2) elementos)))          ; agrega a la pila el resultado de la suma de valor1 y valor2


"- (subtract)    <value1> <value2>       <value1 - value2>"
(defn resta [elementos]
      (let [[elementos valor2] (quitar elementos)           ; declara valor2 y elementos / valor 2 es el primer elemento de la pila
            [elementos valor1] (quitar elementos)]          ; declara valor1 y elementos / valor 1 es el segundo elemento de la pila
           (agregar (- valor1 valor2) elementos)))


"* (multiply)    <value1> <value2>       <value1 * value2>"
(defn multiply [elementos]
      (let [[elementos valor2] (quitar elementos)           ; declara valor2 y elementos / valor 2 es el primer elemento de la pila
            [elementos valor1] (quitar elementos)]          ; declara valor1 y elementos / valor 1 es el segundo elemento de la pila
           (agregar (* valor1 valor2) elementos)))


"/ (divide)      <value1> <value2>       <value1 / value2> (nb. integer)"
(defn divide [elementos]
      (let [[elementos valor2] (quitar elementos)           ; declara valor2 y elementos / valor 2 es el primer elemento de la pila
            [elementos valor1] (quitar elementos)]          ; declara valor1 y elementos / valor 1 es el segundo elemento de la pila
           (if (zero? valor2)
           (throw (Exception. "La division por cero no esta permitida"))
           (agregar (int (/ valor1 valor2)) elementos))))



"% (modulo)      <value1> <value2>       <value1 mod value2>"
(defn modulo [elementos]
      (let [[elementos valor2] (quitar elementos)           ; declara valor2 y elementos / valor 2 es el primer elemento de la pila
            [elementos valor1] (quitar elementos)]
           (if (zero? valor2)
           (throw (Exception. "La division por cero no esta permitida"))
           (agregar (mod valor1 valor2) elementos))))


"! (not)         <value>                 <0 if value non-zero, 1 otherwise>"

(defn not-f [elementos]
      (let [[elementos valor] (quitar elementos)
           resultado (if (zero? valor)
                        1
                        0)]

           (agregar resultado elementos)))


"` (greater)     <value1> <value2>       <1 if value1 > value2, 0 otherwise>\n"
(defn greater [elementos]
            (let [[elementos valor2] (quitar elementos)           ; declara valor2 y elementos / valor 2 es el primer elemento de la pila
                  [elementos valor1] (quitar elementos)
                  resultado (if (> valor1 valor2)
                              1
                              0)]
                 (agregar resultado elementos)))


" (stringmode)                          Toggles 'stringmode'"
(defn toggle-stringmode [elementos]
  (assoc elementos :string-mode (not (:string-mode elementos)))
     )



"El comando : desapila un valor y lo apila dos veces.
: (dup)         <value>                 <value> <value>"
(defn dup [elementos]
      (let [[elementos valor] (quitar elementos)]
           (agregar valor (agregar valor elementos))))

"  (swap)        <value1> <value2>       <value2> <value1>
El comando  intercambia los dos primeros elementos de la pila. "

(defn swap [elementos]
      (let [[elementos valor2] (quitar elementos)           ; declara valor2 y elementos / valor 2 es el primer elemento de la pila
            [elementos valor1] (quitar elementos)]          ; declara valor1 y elementos / valor 1 es el segundo elemento de la pila
           (agregar valor2 (agregar valor1 elementos))))


"$ (pop)         <value>                 pops <value> but does nothing"
(defn pop-f [elementos]
      (let [elementos (quitar elementos)]))

". (output int)  <value>                 outputs <value> as integer
El comando . desapila un valor y lo imprime en la salida estándar como un valor decimal, seguido por un espacio"
(defn output-int [elementos]
      (let [[elementos valor] (quitar elementos)]
           (println valor)
           (flush)
           )
      )

", (output char) <value>                 outputs <value> as ASCII"
(defn output-char [elementos]

      (let [[elementos valor] (quitar elementos)]
           (print (char valor))
           (flush)
           elementos)
      )

"p (put)         <value> <x> <y>         puts <value> at (x,y)"
(defn p-cmd [elementos]
      (let [[elementos y] (quitar elementos)
            [elementos x] (quitar elementos)
            [elementos valor] (quitar elementos)
            matriz (:matriz elementos)]
           (assoc elementos :matriz (assoc matriz y (assoc (matriz y) x valor)))))


"g (get)         <x> <y>                 gets value at (x,y) and pushes it"
(defn g-cmd [elementos]
      (let [[elementos y] (quitar elementos)
            [elementos x] (quitar elementos)
            matriz (:matriz elementos)
            valor (get-in matriz [y x])]

           (agregar valor elementos))
      )