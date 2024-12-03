(ns befunge-93.stack
    (:gen-class)
    )
(defn wrap-coordinate [coord max]
      "Implements toroidal wrapping for PC coordinates"
      (mod coord max))

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
      (let [[elementos valor2] (quitar elementos)
            [elementos valor1] (quitar elementos)]
           (agregar (+ valor1 valor2) elementos)))


"- (subtract)    <value1> <value2>       <value1 - value2>"
(defn resta [elementos]
      (let [[elementos valor2] (quitar elementos)
            [elementos valor1] (quitar elementos)]
           (agregar (- valor1 valor2) elementos)))


"* (multiply)    <value1> <value2>       <value1 * value2>"
(defn multiply [elementos]
      (let [[elementos valor2] (quitar elementos)
            [elementos valor1] (quitar elementos)]
           (agregar (* valor1 valor2) elementos)))


"/ (divide)      <value1> <value2>       <value1 / value2> (nb. integer)"
(defn divide [elementos]
      (let [[elementos valor2] (quitar elementos)
            [elementos valor1] (quitar elementos)]
           (if (zero? valor2)
           (throw (Exception. "La division por cero no esta permitida"))
           (agregar (int (/ valor1 valor2)) elementos))))



"% (modulo)      <value1> <value2>       <value1 mod value2>"
(defn modulo [elementos]
      (let [[elementos valor2] (quitar elementos)
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
            (let [[elementos valor2] (quitar elementos)
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
      (let [[elementos valor2] (quitar elementos)
            [elementos valor1] (quitar elementos)]
           (agregar valor1 (agregar valor2 elementos))))

"$ (pop)         <value>                 pops <value> but does nothing"
(defn pop-f [elementos]
      (let [elementos (first (quitar elementos))]

           elementos)

      )

". (output int)  <value>                 outputs <value> as integer
El comando . desapila un valor y lo imprime en la salida estándar como un valor decimal, seguido por un espacio"
(defn output-int [elementos]
      (let [[elementos valor] (quitar elementos)]
           (print valor)
           (flush)
           elementos)
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
            matriz (:matriz elementos)
            wrapped-x (wrap-coordinate x (count (first matriz)))
            wrapped-y (wrap-coordinate y (count matriz))]
           (assoc elementos :matriz (assoc-in matriz [wrapped-y wrapped-x] (char valor)))))

"g (get)         <x> <y>                 gets value at (x,y) and pushes it"
(defn g-cmd [elementos]
  (let [[elementos y] (quitar elementos)
        [elementos x] (quitar elementos)
        matriz (:matriz elementos)
        valor (if (or (>= x (count (first matriz))) (>= y (count matriz)) (< x 0) (< y 0))
                0
                (get-in matriz [y x]))]
    (agregar (int valor) elementos))
)