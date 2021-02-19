(ns interprete.core-test
  (:require [clojure.test :refer :all]
            [interprete.basic :refer :all]))

(deftest test-aplicar
  (testing "Aplica un operador a sus operandos y retorna el valor resultante (si ocurre un error, muestra un mensaje y retorna nil)"
    (is (= '1 (aplicar 'INT '1.2 '10)))
    (is (= '-42 (aplicar 'INT '-42.2 '10)))
    (is (= '65 (aplicar 'ASC '"ASC" '14)))
    (is (= '67 (aplicar 'ASC '"CSC" '14)))
    (is (= '2 (aplicar '- '12 '10 '14)))
    (is (= '-1 (aplicar '- '9 '10 '14)))
    (is (= '4 (aplicar '* '2 '2 '14)))
    (is (= '-12 (aplicar '* '4 '-3 '14)))
    (is (= '0.25 (aplicar '/ '1 '4 '14)))
    (is (= '0 (aplicar '< '2 '2 '14)))
    (is (= '1 (aplicar '< '1 '2 '14)))
    (is (= '0 (aplicar '< '4 '2 '14)))
    (is (= '1 (aplicar '<= '2 '2 '14)))
    (is (= '1 (aplicar '<= '1 '2 '14)))
    (is (= '0 (aplicar '<= '4 '2 '14)))
    (is (= '0 (aplicar '> '2 '2 '14)))
    (is (= '1 (aplicar '> '4 '2 '14)))
    (is (= '0 (aplicar '> '4 '8 '14)))
    (is (= '1 (aplicar '>= '2 '2 '14)))
    (is (= '1 (aplicar '>= '4 '2 '14)))
    (is (= '0 (aplicar '>= '4 '8 '14)))
    (is (= '1 (aplicar 'OR '1 '0 '14)))
    (is (= '1 (aplicar 'OR '0 '1 '14)))
    (is (= '1 (aplicar 'OR '1 '1 '14)))
    (is (= '0 (aplicar 'OR '0 '0 '14)))
    )
  )

(deftest test-palabras-reservadas
  (testing "Al preguntarse si las siguientes palabras son reservadas el resultado es el indicado"
    (is (= false (palabra-reservada? 'SPACE)))
    (is (= true (palabra-reservada? 'REM)))))

(deftest test-operador?
  (testing "Al preguntarse si los Siguientes simbolos Son parte del lenguaje el resultado es el indicado"
    (is (= false (operador? (symbol "%"))))
    (is (= true (operador? (symbol "+")))))
    )

(deftest test-anular-invalidos
  (testing "Recibe una lista de simbolos y la retorna con aquellos que son invalidos reemplazados por nil"
    (is (= '(IF X nil * Y < 12 THEN LET nil X = 0) (anular-invalidos '(IF X & * Y < 12 THEN LET ! X = 0))))
  )
  )

(deftest test-linea-ya-cargada
  (testing "Al preguntar si una lina ya estaba previamente cargada devuelve true en caso afirmativo, false sino"
    (is (= false (linea-ya-cargada '() '(10 (PRINT X)) )))
    (is (= false (linea-ya-cargada '((20 (X = 100))) '(10 (PRINT X)) )))
    (is (= true (linea-ya-cargada '((10 (PRINT X)) (20 (X = 100))) '(10 (PRINT X)) )))
    ))

(deftest test-actualizar-prog
  (testing "Recibe una linea de codigo y actualiza la lista de lineas de codigo"
    (is (= '((10 (PRINT X))) (actualizar-prog '() '(10 (PRINT X)))))
    (is (= '((10 (PRINT X)) (20 (X = 100))) (actualizar-prog '((10 (PRINT X))) '(20 (X = 100)))))
    (is (= '((10 (PRINT X)) (15 (X = X + 1)) (20 (X = 100))) (actualizar-prog '((10 (PRINT X)) (20 (X = 100))) '(15 (X = X + 1)))))
    (is (= '((10 (PRINT X)) (15 (X = X - 1)) (20 (X = 100))) (actualizar-prog '((10 (PRINT X)) (15 (X = X + 1)) (20 (X = 100))) '(15 (X = X - 1)))))
    ))

(deftest test-cargar-linea
  (testing "Recibe una linea de codigo y un ambiente y retorna el ambiente actualizado"
    (is (= '[((10 (PRINT X))) [:ejecucion-inmediata 0] [] [] [] 0 {}] (cargar-linea '(10 (PRINT X)) [() [:ejecucion-inmediata 0] [] [] [] 0 {}])))
    (is (= '[((10 (PRINT X)) (20 (X = 100))) [:ejecucion-inmediata 0] [] [] [] 0 {}] (cargar-linea '(20 (X = 100)) ['((10 (PRINT X))) [:ejecucion-inmediata 0] [] [] [] 0 {}])))
    (is (= '[((10 (PRINT X)) (15 (X = X + 1)) (20 (X = 100))) [:ejecucion-inmediata 0] [] [] [] 0 {}] (cargar-linea '(15 (X = X + 1)) ['((10 (PRINT X)) (20 (X = 100))) [:ejecucion-inmediata 0] [] [] [] 0 {}])))
    (is (= '[((10 (PRINT X)) (15 (X = X - 1)) (20 (X = 100))) [:ejecucion-inmediata 0] [] [] [] 0 {}] (cargar-linea '(15 (X = X - 1)) ['((10 (PRINT X)) (15 (X = X + 1)) (20 (X = 100))) [:ejecucion-inmediata 0] [] [] [] 0 {}])))
    )
  )

(deftest test-expandir-nexts
  (testing "Recibe una lista de sentencias y la devuelve con las sentencias NEXT compuestas expresadas como sentencias NEXT simples"
    (is (= '((PRINT 1) (NEXT A) (NEXT B)) (expandir-nexts '((PRINT 1) (NEXT A , B)))))
    (is (= '((NEXT A)(NEXT B)) (expandir-nexts '((NEXT A , B)))))
    (is (= '((PRINT 1)) (expandir-nexts '((PRINT 1)))))
    (is (= '((PRINT 1)(NEXT)) (expandir-nexts '((PRINT 1)(NEXT)))))
    (is (= '((NEXT)) (expandir-nexts '((NEXT)))))
    )
  )

(deftest test-dar-error
  (testing "Recibe un error (codigo o mensaje) y el puntero de programa, muestra el error correspondiente y retorna nil"
    ;(is (= nil (dar-error 16 [:ejecucion-inmediata 4])))
    ;(is (= nil (dar-error "?ERROR DISK FULL" [:ejecucion-inmediata 4])))
    ;(is (= nil (dar-error 16 [100 3])))
    ;(is (= nil (dar-error "?ERROR DISK FULL" [100 3])))
    ))

(deftest test-variable-float?
  (testing "Predicado para determinar si un identificador es una variable de punto flotante"
    (is (= true (variable-float? 'X)))
    (is (= false (variable-float? 'X%)))
    (is (= false (variable-float? 'X$)))
    )
  )
(deftest test-variable-integer?
  (testing "Predicado para determinar si un identificador es una variable de entera"
    (is (= false (variable-integer? 'X)))
    (is (= true (variable-integer? 'X%)))
    (is (= false (variable-integer? 'X$)))
    )
  )

(deftest test-variable-string?
  (testing "Predicado para determinar si un identificador es una variable de cadena"
    (is (= false (variable-string? 'X)))
    (is (= false (variable-string? 'X%)))
    (is (= true (variable-string? 'X$)))
    )
  )


(deftest test-contar-sentencia
  (testing "Recibe un numero de linea y un ambiente y retorna la cantidad de sentencias que hay en la linea indicada"
    (is (= '2 (contar-sentencias 10 [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [10 1] [] [] [] 0 {}])))
    (is (= '1 (contar-sentencias 15 [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [10 1] [] [] [] 0 {}])))
    (is (= '2 (contar-sentencias 20 [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [10 1] [] [] [] 0 {}])))
    (is (= nil (contar-sentencias 30 [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [10 1] [] [] [] 0 {}])))
    )
  )

(deftest test-buscar-lineas-restantes
  (testing "Recibe un ambiente y retorna la representacion intermedia del programa a partir del puntero de programa
  (que indica la linea y cuantas sentencias de la misma aun quedan por ejecutar)"
    (is (= nil (buscar-lineas-restantes [() [:ejecucion-inmediata 0] [] [] [] 0 {}])))
    (is (= nil (buscar-lineas-restantes ['((PRINT X) (PRINT Y)) [:ejecucion-inmediata 2] [] [] [] 0 {}])))
    (is (= (list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [10 2] [] [] [] 0 {}])))
    (is (= (list '(10 (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [10 1] [] [] [] 0 {}])))
    (is (= (list '(10) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [10 0] [] [] [] 0 {}])))
    (is (= (list '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [15 1] [] [] [] 0 {}])))
    (is (= (list '(15) (list 20 (list 'NEXT 'I (symbol ",") 'J))) (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [15 0] [] [] [] 0 {}])))
    (is (= '((20 (NEXT I) (NEXT J))) (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [20 3] [] [] [] 0 {}])))
    (is (= '((20 (NEXT I) (NEXT J))) (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [20 2] [] [] [] 0 {}])))
    (is (= (list (list 20 (list 'NEXT 'J))) (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [20 1] [] [] [] 0 {}])))
    (is (= '((20)) (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [20 0] [] [] [] 0 {}])))
    (is (= '((20)) (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [20 -1] [] [] [] 0 {}])))
    (is (= nil (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [25 0] [] [] [] 0 {}])))
    )
  )
(deftest test-continuar-linea
  (testing "Implementa la sentencia RETURN, retornando una dupla (un vector) con un resultado (usado luego por evaluar-linea) y un ambiente actualizado con el nuevo valor del puntero de programa"
    ;(is (= [nil [(list '(10 (PRINT X)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [20 3] [] [] [] 0 {}]] (continuar-linea [(list '(10 (PRINT X)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [20 3] [] [] [] 0 {}])))
    (is (= [:omitir-restante [(list '(10 (PRINT X)) '(15 (GOSUB 100) (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [15 1] [] [] [] 0 {}]] (continuar-linea [(list '(10 (PRINT X)) '(15 (GOSUB 100) (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [20 3] [[15 2]] [] [] 0 {}])))
    )
  )
(deftest test-extraer-data
  (testing "Recibe la representación intermedia de un programa y retorna una lista con todos los valores embebidos en las sentencias DATA"
    (is (= '("HOLA" "MUNDO" 10 20) (extraer-data (list '(10 (PRINT X) (REM ESTE NO) (DATA 30)) '(20 (DATA HOLA)) (list 100 (list 'DATA 'MUNDO (symbol ",") 10 (symbol ",") 20))))))
    (is (= '(30 "HOLA" "MUNDO" 10 20) (extraer-data (list '(10 (PRINT X) (DATA 30) (REM ESTE NO)) '(20 (DATA HOLA)) (list 100 (list 'DATA 'MUNDO (symbol ",") 10 (symbol ",") 20))))))
    (is (= '(30 40 50 "HOLA" "MUNDO" 10 20) (extraer-data (list '(10 (PRINT X) (DATA 30 , 40 , 50) (REM ESTE NO) (DATA 30)) '(20 (DATA HOLA)) (list 100 (list 'DATA 'MUNDO (symbol ",") 10 (symbol ",") 20))))))
    )
  )

(deftest test-ejecutar-asignacion
  (testing "Recibe una asignacion y un ambiente, y retorna el ambiente actualizado al efectuar la asignacion"
    (is (= '[((10 (PRINT X))) [10 1] [] [] [] 0 {X 5}] (ejecutar-asignacion '(X = 5) ['((10 (PRINT X))) [10 1] [] [] [] 0 {}])))
    (is (= '[((10 (PRINT X))) [10 1] [] [] [] 0 {X 5}] (ejecutar-asignacion '(X = 5) ['((10 (PRINT X))) [10 1] [] [] [] 0 '{X 2}])))
    (is (= '[((10 (PRINT X))) [10 1] [] [] [] 0 {X 3}] (ejecutar-asignacion '(X = X + 1) ['((10 (PRINT X))) [10 1] [] [] [] 0 '{X 2}])))
    (is (= '[((10 (PRINT X))) [10 1] [] [] [] 0 {X 26}] (ejecutar-asignacion '(X = X + 14 + 10) ['((10 (PRINT X))) [10 1] [] [] [] 0 '{X 2}])))
    (is (= '[((10 (PRINT X))) [10 1] [] [] [] 0 {X$ "HOLA MUNDO"}] (ejecutar-asignacion '(X$ = X$ + " MUNDO") ['((10 (PRINT X))) [10 1] [] [] [] 0 '{X$ "HOLA"}])))
    (is (= '[((10 (PRINT X))) [10 1] [] [] [] 0 {X$ "HOLA"}] (ejecutar-asignacion '(X$ = "HOLA") ['((10 (PRINT X))) [10 1] [] [] [] 0 '{}])))
    (is (= '[((10 (PRINT X))) [10 1] [] [] [] 0 {X$ "HOLA"}] (ejecutar-asignacion '(X$ = "HOLA") ['((10 (PRINT X))) [10 1] [] [] [] 0 '{X$ "MUNDO"}])))
    )
  )

(deftest test-prepocesar-expresion
  (testing "recibe una expresion y la retorna con las variables reemplazadas por sus valores y el punto por el cero"
    (is (= '("HOLA" + " MUNDO" + "") (preprocesar-expresion '(X$ + " MUNDO" + Z$) ['((10 (PRINT X))) [10 1] [] [] [] 0 '{X$ "HOLA"}])))
    (is (= '(5 + 0 / 2 * 0) (preprocesar-expresion '(X + . / Y% * Z) ['((10 (PRINT X))) [10 1] [] [] [] 0 '{X 5 Y% 2}])))
    (is (= '(5 + 10 / 2 * 0) (preprocesar-expresion '(X + 10 / Y% * Z) ['((10 (PRINT X))) [10 1] [] [] [] 0 '{X 5 Y% 2}])))
    (is (= '(10 + 34 + 2) (preprocesar-expresion '(10 + 34 + 2) ['((10 (PRINT X))) [10 1] [] [] [] 0 '{}])))
    (is (= '("HOLA" + " MUNDO") (preprocesar-expresion '(X$ + " MUNDO") ['((10 (PRINT X))) [10 1] [] [] [] 0 '{X$ "HOLA"}])))
    )
  )
(deftest test-desambiguar
  (testing "Recibe un expresion y la retorna sin los + unarios con los - unarios reemplazados por -u y los MID$ ternarios reemplazados por MID3$"
    (is (= '(-u 2 * ( -u 3 + 5 - ( 2 / 7 ) ))) (desambiguar (list '- 2 '* (symbol "(") '- 3 '+ 5 '- (symbol "(") '+ 2 '/ 7 (symbol ")") (symbol ")"))))
    (is (= (list 'MID$ (symbol "(") '1 (symbol ",") '2 (symbol ")")) (desambiguar (list 'MID$ (symbol "(") 1 (symbol ",") 2 (symbol ")")))))
    (is (= (list 'MID3$ (symbol "(") '1 (symbol ",") '2 (symbol ",") '3(symbol ")")) (desambiguar (list 'MID$ (symbol "(") 1 (symbol ",") 2 (symbol ",") 3 (symbol ")")))))
    (is (= (list 'MID3$ (symbol "(") '1 (symbol ",") '-u '2 '+ 'K (symbol ",") '3 (symbol ")")) (desambiguar (list 'MID$ (symbol "(") 1 (symbol ",") '- 2 '+ 'K (symbol ",") 3 (symbol ")")))))
    )
  )

(deftest test-precendencia
  (testing "Recibe un token y retorna el valor de su precedencia"
    (is (= '1 (precedencia 'OR)))
    (is (= '2 (precedencia 'AND)))
    (is (= '6 (precedencia '*)))
    (is (= '7 (precedencia '-u)))
    (is (= '9 (precedencia 'MID$)))
    )
  )

(deftest test-aridad
  (testing "Recibe un token y retorna el valor de su aridad"
    (is(= 0 (aridad 'THEN)))
    (is(= 1 (aridad 'SIN)))
    (is(= 2 (aridad '*)))
    (is(= 2 (aridad 'MID$)))
    (is(= 3 (aridad 'MID3$)))
    )
  )


(deftest test-elimiar-cero-decimal
  (testing "Recibe un numero y lo retorna sin ceros decimales no significativos"
    (is (= 1.5 (eliminar-cero-decimal (symbol "1.5"))))
    (is (= 1.5 (eliminar-cero-decimal (symbol "1.50"))))
    (is (= 1.05 (eliminar-cero-decimal (symbol "1.050"))))
    (is (= 1 (eliminar-cero-decimal (symbol "1.00000"))))
    (is (= 0.004 (eliminar-cero-decimal (symbol "0.0040000"))))
    (is (= (symbol "A") (eliminar-cero-decimal (symbol "A"))))
    (is (= 0 (eliminar-cero-decimal (symbol "."))))
    (is (= 31.123 (eliminar-cero-decimal (symbol "000031.123"))))
    (is (= "HOLA" (eliminar-cero-decimal "HOLA")))
    )
  )

(deftest test-eliminar-cero-entero
  (testing "Recibe un simbolo y lo retorna convertido en cadena, omitiendo para los numeros del intervalo (-1..1) el cero a la izquierda del punto"
    (is (= nil (eliminar-cero-entero nil)))
    (is (= "A" (eliminar-cero-entero 'A)))
    (is (= "0" (eliminar-cero-entero 0)))
    (is (= "0" (eliminar-cero-entero (symbol "."))))
    (is (= "1.5" (eliminar-cero-entero 1.5)))
    (is (= "1" (eliminar-cero-entero 1)))
    (is (= "-1" (eliminar-cero-entero -1)))
    (is (= "-.5" (eliminar-cero-entero -0.5)))
    (is (= ".5" (eliminar-cero-entero 0.5)))
    )
  )