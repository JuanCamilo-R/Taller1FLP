#lang eopl




;;2.
;;
;; filtro :
;; Propósito:
;; Procedimiento que apartir de un predicado pred y una lista lst, devuelve una lista
;; compuesta por los elementos de lst para los cuales pred devuelve #t.
(define (filtro pred lst)
  (if (null? lst) '()
      (if (pred (car lst))
          (cons (car lst) (filtro pred (cdr lst)))
          (filtro pred (cdr lst)))))

;;Pruebas:
(filtro number? '((1 2 a (b)) 3 4 c 5 d))
(filtro symbol? '((1 2 a (b)) 3 4 c 5 d))
(filtro list? '((1 2 a (b)) 3 4 c 5 d))


;;5.
;;
;;cambioPos :
;:Primera función auxiliar cambioPos
;;Esta función recibe un número num y una lista lista, y devuelve una lista
;;en la que se intercambió el elemento de la lista
;;en la posición num con el elemento en la posición (num + 1).

(define (cambioPos num lista)
  (if (equal? num 0)
      (cons (cadr lista) (cons (car lista) (cddr lista)))
      (cons (car lista) (cambioPos (- num 1) (cdr lista))  )))

;;Pruebas
(cambioPos 2 '(0 1 2 3 4 5 6))
(cambioPos 1 '('a 'c 'b 'd))
(cambioPos 0 '(4 'e 'e 5))


;;5.
;;
;;todoListo? :
;;Segunda función auxiliar todoListo?
;;Esta función verifica si la lista recibida ya está ordenada en orden
;;ascendente o descendente, según la operación especificaada.
;;todoListo? recibe tres parámetros:
;;1-posicion es la posición del elemento de la lista que está comparando
;;con el siguiente elemento, si este valor es distinto de cero no recorrerá la
;;lista completa.
;;2-pred es la función que usará para comparar dos elementos.
;;3-lista es la lista que va a verificar si está ordenada.
;;
;;Si la lista está totalmente ordenada, todoListo? retorna "¡Lista!"
;;de lo contrario retorna la posición previa al primer número desordenado.

(define (todoListo? posicion pred lista)
  (if (null? (cdr lista)) "¡Lista!"
       (if (pred (car lista) (cadr lista))
           (todoListo? (+ posicion 1) pred (cdr lista))
           posicion))  )
;;Pruebas
(todoListo? 0 < '(0 1 2 3 4 5))
(todoListo? 0 > '(0 1 2 3 4 5))
(todoListo? 0 < '(10 15 16 35 50 24 100))


;;5.
;;
;;ordenar:
;;La función ordenar recibe una operación pred que puede ser < ó >,
;;y una lista de números lst-num, la cual devuelve ordenada según el operador
;;ingresado, con ayuda de las funciones todoListo? y cambioPos. 
(define (ordenar pred lst-num)
  (if (null? lst-num) lst-num
      (if (number? (todoListo? 0 pred lst-num))
                 (ordenar pred (cambioPos (todoListo? 0 pred lst-num)
                                          lst-num))
                 lst-num)))  

;;Pruebas
(ordenar < '(58 41 67 54 32 10))
(ordenar > '(58 41 67 54 32 10))
(ordenar > '(0 1 2 3 4 5 6 7 8 9))





;;8.
;;
;;intercambio:
;;Esta función recibe tres argumentos: dos elementos elem1 y elem2, y una lista S-list
;;a la cual pertenecen elem1 y elem2.
;;intercambio revisa elemento a elemento de S-list si hay
;;ocurrencias de elem1 o elem2 para intercambiarlos. En caso
;;de encontrar una sublista, se aplica la función a la sublista.
(define (intercambio elem1 elem2 S-list)
  (if (null? S-list) S-list
    (if (list? (car S-list))
       (cons (intercambio elem1 elem2 (car S-list))  (intercambio elem1 elem2 (cdr S-list))) 
       (if (equal? elem1 (car S-list))
          (cons elem2 (intercambio elem1 elem2 (cdr S-list)))
          (if (equal? elem2 (car S-list))
              (cons elem1 (intercambio elem1 elem2 (cdr S-list)))
              (cons (car S-list) (intercambio elem1 elem2 (cdr S-list)))))))  )

;;Pruebas
(intercambio 'a 'd '(a b c d))
(intercambio 'a 'd '(a d () c d))
(intercambio 'x 'y '((x) y (z (x))))




;;11.
;;
;;list-append:
;;Recibe dos listas lst1 y lst2, y devuelve una lista con los elementos de
;;lst2 añadidos a los de lst1. 


(define (list-append list1 list2)
  (if (null? list1) list2
      (cons (car list1) (list-append (cdr list1) list2))))

;;Pruebas
(list-append '(1 2 3) '(4 5))
(list-append '('a () c) '((()) e))
(list-append '(u f c j) '(h))



;;14.
;;
;;Presente?:
;;Función auxiliar de path.
;;Recibe una lista lista y un elemento elem. Si el elemento está presente en la
;;lista recibida, o en alguna de sus sublistas, la función retorna #t, de lo
;;contrario retorna #f.
(define (presente? lista elem)
  (if (null? lista) #f
      (if (equal? elem (car lista)) #t
          (if (list? (car lista))
              (or (presente? (car lista) elem) (presente? (cdr lista) elem))
              (presente? (cdr lista) elem)))))

;;14
;;
;;path:
;;Recibe un número n y un árbol binario de búsqueda BST que contiene el número n.
;;La función retorna una lista con la ruta a tomar (iniciando desde el nodo
;;raíz del árbol), indicada por cadenas left y right, hasta llegar al número n.
;;Si n se encuentra en el nodo raíz, path retorna una lista vacía.
(define (path n BTS)
  (if (equal? (car BTS) n) '()
      (if (presente? (cadr BTS) n)
          (cons 'left (path n (cadr BTS)))
          (cons 'right (path n (caddr BTS))))))


;;Pruebas

(path 17
'(14 (7 () (12 () ()))
     (26 (20 (17 () ()) ())
         (31 () ())        )))
(path 14
'(14 (7 () (12 () ()))
     (26 (20 (17 () ()) ())
         (31 () ())        )))
(path 31
'(14 (7 () (12 () ()))
     (26 (20 (17 () ()) ())
         (31 () ())        )))
