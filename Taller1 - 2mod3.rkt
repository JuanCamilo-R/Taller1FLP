#lang eopl

;Poner contratos
;poner pruebas


;2.
(define (filtro pred lst)
  (if (null? lst) '()
      (if (pred (car lst))
          (cons (car lst) (filtro pred (cdr lst)))
          (filtro pred (cdr lst)))))



;5aux1
;Función cambioPos
;Esta función recibe un número num y una lista, y devuelve una lista
; en la que se intercambió el elemento de la lista
;en la posición num con el elemento en la posición (num + 1).

(define (cambioPos num lista)
  (if (equal? num 0)
      (cons (cadr lista) (cons (car lista) (cddr lista)))
      (cons (car lista) (cambioPos (- num 1) (cdr lista))  )))


;5aux2
;función todoListo?
;Esta función verifica si la lista recibida ya está ordenada en orden
;ascendente o descendente, según la operación especificaada.
;todoListo? recibe tres parámetros:
;posicion es la posición del elemento de la lista que está comparando
;con el siguiente elemento.
;pred es la función que usará para comparar dos elementos.
;lista es la lista que va a verificar si está ordenada.
;Si la lista está totalmente ordenada la función retorna "¡Lista!"
;de lo contrario retorna la posición previa
;al primer número desordenado.

(define (todoListo? posicion pred lista)
  (if (null? (cdr lista)) "¡Lista!"
       (if (pred (car lista) (cadr lista))
           (todoListo? (+ posicion 1) pred (cdr lista))
           posicion))  )

;5
;La función ordenar recibe una operación que puede ser < ó >,
;y una lista de números, la cual devuelve ordenada según el operador
;ingresado.
(define (ordenar pred lst-num)
  (if (null? lst-num) lst-num
      (if (number? (todoListo? 0 pred lst-num))
                 (ordenar pred (cambioPos (todoListo? 0 pred lst-num)
                                          lst-num))
                 lst-num)))  



;8 

;Esta función revisa elemento a elemento de S-list si hay
;ocurrencias de elem1 o elem2 para reemplazarlos, en caso
;de encontrar una sublista, se aplica la función a la sublista.
(define (intercambio elem1 elem2 S-list)
  (if (null? S-list) S-list
    (if (list? (car S-list))
       (cons (intercambio elem1 elem2 (car S-list))  (intercambio elem1 elem2 (cdr S-list))) 
       (if (equal? elem1 (car S-list))
          (cons elem2 (intercambio elem1 elem2 (cdr S-list)))
          (if (equal? elem2 (car S-list))
              (cons elem1 (intercambio elem1 elem2 (cdr S-list)))
              (cons (car S-list) (intercambio elem1 elem2 (cdr S-list)))))))  )


;11

(define (list-append list1 list2)
  (if (null? list1) list2
      (cons (car list1) (list-append (cdr list1) list2))))



;14auxPresente?
; PONER OR EN LA RECURSIÓN
(define (presente? lista elem)
  (if (null? lista) #f
      (if (equal? elem (car lista)) #t
          (if (list? (car lista))
              (or (presente? (car lista) elem) (presente? (cdr lista) elem))
              (presente? (cdr lista) elem)))))

;14

(define (path n BTS)
  (if (equal? (car BTS) n) '()
      (if (presente? (cadr BTS) n)
          (cons 'left (path n (cadr BTS)))
          (cons 'right (path n (caddr BTS))))))

(path 17
'(14 (7 () (12 () ()))
     (26 (20 (17 () ()) ())
         (31 () ())        )))



