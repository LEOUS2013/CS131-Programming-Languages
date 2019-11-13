#lang racket
;map function
(define (my_map func list)
  (my_map_helper func list empty))
  
(define (my_map_helper func list acc)
  (cond [(equal? list '()) (reverse acc)]
        [else (my_map_helper func (cdr list) (cons (func (car list)) acc))]))

;reverses a list
(define (my_rev list)
  (my_rev_helper list '()))

(define (my_rev_helper list acc)
  (cond
    [(equal? list empty) acc]
    [else (my_rev_helper (cdr list) (cons (car list) acc))]))

;removes consecutive duplicate values
(define (my_remove_consec list)
  (reverse (my_remove_consec_helper list '())))

(define (my_remove_consec_helper list acc)
  (cond
    [(equal? list empty) acc]
    [(equal? (cdr list) empty) (cons (car list) acc)]
    [(equal? (car list) (cadr list)) (my_remove_consec_helper (cdr list) acc)]
    [else (my_remove_consec_helper (cdr list) (cons (car list) acc))]))

;determines if two lists are equal
(define (my_equal list1 list2)
  (cond
    [(equal? list1 empty) (equal? list2 empty)]
    [(equal? list2 empty) (equal? list1 empty)]
    [(equal? (car list1) (car list2)) (my_equal (cdr list1) (cdr list2))]
    [else (#f)]))

#|
12
'(if % 12 20)
#t
#f
'%
'(not %)
'(if % a (cons a b))
'(cons a b)
'(cons a λ)
'(cons (cons a (if % b c)) (cons (if % b a) c))
'((if % cons list) a b)
'(if % (list) (list a))
'(if % '(a b) '(a c))
'(if % '(a b) '(a c))
'(quoth (a (if % b c)))
'(if x (if % y z) z)
'(if % (if x y z) (g x y z))
'((λ (a) ((if % f g) a)) (if % 1 2))
'((λ (a) ((if % f g) a)) (if % 1 2)) 
'((λ (a!b) a!b) (if % c d))      
'(if % '((λ (a) a) c) '((lambda (b) b) d))
'(+
     (not %)
     ((λ (a b!c) (f a b!c)) 1 2))
'((λ (a b) (f (if % a b) (if % b a))) 1 2)
'((λ (a b!c) (f (if % a b!c) (if % b!c a)))   ########################
'((λ (lambda!if) (+ lambda!if (% if lambda!if (f (if % lambda!if λ))) 3)
'((λ (a)
      ((if % eq? eqv?)
       a
       ((λ (a!b b!a) ((λ (a b) (a b)) (if % b!a a!b) (if % a!b b!a)))
        a (λ (a!b) (if % a!b a)))))
     (λ (b!a a!b) (b!a a!b)))
|#
