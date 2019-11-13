#lang racket

;main function
(define (expr-compare expr1 expr2)
  (define dict (make-hash))
  (cond
    [(list? expr1) (if (list? expr2)
                       (clean-expr-2 (clean-expr-1 (compare_list expr1 expr2) dict) dict)
                       (cons 'if (cons '% (cons expr1 (cons expr2 '())))))]
    [(list? expr2) (cons 'if (cons '% (cons expr1 (cons expr2 '()))))]
    [else (compare_val expr1 expr2)]))

;compares two values
(define (compare_val val1 val2)
  (cond
    [(equal? val1 val2) (if (equal? val1 'lambda)
                            'λ
                            val1)]
    [(boolean? val1) (if val1
                          '%
                          '(not %))]
    [(or (and (equal? 'lambda val1) (equal? 'λ val2))
         (and (equal? 'lambda val2) (equal? 'λ val1)))
     'λ]
    [(and (list? val1) (list? val2)) (compare_list val1 val2)]
    [else
     (cons 'if (cons '% (cons val1 (cons val2 '()))))]))

;compares each element in two lists
(define (compare_list list1 list2)
  (reverse (compare_list_helper list1 list2 '())))

(define (compare_list_helper list1 list2 acc)
  (cond
    [(or (empty? list1) (empty? list2)) acc]
    [(and (equal? (car list1) 'lambda) (equal? (car list2) 'lambda) (equal? (length list1) (length list2)) (> (length list1) 2)) (compare_lambdas list1 list2)]
    [(equal? (car list1) (car list2)) (if (can_combine (car list1))
                                          (compare_list_helper (cdr list1) (cdr list2) (cons (compare_val (car list1) (car list2)) acc))
                                          (reverse (cons 'if (cons '% (cons list1 (cons list2 '()))))))]
    [(or (equal? (car list1) 'if) (equal? (car list2) 'if)) (reverse (cons 'if (cons '% (cons list1 (cons list2 '())))))]
    [else
     (compare_list_helper (cdr list1) (cdr list2) (cons (compare_val (car list1) (car list2)) acc))]))

;determines if we can combine the objects based on their type
(define (can_combine keyword)
  (cond
    [(equal? keyword 'list) #f]
    [(equal? keyword 'quote) #f]
    [else #t]))

;cleans expression by replacing anything possible
(define (clean-expr-1 expr dict)
  (cond
   [(equal? expr '()) '()]
   [(and (= (length expr) 4) (equal? (car expr) 'if) (equal? (cadr expr) '%)) (and (if (dict-has-key? dict expr)
                                                                                  (dict-set! dict expr (+ 1 (dict-ref dict expr)))
                                                                                  (dict-set! dict expr 1)) expr)]
   [(list? (car expr)) (cons (clean-expr-1 (car expr) dict) (clean-expr-1 (cdr expr) dict))]
   [else (cons (car expr) (clean-expr-1 (cdr expr) dict))]))

(define (clean-expr-2 expr dict)
  (cond
    [(equal? expr '()) '()]
    [(and (= (length expr) 4) (equal? (car expr) 'if) (equal? (cadr expr) '%)) (if (> (dict-ref dict expr) 1)
                                                                                   (string->symbol (string-append (symbol->string (caddr expr)) (string-append "!" (symbol->string (cadddr expr)))))
                                                                                   expr)]
    [(list? (car expr)) (cons (clean-expr-2 (car expr) dict) (clean-expr-2 (cdr expr) dict))]
   [else (cons (car expr) (clean-expr-2 (cdr expr) dict))]))
                                                                                   

;compares two lambda functions
(define (compare_lambdas list1 list2)
  (reverse (cons 'λ (compare_lambdas_helper (cdr list1) (cdr list2)))))

(define (compare_lambdas_helper list1 list2)
  (cond
    [(equal? list1 empty) (if (equal? list2 empty)
                              '()
                              list2)]
    [(equal? list2 empty) list1]
    [(equal? (car list1) (car list2)) (cons (car list1) (compare_lambdas_helper (cdr list1) (cdr list2)))]
    [(and (list? (car list1)) (list? (car list2))) (cons (compare_lambdas_helper (car list1) (car list2)) (compare_lambdas_helper (cdr list1) (cdr list2)))] 
    [else (cons (cons 'if (cons '% (cons (car list1) (cons (car list2) '())))) (compare_lambdas_helper (cdr list1) (cdr list2)))]))

;test cases for expr-compare
#|(expr-compare 12 12)
(expr-compare 12 20)
(expr-compare #t #t)
(expr-compare #f #f)
(expr-compare #t #f)
(expr-compare #f #t)
(expr-compare 'a '(cons a b))
(expr-compare '(cons a b) '(cons a b))
(expr-compare '(cons a lambda) '(cons a λ))
(expr-compare '(cons (cons a b) (cons b c))
              '(cons (cons a c) (cons a c)))
(expr-compare '(cons a b) '(list a b))
(expr-compare '(list) '(list a))
(expr-compare ''(a b) ''(a c))
(expr-compare '(quote (a b)) '(quote (a c)))
(expr-compare '(quoth (a b)) '(quoth (a c)))
(expr-compare '(if x y z) '(if x z z))
(expr-compare '(if x y z) '(g x y z))
(expr-compare '((lambda (a) (f a)) 1) '((lambda (a) (g a)) 2))
(expr-compare '((lambda (a) (f a)) 1) '((λ (a) (g a)) 2))
(expr-compare '((lambda (a) a) c) '((lambda (b) b) d))
(expr-compare ''((λ (a) a) c) ''((lambda (b) b) d))
(expr-compare '(+ #f ((λ (a b) (f a b)) 1 2))
              '(+ #t ((lambda (a c) (f a c)) 1 2)))
(expr-compare '((λ (a b) (f a b)) 1 2)
              '((λ (a b) (f b a)) 1 2))
(expr-compare '((λ (a b) (f a b)) 1 2)
              '((λ (a c) (f c a)) 1 2))
(expr-compare '((lambda (lambda) (+ lambda if (f lambda))) 3)
              '((lambda (if) (+ if if (f λ))) 3))|#
(expr-compare '((lambda (a) (eq? a ((λ (a b) ((λ (a b) (a b)) b a))
                                    a (lambda (a) a))))
                (lambda (b a) (b a)))
              '((λ (a) (eqv? a ((lambda (b a) ((lambda (a b) (a b)) b a))
                                a (λ (b) a))))
                (lambda (a b) (a b))))