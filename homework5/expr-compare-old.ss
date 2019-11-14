#lang racket

;main function
(define (expr-compare expr1 expr2)
  (define dict (dict-set #hash() '() '()))
  (cond
    [(list? expr1) (if (list? expr2)
                       (clean-expr-2 (compare_list expr1 expr2) (clean-expr-1 (compare_list expr1 expr2) dict))
                       (cons 'if (cons '% (cons expr1 (cons expr2 '())))))]
    [(list? expr2) (cons 'if (cons '% (cons expr1 (cons expr2 '()))))]
    [else (compare_val expr1 expr2)]))

;compares two values
(define (compare_val val1 val2)
  (cond
    [(equal? val1 val2) val1]
    [(boolean? val1) (if val1
                          '%
                          '(not %))]
    #|[(or (and (equal? 'lambda val1) (equal? 'λ val2))
         (and (equal? 'lambda val2) (equal? 'λ val1)))
     'λ]|#
    [(and (list? val1) (list? val2)) (compare_list val1 val2)]
    [else
     (cons 'if (cons '% (cons val1 (cons val2 '()))))]))

;compares each element in two lists
(define (compare_list list1 list2)
  (reverse (compare_list_helper list1 list2 '())))

(define (compare_list_helper list1 list2 acc)
  (cond
    [(not (equal? (length list1) (length list2))) (reverse (cons 'if (cons '% (cons list1 (cons list2 '())))))]
    [(or (empty? list1) (empty? list2)) acc]
    [(and (is_lambda (car list1)) (is_lambda (car list2)) (equal? (length list1) (length list2)) (> (length list1) 2)) (reverse (compare_lambdas list1 list2))]
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
   [(equal? expr '()) dict]
   [(and (list? (car expr)) (= (length (car expr)) 4) (equal? (caar expr) 'if) (equal? (cadr (car expr)) '%))
                           (if (dict-has-key? dict (car expr))
                               (let ([new-dict (dict-set dict (car expr) (+ 1 (dict-ref dict (car expr))))]) (clean-expr-1 (cdr expr) new-dict))
                               (let ([new-dict (dict-set dict (car expr) 1)]) (clean-expr-1 (cdr expr) new-dict)))]
   [(list? (car expr)) (clean-expr-1 (cdr expr) (clean-expr-1 (car expr) dict))]
   [else (clean-expr-1 (cdr expr) dict)]))

(define (clean-expr-2 expr dict)
  (cond
    [(equal? expr '()) '()]
    [(and (= (length expr) 4) (equal? (car expr) 'if) (equal? (cadr expr) '%)) (if (and (dict-has-key? dict expr) (> (dict-ref dict expr) 1))
                                                                                   (make_bound (caddr expr) (cadddr expr))
                                                                                   expr)]
    [(list? (car expr)) (cons (clean-expr-2 (car expr) dict) (clean-expr-2 (cdr expr) dict))]
   [else (cons (car expr) (clean-expr-2 (cdr expr) dict))]))
                                                                                   

;compares two lambda functions
(define (compare_lambdas list1 list2)
  (define dict (dict-set #hash() '() '()))
  (let ([new-dict1 (populate_dict1 (cadr list1) (cadr list2) dict)]
        [new-dict2 (populate_dict2 (cadr list1) (cadr list2) dict)])
  (cons 'λ (cons (compare_lambdas_helper (modify1 (cadr list1) new-dict1) (modify2 (cadr list2) new-dict2) '())
                 (compare_lambdas_helper (modify1 (cddr list1) new-dict1) (modify2 (cddr list2) new-dict2) '())))))

(define (compare_lambdas_helper list1 list2 acc)
  (cond
    [(or (empty? list1) (empty? list2)) (reverse acc)]
    [(and (list? (car list1)) (list? (car list2))) (if (and (is_lambda (caar list1)) (is_lambda (caar list2)))
                                                       (compare_lambdas_helper (cdr list1) (cdr list2) (cons (compare_lambdas (car list1) (car list2)) acc))
                                                       (compare_lambdas_helper (cdr list1) (cdr list2) (cons (compare_lambdas_helper (car list1) (car list2) '()) acc)))]
    [(equal? (car list1) (car list2))
     (compare_lambdas_helper (cdr list1) (cdr list2) (cons (car list1) acc))]
    [else
     (compare_lambdas_helper (cdr list1) (cdr list2) (cons (compare_val (car list1) (car list2)) acc))]))

;checks if both values are lambda
(define (both_lambda val1 val2)
  (cond
    [(and (is_lambda val1) (is_lambda val2)) #t]
    [else #f]))

;checks if single value is lambda
(define (is_lambda val)
  (cond
    [(or (equal? val 'λ) (equal? val 'lambda)) #t]
    [else #f]))

;makes two variables bound
(define (make_bound var1 var2)
  (string->symbol (fix_bound (string-append (symbol->string var1) (string-append "!" (symbol->string var2))))))

;fixes the case where bound variables actually don't need to be
(define (fix_bound bound_str)
  (let ([substr_list (string-split bound_str "!")]) 
  (cond
    [(equal? (list-ref substr_list 0) (list-ref substr_list (- (length substr_list) 1)))
     (list-ref substr_list 0)]
    [else
     bound_str])))

;modifies list1 for bound variables
(define (modify1 list dict)
  (cond
    [(equal? empty list)
     '()]
    [(symbol? list) (if (dict-has-key? dict list)
                        (cons (make_bound list (dict-ref dict list)) '())
                        (cons list '()))]
    [(list? (car list)) (cons (modify1 (car list) dict) (modify1 (cdr list) dict))]
    [(dict-has-key? dict (car list))
     (cons (make_bound (car list) (dict-ref dict (car list))) (modify1 (cdr list) dict))]
    [else
     (cons (car list) (modify1 (cdr list) dict))]))

;modifies list2 for bound variables
(define (modify2 list dict)
  (cond
    [(equal? empty list)
     '()]
    [(symbol? list) (if (dict-has-key? dict list)
                        (cons (make_bound list (dict-ref dict list)) '())
                        (cons list '()))]
    [(list? (car list)) (cons (modify2 (car list) dict) (modify2 (cdr list) dict))]
    [(dict-has-key? dict (car list))
     (cons (make_bound (dict-ref dict (car list)) (car list)) (modify2 (cdr list) dict))]
    [else
     (cons (car list) (modify2 (cdr list) dict))]))

;populates dictionary1 with mappings from first lambda's arguments to second lambda's arguments
(define (populate_dict1 args1 args2 dict)
  (cond
    [(or (equal? args1 empty) (equal? args2 empty)) dict]
    [(and (symbol? args1) (symbol? args2))
     (if (equal? args1 args2)
         dict
         (dict-set dict args1 args2))]
    [(equal? (car args1) (car args2))
     (populate_dict1 (cdr args1) (cdr args2) dict)]
    [else
     (let ([new-dict (dict-set dict (car args1) (car args2))]) 
     (populate_dict1 (cdr args1) (cdr args2) new-dict))]))

;populates dictionary2 with mappings from second lambda's arguments to first lambda's arguments
(define (populate_dict2 args1 args2 dict)
  (cond
    [(or (equal? args1 empty) (equal? args2 empty)) dict]
    [(and (symbol? args1) (symbol? args2))
     (if (equal? args1 args2)
         dict
         (dict-set dict args2 args1))]
    [(equal? (car args1) (car args2))
     (populate_dict2 (cdr args1) (cdr args2) dict)]
    [else
     (let ([new-dict (dict-set dict (car args2) (car args1))]) 
     (populate_dict2 (cdr args1) (cdr args2) new-dict))]))



;test cases for expr-compare
(expr-compare 12 12)
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
              '((lambda (if) (+ if if (f λ))) 3))
(expr-compare '((lambda (a) (eq? a ((λ (a b) ((λ (a b) (a b)) b a))
                                    a (lambda (a) a))))
                (lambda (b a) (b a)))
              '((λ (a) (eqv? a ((lambda (b a) ((lambda (a b) (a b)) b a))
                                a (λ (b) a))))
                (lambda (a b) (a b))))

;TA's test cases start here
(expr-compare '(λ (x) ((λ (x) x) x))
              '(λ (y) ((λ (x) y) x)))
(expr-compare '(((λ (g)
                   ((λ (x) (g (lambda () (x x))))     ; This is the way we define a recursive function
                    (λ (x) (g (lambda () (x x))))))   ; when we don't have 'letrec'
                 (λ (r)                               ; Here (r) will be the function itself
                   (λ (n) (if (= n 0)
                              1
                              (* n ((r) (- n 1))))))) ; Therefore this thing calculates factorial of n
                10)
              '(((λ (x)
                   ((λ (n) (x (lambda () (n n))))
                    (λ (r) (x (lambda () (r r))))))
                 (λ (g)
                   (λ (x) (if (= x 0)
                              1
                              (* x ((g) (- x 1)))))))
                9))
(expr-compare '(lambda (a b) (a b c)) '(lambda (x y) x))