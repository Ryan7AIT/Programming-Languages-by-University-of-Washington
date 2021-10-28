
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

(define (sequence l h s)
  (let ([x l])
    (if (> x  h)
        null
        (cons l (sequence (+ l s) h s)))))


(define (string-append-map xs s)
  (if (null? xs)
      xs
      (map (lambda (s1) (string-append s1 s))  xs)))

(define (list-nth-mod xs n )
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(null? xs) (error  "list-nth-mod: empty list" )]
        [else
         (let ([i (remainder n (length xs))])
           (car (list-tail xs i)))]))

 
(define (stream-for-n-steps stream n)
  (if (= n 0)
      null
      (cons (car (stream)) (stream-for-n-steps (cdr (stream)) (- n 1)))))


(define funny-number-stream
  (letrec ([f (lambda (x) (if (= 0 (remainder x 5 )) (cons (- 0 x) (lambda () (f (+ x 1))))  (cons x (lambda () (f (+ x 1))))))])
    (lambda () (f 1))))
 

(define dan-then-dog
  (letrec ([f (lambda (s1 s2) (cons s1 (lambda () (f s2 s1))))])
    (lambda () (f "dan.jpg" "dog.jpg"))))


(define (stream-add-zero s)
  (lambda () (cons (cons 0 (car (s))) (stream-add-zero (cdr (s))))))


(define (cycle-lists xs ys)
  (letrec ([helper (lambda (n) (+ n 1))]
           [f (lambda (x y i) (cons (cons (list-nth-mod x i) (list-nth-mod y i)) (lambda () (f x y (helper i)))))])
    (lambda () (f xs ys 0)))) 




(define (vector-assoc v vect) 
  (letrec ([helper (lambda (pos vect) (cond [(< pos (vector-length vect))
                                             (if (and (pair? (vector-ref vect pos)) (equal? (car (vector-ref vect pos)) v))
                                                 (vector-ref vect pos)
                                                 (helper (+ 1 pos) vect))]
                                            [else
                                             #f]))])
    (helper 0 vect))) 




(define (cached-assoc xs n)
  (letrec ([memo (make-vector n #f)]
           [iter 0]
           [f (lambda (val)
               (let ([ans (vector-assoc val memo)])
                 (if ans
                     (cdr ans)
                     (let ([new-ans (assoc val xs)])
                       (begin
                         (vector-set! memo iter (cons val new-ans))
                         (set! iter (if (= (+ 1 iter) n) 0 (+ iter 1)))
                             new-ans)))))])
    (lambda (v) (f v))))
















             





