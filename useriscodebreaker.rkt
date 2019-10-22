#lang racket
(provide set-balls2!
         pegs-pair2
         choose-noduplicates
         choose-duplicates
         choose-duplicates-empty)
         

(define (match lst)                        ;gives the matching of car and cdr of list which is formed by zipping 2 list
  (length (filter (lambda (x) (if (= (car x) (cdr x)) #t #f)) lst)))

(define (find-num-white a b cnt)
  (if (null? a) cnt
      (if (member (car a) b) (find-num-white (remove (car a) a) (remove (car a) b) (+ cnt 1)) (find-num-white (remove (car a) a) b cnt))))
(define (zip l1 l2)
  (if (or (null? l1) (null? l2)) '()
      (cons (cons (car l1) (car l2)) (zip (cdr l1) (cdr l2)))))

(define (pegs-pair2 x y)             ;return a cons value whose car gives white balls and cdr gives black balls,  x is guessed value y is supposed answer
  (let* (;[sorted-x (sort x <)]
         ;[sorted-y (sort y <)]
         ;[lst1 (zip sorted-x sorted-y)]
         [lst2 (zip x y)]
         [p (find-num-white x y 0)]
         [q (match lst2)])
    (cons (- p q) q)))

(define n 6);(total-balls))
(define m 4);(balls-chosen))

(define (set-balls2! x y)
  (set! n x) (set! m y))

(define lst (build-list (+ n 1) (lambda (x) x)))

(define ans-ball-list '())

(define cnt 0)

(define (check a lst)
  (if (null? lst) (begin0 (if (>= cnt 2) #t #f) (set! cnt 0))
      (if (equal? a (car lst)) (begin (set! cnt (+ cnt 1)) (check a (cdr lst))) (check a (cdr lst)))))

(define (choose-noduplicates m n)    ;genrates a initial code with no-duplicates
  (set! lst (build-list (+ n 1) (lambda (x) x)))
  (if (= m 0) (begin0 ans-ball-list (set! ans-ball-list '()))
      (let ([a (list-ref (cdr lst) (random n))])
        (if (member a ans-ball-list) (choose-noduplicates m n) (begin (set! ans-ball-list (cons a ans-ball-list)) (choose-noduplicates (- m 1) n))))))

(define (choose-duplicates m n)      ;genrates a initial code with duplicates.
  (set! lst (build-list (+ n 1) (lambda (x) x)))
  ;(build-list m (lambda (x) (+ 1 (random n))))
  (if (= m 0) (begin0 ans-ball-list (set! ans-ball-list '()))
      (let ([a (list-ref (cdr lst) (random n))])
        (if (or (check-duplicates ans-ball-list) (check a ans-ball-list)) (choose-noduplicates m n) (begin (set! ans-ball-list (cons a ans-ball-list)) (choose-duplicates (- m 1) n))))))

(define (choose-duplicates-empty m n)      ;genrates a initial code with duplicates+empty.
  ;(build-list m (lambda (x) (random n))))
  (set! lst (build-list (+ n 1) (lambda (x) x)))
  (if (= m 0) (begin0 ans-ball-list (set! ans-ball-list '()))
      (let ([a (list-ref lst (random n))])
        (if (= a 0) (if (member a ans-ball-list) (choose-duplicates m n) (begin (set! ans-ball-list (cons a ans-ball-list)) (choose-duplicates (- m 1) n)))
            (if (or (check-duplicates ans-ball-list) (check a ans-ball-list)) (choose-duplicates-empty m n) (begin (set! ans-ball-list (cons a ans-ball-list)) (choose-duplicates-empty (- m 1) n)))))))
  
  
;(define list-of-balls (player-input balls))  ;list of numbers corresponding to colours
;(define pair (pegs-pair list-of-balls ans-ball-list));
;(give-output pair) ;car contains white balls cdr contains balck-ball