#lang racket
;a is (car (choose-from)) and b is (cdr (choose-from))
(provide (all-defined-out))
(define-syntax lc
  (syntax-rules (: <- @)
    [(lc expr : var <- drawn-from) (map (lambda (var) expr) drawn-from)]
    [(lc expr : @ guard) (if guard (list expr) `())]
    [(lc expr : @ guard  qualifier ...) 
     (append* (lc (lc expr : qualifier ...) : @ guard))]
    [(lc expr : var <- drawn-from  qualifier ...) 
     (append* (lc (lc expr :  qualifier ... ) : var <- drawn-from))]))

(define (zip l1 l2)
  (if (or (null? l1) (null? l2)) '()
      (cons (cons (car l1) (car l2)) (zip (cdr l1) (cdr l2)))))

(define (choose m lst)        ;form list of lists of m elemnts taken from list lst and duplicates are also allowed 
    (if (= m 1) (map (lambda (x) (list x)) lst)
        (let ([l (choose (- m 1) lst)])
          (append* (lc (map (lambda (y) (cons x y)) l) : x <- lst)))))

   (define n 6) ;(total balls))
   (define m 4) ;(balls chosen))
   (define pair (cons 0 0))
  ; (define guess-no 1)

(define (set-balls! x y z)
  (set! n x) (set! m y) (set! pair z))
   
;  (define (no-duplicate-list list)
;    (if (null? list) '()
;           (append (car list) (no-duplicate-list (remove* (list (car l)) list)))))

  ;(define (comb a l)   ;choose a elements from the list 'l' of n elements.
;     (cond [(= a 0) '(())]
;           [(= (length l) a) (list l)]
;           [else (append (map (lambda (x) (cons (car l) x)) (comb (cdr l) (- a 1))) (comb (cdr l) n))]))

  (define (form-input-list a i)        ;form a lists which will be used for forming our 1st guess
    ;(set! guess-no 1)
    (cond [(= a 0) '()]
          [(= a 1) (list i)]
          [(cons i (cons i (form-input-list (- a 2) (+ i 1))))]))

;(define (num_possbl a b) (expt b a))
;   (define (listt a b) (build-list b (lambda (x) (+ x 1))))
;   (define (possbl a b) (choose a (listt a b)))
 
(define num_possbl (expt n m))
(define listt (build-list n (lambda (x) (+ x 1))))
(define possbl (choose m listt))
(define (match lst)                        ;gives the matching of car and cdr of list which is formed by zipping 2 list
     (length (filter (lambda (x) (if (= (car x) (cdr x)) #t #f)) lst)))


  (define (find-num-white a b cnt)
    (if (null? a) cnt
        (if (member (car a) b) (find-num-white (remove (car a) a) (remove (car a) b) (+ cnt 1)) (find-num-white (remove (car a) a) b cnt)))) 

  (define (pegs-pair x y)             ;return a cons value whose car gives white balls and cdr gives black balls,  x is guessed value y is supposed answer
     (let* (;[sorted-x (sort x <)]
            ;[sorted-y (sort y <)]
            ;[lst1 (zip sorted-x sorted-y)]
            [lst2 (zip x y)]
            [p (find-num-white x y 0)]
            [q (match lst2)])
        (cons (- p q) q)))
    


    (define (check l1 l2 consed-value)
      (if (equal? (pegs-pair l1 l2) consed-value) #f #t))

    ;(input (form-input-list m 1))
    ;(let* ([pair (given-output (input (form-input-list m 1)))]    ;car pair gives white balls cdr gives black balls
           ;[modified-possbl (filter (lambda (x) (check (form-input-list m 1) x pair)) possbl)])   ;check returns #t if the guess and ans doesnt satisfy pair
    
    ;(define (modified-possbl pair a b) (filter (lambda (x) (not (check (form-input-list a 1) x pair))) (possbl a b)))


 (define modified-possbl possbl)  ;2
(define guess (form-input-list m 1))  ;1

(define cnt 0)
    (define (what-to-guess)
      (begin 
             (displayln cnt)
             (displayln m)
             (set! cnt (+ 1 cnt)))

      (set! modified-possbl (filter (lambda (x) (not (check guess x pair))) modified-possbl))   ;4
      (define current-minimum (expt 10 12))
      (define current-maximum 0)

      (define (minimum-of-all a lst)      ; a is the guess
         (if (null? lst) (begin (set! current-maximum current-minimum) (set! current-minimum (expt 10 12)) current-maximum)
            (let ([current-num (length (filter (lambda (y) (check a y (pegs-pair a (car lst)))) modified-possbl))])
              (if (< current-num current-maximum) 0    ;(minimum-of-all a (cdr lst))
                   (if (< current-num current-minimum) (begin (set! current-minimum current-num) (minimum-of-all a (cdr lst)))
                                     (minimum-of-all a (cdr lst))))))) 

      (define (maximum-of-all lst)
        (if (null? lst) '()
           (let ([m (minimum-of-all (car lst) modified-possbl)])
             (cons (cons (car lst) m) (maximum-of-all (cdr lst))))))
      
(cond [(= (length modified-possbl) 1) (begin ;(set! guess-no (+ 1 guess-no) )
                                        (begin0 (car modified-possbl); (input (car modified-possbl))
                                              (displayln (car modified-possbl))
                                             (displayln "that was your pattern right?")))]  ;just write inpu here in pace of displayln
      [(let* ([guess-list (reverse (maximum-of-all possbl))])
              (set! guess (caar (dropf guess-list (lambda (x) (= (cdr x) 0)))))   ;3
       ;(define guess-list (reverse (maximum-of-all possbl))) (define guess (caar (dropf guess-list (lambda (x) (= (cdr x) 0)))))
          ; gives the input of our guess
      (begin ;(set! guess-no (+ 1 guess-no))
        (begin0 guess;(set! pair (pegs-pair guess '(3 4 5 6))) ;(input guess) ;(set! pair (given-output) ;(input guess)))
                                                                ; set pair to the cons value returned by the user
      ;(set! modified-possbl (filter (lambda (x) (not (check guess x pair))) modified-possbl)) ;(what-to-guess)   ;5
      )))])) ;(what-to-guess))]))  ; the list modified-possbl gets modified-further)



