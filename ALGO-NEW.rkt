#lang racket
(provide form-input-list2
         what-to-guess-genetic
         genetic-set-balls!)

(define-syntax lc                                  ;;same
  (syntax-rules (: <- @)
    [(lc expr : var <- drawn-from) (map (lambda (var) expr) drawn-from)]
    [(lc expr : @ guard) (if guard (list expr) `())]
    [(lc expr : @ guard  qualifier ...) 
     (append* (lc (lc expr : qualifier ...) : @ guard))]
    [(lc expr : var <- drawn-from  qualifier ...) 
     (append* (lc (lc expr :  qualifier ... ) : var <- drawn-from))]))
(define (zip l1 l2)                                 ;;;;;same
  (if (or (null? l1) (null? l2)) '()
      (cons (cons (car l1) (car l2)) (zip (cdr l1) (cdr l2)))))

(define-syntax while
  (syntax-rules ()
    [(while conditions body) (begin (define (iter) (cond [condtions (begin body (iter))])) (iter))]
    [(while conditions body more ...) (begin (define (iter) (cond [conditions (begin body more ... (iter))])) (iter))]))

;(define (choose m lst)        ;form list of lists of m elemnts taken from list lst and duplicates are also allowed    ;working
;  (if (= m 1) (map (lambda (x) (list x)) lst)
;      (let ([l (choose (- m 1) lst)])
;        (append* (lc (map (lambda (y) (cons x y)) l) : x <- lst)))))
;(define guess-no 1)

(define n 8);(total balls))
(define m 5);(balls chosen))
;(define listt (build-list n (lambda (x) (+ x 1))))
;(define possbl (choose m listt))

(define pair (cons 0 0))
(define consed-value '())  ;give a list containing cons value whose car contains a list and cdr gives a pair)
;(define pair (pegs-pair (form-input-list2 m 1) '(2 7 1 4 5 6 8 9 ))) ;(given-output (input (form-input-list m 1))))   ;pair contains value of X1 Y1
;(set! consed-value (append (list (cons (form-input-list2 m 1) pair)) consed-value))


(define (genetic-set-balls! x y z)
  (set! n x) (set! m y) (set! pair z) (set! consed-value (cons (cons guess pair) consed-value)))
  

(define (form-input-list2 a i)        ;form a lists which will be used for forming our 1st guess    ;working
  ;(set! guess-no 1)
  (cond [(= a 0) '()]
        [(= a 1) (list i)]
        [(= a 2) (list i (+ i 1))]
        [(= a 3) (list i (+ i 1) (+ i 2))]
        [(cons i (cons i (form-input-list2 (- a 2) (+ i 1))))]))

(define (match lst)                      ;;same  ;gives the matching of car and cdr of list which is formed by zipping 2 list    ;w
  (length (filter (lambda (x) (if (= (car x) (cdr x)) #t #f)) lst)))

(define (find-num-white a b cnt)   ;w          ;;same
    (if (null? a) cnt
        (if (member (car a) b) (find-num-white (remove (car a) a) (remove (car a) b) (+ cnt 1)) (find-num-white (remove (car a) a) b cnt))))

(define (pegs-pair x y)             ;;same ;return a cons value whose car gives white balls and cdr gives black balls,  x is guessed value y is supposed answer
  (let* (;[sorted-x (sort x <)]
         ;[sorted-y (sort y <)]
         ;[lst1 (zip sorted-x sorted-y)]
         [lst2 (zip x y)]
         [p (find-num-white x y 0)]
         [q (match lst2)])
    (cons (- p q) q)))

;(input (form-input-list m 1))  ;gives input the initial guess

 
(define my-list '())

(define (form-list a m)
  (if (= a 0) my-list
      (let ([l (build-list m (lambda (x) (+ 1 (random n))))])
        (cond [(not (member l my-list)) (begin (set! my-list (cons l my-list)) (form-list (- a 1) m))]
              [else (form-list a m)]))))
       
;(define (apply-each l1 l2 cnt)
;  (if (null? l2) cnt
;      (let ([pair (pegs-pair x (car l2))]
;       [white-balls (car pair)]
;       [black-balls (cdr pair)]
;       [num (+ white-balls black-balls)]
;         (apply-each l1 (cdr l2) (+ cnt num))))))



(define (add-to-E l1 l2)
  (if (null? l1) l2
      (if (not (member (car l1) l2)) (add-to-E (cdr l1) (append (list (car l1)) l2))
          (add-to-E (cdr l1) l2))))

(define (sum l)
  (foldr + 0 l))

(define (split lst1 lst2 number)
  (if (= number 0) (cons lst2 lst1)
      (split (cdr lst1) (append lst2 (list (car lst1))) (- number 1))))

(define (single-point-co l1 l2)
  (let* ([a (+ (random (- m 1)) 1)]
        [consed-list1 (split l1 '() a)]
        [consed-list2 (split l2 '() a)])
    (cons (append (car consed-list1) (cdr consed-list2)) (append (car consed-list2) (cdr consed-list1)))))

(define (choose-2-diff-pos)
  (let* ([num1 (+ (random (- m 1)) 1)]
         [num2 (+ (random (- m 1)) 1)])
    (if (not (= num1 num2)) (cons (min num1 num2) (max num1 num2))
        (choose-2-diff-index))))

(define (double-point-co l1 l2)
  (let* ([pair (choose-2-diff-pos)]
         [a (car pair)]
         [b (cdr pair)]
         [consed-list1 (split l1 '() a)]
         [consed-lst1 (split (cdr consed-list1) '() (- b a))]
         [consed-list2 (split l2 '() a)]
         [consed-lst2 (split (cdr consed-list2) '() (- b a))])
    (cons (append (car consed-list1) (car consed-lst2) (cdr consed-lst1)) (append (car consed-list2) (car consed-lst1) (cdr consed-lst2)))))    


(define (do-single-point-crossover lst)
 (if (or (= (length lst) 1) (= (length lst) 0)) lst
      (let ([consed-list (single-point-co (first lst) (second lst))])
         (append (list (car consed-list)) (list (cdr consed-list)) (do-single-point-crossover (cddr lst))))))
;  (append* (map (lambda (x) (let ([lstt (remove x lst)]) (append* (map (lambda (y) (let ([consed-list (single-point-co x y)])
;                                                                      (append (list (car consed-list)) (list (cdr consed-list))))) lstt)))) lst)))

(define (do-double-point-crossover lst)
 (if (or (= (length lst) 1) (= (length lst) 0)) lst
      (let ([consed-list (double-point-co (first lst) (second lst))])
         (append (list (car consed-list)) (list (cdr consed-list)) (do-double-point-crossover (cddr lst))))))
;  (append* (map (lambda (x) (let ([lstt (remove x lst)]) (append* (map (lambda (y) (let ([consed-list (double-point-co x y)])
;                                                                      (append (list (car consed-list)) (list (cdr consed-list))))) lstt)))) lst)))

(define (do-crossover lst)
  (let* ([num (length lst)]
         [lngth (quotient num 2)]
         [consed-list (split lst '() lngth)])
    (append (do-single-point-crossover (car consed-list)) (do-double-point-crossover (cdr consed-list)))))

(define (mutate lst)   ;lst is simply a list
  (list-set lst (random m) (+ 1 (random n))))

(define (do-mutation lst)     ;lst is a list of list
  (map (lambda (x) (mutate x)) lst))

(define (choose-2-diff-index-value lst)
  (let* ([num1 (random m)]
        [value1 (list-ref lst num1)]
        [num2 (random m)]
        [value2 (list-ref lst num2)])
    (if (not (= num1 num2)) (cons (cons num1 value1) (cons num2 value2))
        (choose-2-diff-index-value lst))))

(define (permute lst) ;lst is simply a list
   (let* ([consed-value (choose-2-diff-index-value lst)]    ;car again contains a consed value whose car gives index and cdr gives value at that index similarly cdr
         [lst1 (list-set lst (caar consed-value) (cddr consed-value))]
         [lst2 (list-set lst1 (cadr consed-value) (cdar consed-value))])
     lst2))

(define (do-permutation lst)    ;lst is a list of list
  (map (lambda (x) (permute x)) lst))

(define (choose-2-diff-index)
  (let* ([num1 (random m)]
         [num2 (random m)])
    (if (not (= num1 num2)) (cons (min num1 num2) (max num1 num2))
        (choose-2-diff-index))))

(define (first-list lst1 lst2 p)
  (if (= p 0) (let ([a (car lst2)])
                (cons (append lst1 (list a)) (cdr lst2)))
       (let ([a (car lst2)])
           (first-list (append lst1 (list a)) (cdr lst2) (- p 1)))))
;(define (first-list lst1 lst2 p)
;  (if (= p 0) (let ([a (car lst2)])
;                (begin (set! lst2 (cdr lst2)) (cons a lst1)))
;       (let ([a (car lst2)])
;          (begin (set! lst2 (cdr lst2)) (first-list (cons a lst1) lst2 (- p 1))))))

(define (reverse-between lst p q)
  (if (or (= (- q p) 2) (= (- q p) 1)) lst
      (let* ([consed-list1 (first-list '() lst p)]
             [lst1 (car consed-list1)]
             [consed-list2 (first-list '() (cdr consed-list1) (- (- q p) 2))]
             [lst2 (car consed-list2)])
        (append lst1 (reverse lst2) (cdr consed-list2)))))
;(define (reverse-between lst p q)
;  (if (or (= (- q p) 2) (= (- q p) 1)) lst
;      (let* ([lst1 (first-list '() lst p)]
;             [lst2 (first-list '() lst (- (- q p) 2))])
;        (append lst1 (reverse lst2) lst))))

(define (invert lst)
  (let* ([consed-value (choose-2-diff-index)]
         [lstt (reverse-between lst (car consed-value) (cdr consed-value))])
    lstt))

(define (do-inversion lst)
  (map (lambda (x) (invert x)) lst))

(define (make-a-list l1 l2 num)
  (if (= num 0) (cons l1 l2)
      (let ([a (list-ref l2 (random (length l2)))])
        (make-a-list (cons a l1) (remove a l2) (- num 1)))))

(define (create-new-populatn lst) ;lst is a list of list
  (let* ([mod-lst (do-crossover lst)]
        [a (length mod-lst)]
        [x (floor (* 0.03 a))]
        [y (floor (* 0.02 a))])
  
    (define consed-lista (make-a-list '() mod-lst x))
      (define lst1 (car consed-lista))   ;list for mutation
       ;(set! mod-lst (remove* lst1 mod-lst))

    (define consed-listb (make-a-list '() (cdr consed-lista) x))
      (define lst2 (car consed-listb))   ;list for permutation
    ;(define (add-to-lst lsta lstb)
      ;(let ([m (list-ref mod-lst (random a))])
        ;(if (not (member m lstb)) (cons
    ;build-list n (lambda (x) (let ([lstt (list-ref mod-lst (random a))]))(do-permutation mod-lst2))

    ;(set! mod-lst (remove* lst2 mod-lst))

    (define consed-listc (make-a-list '() (cdr consed-listb) y))   
      (define lst3 (car consed-listc))   ;list for inversion

    ;(set! mod-lst (remove* lst3 mod-lst))

    (define updated-part-list (append (do-mutation lst1) (do-permutation lst2) (do-inversion lst3) (cdr consed-listc)))

    (define (add lst)
      (let ([lstt (build-list m (lambda (x) (+ 1 (random n))))])
        (if (not (member lstt lst)) (cons lstt lst)
            (add lst))))

    (define (add-to-list lst1 lst3 lst2)
      (if (null? lst1) lst3
          (if (not (member (car lst1) lst2)) (begin (set! lst3 (cons (car lst1) lst3)) (add-to-list (cdr lst1) lst3 lst2)) (add-to-list (cdr lst1) (add lst3) lst2))))

      (add-to-list updated-part-list '() lst)))
  

(define (is-eligible? lst)
  (andmap (lambda (x) (equal? (pegs-pair lst (car x)) (cdr x))) consed-value))           
  

(define (what-to-guess eligible-sol-list)
  (if (= (length eligible-sol-list) 1) (car eligible-sol-list)
      (let* ([final-list (map (lambda (x) (let ([new-sol-list (remove x eligible-sol-list)])        ;(andmap (lambda (x) (equal? (pegs-pair (car x) y) (pegs-pair z (car x)))) consed-value)
                                        (cons x (sum (map (lambda (y) (length (filter (lambda (z) (equal? (pegs-pair x y) (pegs-pair z x))) (remove y new-sol-list)))) new-sol-list))))) eligible-sol-list)]
             [CURRENT-MIN (cdr (car final-list))]
             [my-guess (caar final-list)])
        ;(displayln final-list)
     (begin (map (lambda (x) (cond [(< (cdr x) CURRENT-MIN) (begin (set! CURRENT-MIN (cdr x)) (set! my-guess (car x)))])) final-list) my-guess)))) 



(define i 1)
(define guess (form-input-list2 m 1))

(define (what-to-guess-genetic)
  (if (not (equal? (cdr pair) m)) (begin (set! i (+ i 1))
                                         (let* ([E '()]
                                               [h 1]
                                               [population (form-list 150 m)])      ;population is a list of list
                                           
                                           (while (and (if (= n 12) (<= h 200) (<= h 100)) (if (= n 12) (<= (length E) 100) (if (= n 9) (<= (length E) 80) (<= (length E) 60))))
                                             (set! population (create-new-populatn population));new population generated..
                                             
                                             (define fitness-function-list (filter (lambda (x) (is-eligible? x)) population))
                                             (set! E (add-to-E fitness-function-list E))
                                             ;(set! population E) 
                                             (set! h (+ h 1)))
                                           (displayln E)
                                           (if (null? E) (what-to-guess-genetic)
                                           (begin (set! guess (what-to-guess E))
                                           (displayln guess)
                                           ;(set! guess-no (+ 1 guess-no))
                                           ;(input guess)
                                           ;(set! pair (pegs-pair guess '(2 7 1 4 5 6 8 9))) ;(given-output (input guess)))
                                           ;(set! consed-value (cons (cons guess pair) consed-value))
                                           guess))))
         (begin ;(set! guess-no (+ 1 guess-no))
           (displayln guess) guess)))
;(while (not (equal? (cdr pair) m))
;       (set! i (+ i 1))
;       (define E '())
;       (define h 1)
;       (define population (form-list 150 m))      ;population is a list of list
;       (while (and (<= h 100) (<= (length E) 60))
;              (set! population (create-new-populatn population));new population generated..
;              (define fitness-function-list (filter (lambda (x) (is-eligible? x)) population))
;              (set! E (add-to-E fitness-function-list E))
;              (set! population E) 
;              (set! h (+ h 1)))
;       (define guess (what-to-guess E))
;       (input guess)
;       (set! pair (given-output (input guess)))
;       (set! consed-value (append (list (cons guess pair)) consed-value)))
  
         
          
       

    
      
