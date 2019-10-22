#lang racket

(require 2htdp/image)
(require 2htdp/universe)
(require lang/posn)
(require racket/mpair)

(require "algo-used-1.rkt")
(require "useriscodebreaker.rkt")
(require "ALGO-NEW.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;general;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax lc
  (syntax-rules (: <- @)
    [(lc expr : var <- drawn-from) (map (lambda (var) expr) drawn-from)]
    [(lc expr : @ guard) (if guard (list expr) `())]
    [(lc expr : @ guard  qualifier ...) 
     (append* (lc (lc expr : qualifier ...) : @ guard))]
    [(lc expr : var <- drawn-from  qualifier ...) 
     (append* (lc (lc expr :  qualifier ... ) : var <- drawn-from))]))

(define-syntax for
  (syntax-rules (:)
    [(for init : condition : step : statements)
     (begin init (define (iter) (cond [condition (begin statements step (iter))])) (iter))]))

(define (inside? button-location x y)        
  (if (and (and (>= x (caar button-location)) (<= x (cdar button-location)))
           (and (>= y (cadr button-location)) (<= y (cddr button-location))))
      #t #f))

(define pair (cons 0 0))
(define guess-no 1)

;;DIMENSIONS:-
(define width 800)
(define height 800)
(define window (empty-scene width height "white"))
(define window-cover (overlay (scale/xy (/ width 320) (/ height 180) (bitmap "image-1.jpg")) window))
(define window-cover2 (overlay (scale/xy (/ width 3000) (/ height 2000) (bitmap "image-2.jpg")) window))
(define window-cover3 (overlay (scale/xy (/ width 850) (/ height 1100) (bitmap "image-4.png")) window))
(define window-cover4 (overlay (scale/xy (/ width 225) (/ height 225) (bitmap "image-5.jpg")) window))
(define box-width (quotient width 5))
(define box-height (quotient height 25))

(define button-center-x (quotient width 2))
(define button-center-y (quotient height 2))
(define button-center-x-gap (quotient width 5))
(define button-center-y-gap (quotient height 20))

;;Colour-Balls:-
(define radius 20)
(define colour-list
  (list "Red" "Yellow" "Green" "Medium Blue" "Brown" "Gray" "Orange" "LightCoral" "Dark Olive Green" "DeepPink" "Aqua" "Medium Violet Red"))
(define colour-ball-list
  (build-list 12 (lambda (x) (circle radius "solid" (list-ref colour-list x)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;start-window;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;highlight-state represents whether to highlight button or not
(define highlight-play 0)
(define highlight-quit 0)
(define highlight-instructions 0)
(define highlight-about 0)

(define (play-button)
  (if (equal? 0 highlight-play) (overlay (text "Play" 30 "white") (rectangle box-width box-height "solid" "red"))
      (overlay (text "Play" 30 "white") (rectangle (* 1.5 box-width) (* 2 box-height) "solid" "green"))))
(define (quit-button)
  (if (equal? 0 highlight-quit) (overlay (text "Quit" 30 "white") (rectangle box-width box-height "solid" "red"))
      (overlay (text "Quit" 30 "white") (rectangle (* 1.5 box-width) (* 2 box-height) "solid" "green"))))
(define (instructions-button)
  (if (equal? 0 highlight-instructions) (overlay (text "Instructions" 30 "white") (rectangle box-width box-height "solid" "red"))
      (overlay (text "Instructions" 30 "white") (rectangle (* 1.5 box-width) (* 2 box-height) "solid" "green"))))
(define (about-button)
  (if (equal? 0 highlight-about) (overlay (text "About us" 30 "white") (rectangle box-width box-height "solid" "red"))
      (overlay (text "About us" 30 "white") (rectangle (* 1.5 box-width) (* 2 box-height) "solid" "green"))))

(define play-button-location             ;;it returns a cons - first element is a cons of lower and upper limit of x and second element is a cons of lower and upper limit of y
  (cons (cons (- button-center-x (quotient (* 1.5 box-width) 2)) (+ button-center-x (quotient (* 1.5 box-width) 2)))
        (cons (- button-center-y (* 2 button-center-y-gap) (quotient (* 2 box-height) 2)) (+ (- button-center-y (* 2 button-center-y-gap)) (quotient (* 2 box-height) 2)))))
(define quit-button-location
  (cons (cons (- button-center-x (quotient (* 1.5 box-width) 2)) (+ button-center-x (quotient (* 1.5 box-width) 2)))
        (cons (- button-center-y (quotient (* 2 box-height) 2)) (+ button-center-y (quotient (* 2 box-height) 2)))))
(define instructions-button-location
  (cons (cons (- button-center-x (quotient (* 1.5 box-width) 2)) (+ button-center-x (quotient (* 1.5 box-width) 2)))
        (cons (- (+ button-center-y (* 2 button-center-y-gap)) (quotient (* 2 box-height) 2)) (+ button-center-y (* 2 button-center-y-gap) (quotient (* 2 box-height) 2)))))
(define about-button-location
  (cons (cons (- button-center-x (quotient (* 1.5 box-width) 2)) (+ button-center-x (quotient (* 1.5 box-width) 2)))
        (cons (- (+ button-center-y (* 4 button-center-y-gap)) (quotient (* 2 box-height) 2)) (+ button-center-y (* 4 button-center-y-gap) (quotient (* 2 box-height) 2)))))

(define (highlight-start-world w x y)
  (begin (set! highlight-play 0) (set! highlight-quit 0) (set! highlight-instructions 0) (set! highlight-about 0)
         (cond [(inside? play-button-location x y) (set! highlight-play 1)]
               [(inside? quit-button-location x y) (set! highlight-quit 1)]
               [(inside? instructions-button-location x y) (set! highlight-instructions 1)]
               [(inside? about-button-location x y) (set! highlight-about 1)])
           w))

(define (update-start-world w x y)
  (cond [(inside? play-button-location x y) (set! w "play-world1")]
        [(inside? quit-button-location x y) (set! w "quit-world")]
        [(inside? instructions-button-location x y) (set! w "instructions-world")]
        [(inside? about-button-location x y) (set! w "about-world")])
  w)

(define (start-window)
  (place-images (list (play-button) (quit-button) (instructions-button) (about-button))
                       (list (make-posn button-center-x (- button-center-y (* 2 button-center-y-gap)))
                             (make-posn button-center-x button-center-y)
                             (make-posn button-center-x (+ button-center-y (* 2 button-center-y-gap)))
                             (make-posn button-center-x (+ button-center-y (* 4 button-center-y-gap))))
                       ;window-cover)]
                       (overlay/align "middle" "top" (text/font "Mastermind" 80 "yellow" #f 'swiss "italic" "bold" #f) window-cover)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;play-window1;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define highlight-online 0)
(define highlight-offline-levels 0)
(define highlight-offline-vs 0)
(define highlight-back 0)


(define (online-button)
  (if (equal? 0 highlight-online) (overlay (text "Online" 30 "white") (rectangle box-width box-height "solid" "black"))
      (overlay (text "Online" 30 "white") (rectangle box-width box-height "solid" "blue"))))
(define (offline-levels-button)
  (if (equal? 0 highlight-offline-levels) (overlay (text "Levels" 30 "white") (rectangle box-width box-height "solid" "black"))
      (overlay (text "Levels" 30 "white") (rectangle box-width box-height "solid" "blue"))))
(define (offline-vs-button)
  (if (equal? 0 highlight-offline-vs) (overlay (text "VS Mode" 30 "white") (rectangle box-width box-height "solid" "black"))
      (overlay (text "VS Mode" 30 "white") (rectangle box-width box-height "solid" "blue"))))
(define (back-button)
  (if (equal? 0 highlight-back) (overlay (text "Back" 30 "white") (rectangle box-width box-height "solid" "black"))
      (overlay (text "Back" 30 "white") (rectangle box-width box-height "solid" "blue"))))

(define online-button-location
  (cons (cons (- button-center-x button-center-x-gap (* 1.5 box-width)) (- button-center-x button-center-x-gap (* 0.5 box-width)))
        (cons (- button-center-y (* 0.5 box-height)) (+ button-center-y (* 0.5 box-height)))))
(define offline-levels-button-location
  (cons (cons (- button-center-x (* 0.5 box-width)) (+ button-center-x (* 0.5 box-width)))
        (cons (- button-center-y (* 0.5 box-height)) (+ button-center-y (* 0.5 box-height)))))
(define offline-vs-button-location
  (cons (cons (+ button-center-x button-center-x-gap (* 0.5 box-width)) (+ button-center-x button-center-x-gap (* 1.5 box-width)))
        (cons (- button-center-y (* 0.5 box-height)) (+ button-center-y (* 0.5 box-height)))))
(define back-button-location
  (cons (cons (- (* 0.5 button-center-x-gap) (* 0.5 box-width)) (+ (* 0.5 button-center-x-gap) (* 0.5 box-width)))
        (cons (+ button-center-y-gap (* 0.5 box-height)) (+ button-center-y-gap (* 1.5 box-height)))))

(define (highlight-play-world1 w x y)
  (begin (set! highlight-online 0) (set! highlight-offline-levels 0) (set! highlight-offline-vs 0) (set! highlight-back 0)
         (cond [(inside? online-button-location x y) (set! highlight-online 1)]
               [(inside? offline-levels-button-location x y) (set! highlight-offline-levels 1)]
               [(inside? offline-vs-button-location x y) (set! highlight-offline-vs 1)]
               [(inside? back-button-location x y) (set! highlight-back 1)])
           w))

(define (update-play-world1 w x y)
  (cond [(inside? back-button-location x y) (set! w "start-world")]
        [(inside? offline-levels-button-location x y) (set! w "levels-world")]
        [(inside? offline-vs-button-location x y) (set! w "vs-world")]
        [(inside? online-button-location x y) (set! w "online-world")])
  w)


(define (play-window1)
  (place-images (list (online-button) (offline-levels-button) (offline-vs-button) (back-button))
                       (list (make-posn (- button-center-x button-center-x-gap box-width) button-center-y)
                             (make-posn button-center-x button-center-y)
                             (make-posn (+ button-center-x button-center-x-gap box-width) button-center-y)
                             (make-posn (* 0.5 button-center-x-gap) (+ button-center-y-gap box-height)))
                       window-cover2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;about-window;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (highlight-about-world w x y)
  (begin (set! highlight-back 0)
         (cond [(inside? back-button-location x y) (set! highlight-back 1)])
         w))

(define (update-about-world w x y)
  (cond [(inside? back-button-location x y) (set! w "start-world")])
  w)

(define (about-window)
  (place-images (list (back-button))
                (list (make-posn (* 0.5 button-center-x-gap) (+ button-center-y-gap box-height)))
                (overlay (scale/xy (/ width 710) (/ height 599) (bitmap "about-us image.png")) window-cover4)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;instructions-window;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define highlight-next 0)
(define highlight-prev 0)

(define (next-button)
  (if (equal? 0 highlight-next) (overlay (text "Next" 30 "white") (rectangle box-width box-height "solid" "black"))
      (overlay (text "Next" 30 "white") (rectangle box-width box-height "solid" "blue"))))
(define (prev-button)
  (if (equal? 0 highlight-prev) (overlay (text "Prev" 30 "white") (rectangle box-width box-height "solid" "black"))
      (overlay (text "Prev" 30 "white") (rectangle box-width box-height "solid" "blue"))))

(define prev-button-location
  (cons (cons (- (- button-center-x (* 2 box-width)) (* 0.5 box-width)) (+ (- button-center-x (* 2 box-width)) (/ box-width 2)))
        (cons (- (- height (/ box-height 2)) (/ box-height 2)) (+ (- height (/ box-height 2)) (/ box-height 2)))))
(define next-button-location
  (cons (cons (- (+ (* 2 box-width) button-center-x) (* 0.5 box-width)) (+ (+ (* 2 box-width) button-center-x) (/ box-width 2)))
        (cons (- (- height (/ box-height 2)) (/ box-height 2)) (+ (- height (/ box-height 2)) (/ box-height 2)))))

(define (highlight-instructions-world w x y)
  (begin (set! highlight-next 0) (set! highlight-prev 0) (set! highlight-back 0)
         (cond [(inside? next-button-location x y) (set! highlight-next 1)]
               [(inside? prev-button-location x y) (set! highlight-prev 1)]
               [(inside? back-button-location x y) (set! highlight-back 1)])
           w))

(define indx 0)
(define (update-instructions-world w x y)
  (cond [(inside? back-button-location x y) (set! w "start-world")]
        [(and (> indx 0) (inside? prev-button-location x y)) (set! indx (- indx 1))]
        [(and (< indx 2) (inside? next-button-location x y)) (set! indx (+ indx 1))])
  w)

(define list-of-instructions
  (list (scale/xy (/ width 877) (/ height 936) (bitmap "instructions-1.png"))
        (scale/xy (/ width 878) (/ height 938) (bitmap "instructions-2.png"))
        (scale/xy (/ width 905) (/ height 954) (bitmap "instructions-3.png"))))

(define (instructions-window)
  (place-images (list (prev-button) (next-button) (back-button))
                (list (make-posn (- button-center-x (* 2 box-width) ) (- height (/ box-height 2)))
                      (make-posn (+ button-center-x (* 2 box-width)) (- height (/ box-height 2)))
                      (make-posn (* 0.5 button-center-x-gap) (+ button-center-y-gap box-height)))
                (overlay (list-ref list-of-instructions indx) window)))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;online-window;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define highlight-start 0)
(define highlight-join 0)

(define (start-button)
  (if (equal? 0 highlight-start) (overlay (text "Start" 30 "white") (rectangle box-width box-height "solid" "black"))
      (overlay (text "Start" 30 "white") (rectangle box-width box-height "solid" "blue"))))
(define (join-button)
  (if (equal? 0 highlight-join) (overlay (text "Join" 30 "white") (rectangle box-width box-height "solid" "black"))
      (overlay (text "Join" 30 "white") (rectangle box-width box-height "solid" "blue"))))

(define start-button-location
  (cons (cons (- (/ width 3) (* 0.5 box-width)) (+ (/ width 3) (* 0.5 box-width)))
        (cons (- button-center-y (* 0.5 box-height)) (+ button-center-y (* 0.5 box-height)))))
(define join-button-location
  (cons (cons (- (* 2 (/ width 3)) (* 0.5 box-width)) (+ (* 2 (/ width 3)) (* 0.5 box-width)))
        (cons (- button-center-y (* 0.5 box-height)) (+ button-center-y (* 0.5 box-height)))))


(define (highlight-online-world w x y)
  (begin (set! highlight-start 0) (set! highlight-join 0) (set! highlight-back 0)
         (cond [(inside? start-button-location x y) (set! highlight-start 1)]
               [(inside? join-button-location x y) (set! highlight-join 1)]
               [(inside? back-button-location x y) (set! highlight-back 1)])
         w))

(define (update-online-world w x y)
  (cond [(inside? back-button-location x y) (set! w "play-world1")])
  w)

(define (online-window)
  (place-images (list (start-button) (join-button) (back-button))
                (list (make-posn (/ width 3) button-center-y)
                      (make-posn (* 2 (/ width 3)) button-center-y)
                      (make-posn (* 0.5 button-center-x-gap) (+ button-center-y-gap box-height)))
                       window-cover2))
                      

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;levels-window;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define highlight-easy 0)
(define highlight-medium 0)
(define highlight-hard 0)
(define highlight-insane 0)
;;Back-button is also there

(define (easy-button)
  (if (equal? 0 highlight-easy) (overlay (text "Easy" 30 "white") (rectangle box-width box-height "solid" "black"))
      (overlay (text "Easy" 30 "white") (rectangle box-width box-height "solid" "blue"))))
(define (medium-button)
  (if (equal? 0 highlight-medium) (overlay (text "Medium" 30 "white") (rectangle box-width box-height "solid" "black"))
      (overlay (text "Medium" 30 "white") (rectangle box-width box-height "solid" "blue"))))
(define (hard-button)
  (if (equal? 0 highlight-hard) (overlay (text "Hard" 30 "white") (rectangle box-width box-height "solid" "black"))
      (overlay (text "Hard" 30 "white") (rectangle box-width box-height "solid" "blue"))))
(define (insane-button)
  (if (equal? 0 highlight-insane) (overlay (text "Insane" 30 "white") (rectangle box-width box-height "solid" "black"))
      (overlay (text "Insane" 30 "white") (rectangle box-width box-height "solid" "blue"))))

(define h/5 (quotient height 5))                                  ;; used at many places 

(define easy-button-location
  (cons (cons (- button-center-x (quotient box-width 2)) (+ button-center-x (quotient box-width 2)))
        (cons (- h/5 (* 0.5 box-height)) (+ h/5 (* 0.5 box-height)))))
(define medium-button-location
 
  (cons (cons (- button-center-x (quotient box-width 2)) (+ button-center-x (quotient box-width 2)))
        (cons (- (* 2 h/5) (* 0.5 box-height)) (+ (* 2 h/5) (* 0.5 box-height)))))
(define hard-button-location
  (cons (cons (- button-center-x (quotient box-width 2)) (+ button-center-x (quotient box-width 2)))
        (cons (- (* 3 h/5) (* 0.5 box-height)) (+ (* 3 h/5) (* 0.5 box-height)))))
(define insane-button-location
  (cons (cons (- button-center-x (quotient box-width 2)) (+ button-center-x (quotient box-width 2)))
        (cons (- (* 4 h/5) (* 0.5 box-height)) (+ (* 4 h/5) (* 0.5 box-height)))))

(define (highlight-levels-world w x y)
  (begin (set! highlight-easy 0) (set! highlight-medium 0) (set! highlight-hard 0) (set! highlight-insane 0) (set! highlight-back 0)
         (cond [(inside? easy-button-location x y) (set! highlight-easy 1)]
               [(inside? medium-button-location x y) (set! highlight-medium 1)]
               [(inside? hard-button-location x y) (set! highlight-hard 1)]
               [(inside? insane-button-location x y) (set! highlight-insane 1)]
               [(inside? back-button-location x y) (set! highlight-back 1)])
           w))

(define (update-levels-world w x y)
  (cond [(inside? back-button-location x y) (set! w "play-world1")]
        [(inside? easy-button-location x y) (set! w "easy-world")]
        [(inside? medium-button-location x y) (set! w "medium-world")]
        [(inside? hard-button-location x y) (set! w "hard-world")]
        [(inside? insane-button-location x y) (set! w "insane-world")])
  w)

(define (levels-window)
  (place-images (list (easy-button) (medium-button) (hard-button) (insane-button) (back-button))
                       (list (make-posn button-center-x h/5)
                             (make-posn button-center-x (* 2 h/5))
                             (make-posn button-center-x (* 3 h/5))
                             (make-posn button-center-x (* 4 h/5))
                             (make-posn (* 0.5 button-center-x-gap) (+ button-center-y-gap box-height)))
                       window-cover2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;EMH;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define highlight-vec (make-vector 9 0))

(define (l-button index)
  (if (equal? 0 (vector-ref highlight-vec index))
      (overlay (text (number->string (+ 1 index)) 30 "white") (rectangle box-width box-height "solid" "black"))
      (overlay (text (number->string (+ 1 index)) 30 "white") (rectangle box-width box-height "solid" "blue"))))

(define h/4 (quotient height 4))
(define w/4 (quotient width 4))

(define (l-button-location index)
  (let ([i/3 (+ 1 (quotient index 3))]
        [r/3 (+ 1 (remainder index 3))])
    (cons (cons (- (* r/3 w/4) (* 0.5 box-width)) (+ (* r/3 w/4) (* 0.5 box-width)))
          (cons (- (* i/3 h/4) (* 0.5 box-height)) (+ (* i/3 h/4) (* 0.5 box-height))))))

(define (highlight-emh-world w x y)
  (begin (vector-fill! highlight-vec 0) (set! highlight-back 0)
         (if (inside? back-button-location x y) (begin (set! highlight-back 1) w)
         (highlight-emh-world-helper w x y 0))))

(define (highlight-emh-world-helper w x y index)
  (if (> index 8) w
      (if (inside? (l-button-location index) x y) (begin (vector-set! highlight-vec index 1) w) (highlight-emh-world-helper w x y (+ 1 index)))))

(define (emh-window)
  (emh-window-helper 0 (place-images (list (back-button)) (list (make-posn (* 0.5 button-center-x-gap) (+ button-center-y-gap box-height))) window-cover2)))

(define (emh-window-helper index scene)
  (if (> index 8) scene
      (emh-window-helper (+ 1 index) (place-images (list (l-button index))
                                                    (list (make-posn (* (+ 1 (remainder index 3)) w/4) (* (+ 1 (quotient index 3)) h/4)))
                                                    scene))))

(define level-code (cons 0 0));;first element represents the level-type (easy-1, medium-2......) second-element represnts the level-number

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;EASY-LEVEL;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (update-easy-world w x y)
  (cond [(inside? back-button-location x y) (set! w "levels-world") w]
        [else (update-easy-world-helper w x y 0)])
  )

(define (update-easy-world-helper w x y index)
  (if (> index 8) w
      (if (inside? (l-button-location index) x y) (begin (set! level-code (cons 1 (+ 1 index)))
                                                         (set-from-where-to-guess-list!)
                                                         (set! w "levels-game-world") w)
          (update-easy-world-helper w x y (+ 1 index)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;MEDIUM-LEVEL;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (update-medium-world w x y)
  (cond [(inside? back-button-location x y) (set! w "levels-world") w]
        [else (update-medium-world-helper w x y 0)])
  )

(define (update-medium-world-helper w x y index)
  (if (> index 8) w
      (if (inside? (l-button-location index) x y) (begin (set! level-code (cons 2 (+ 1 index)))
                                                         (set-from-where-to-guess-list!)
                                                         (set! w "levels-game-world") w)
          (update-medium-world-helper w x y (+ 1 index)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;HARD-LEVEL;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (update-hard-world w x y)
  (cond [(inside? back-button-location x y) (set! w "levels-world") w]
        [else (update-hard-world-helper w x y 0)])
  )

(define (update-hard-world-helper w x y index)
  (if (> index 8) w 
      (if (inside? (l-button-location index) x y) (begin (set! level-code (cons 3 (+ 1 index)))
                                                         (set-from-where-to-guess-list!)
                                                         (set! w "levels-game-world") w)
          (update-hard-world-helper w x y (+ 1 index)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;INSANE-LEVEL;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;some functions derived from EMH
(define highlight-vec2 (make-vector 6 0))

(define w/3 (quotient width 3))

(define (l-button2 index)
  (if (equal? 0 (vector-ref highlight-vec2 index))
      (overlay (text (number->string (+ 1 index)) 30 "white") (rectangle box-width box-height "solid" "black"))
      (overlay (text (number->string (+ 1 index)) 30 "white") (rectangle box-width box-height "solid" "blue"))))

(define (l-button-location2 index)
  (let ([i/2 (+ 1 (quotient index 2))]
        [r/2 (+ 1 (remainder index 2))])
    (cons (cons (- (* r/2 w/3) (* 0.5 box-width)) (+ (* r/2 w/3) (* 0.5 box-width)))
          (cons (- (* i/2 h/4) (* 0.5 box-height)) (+ (* i/2 h/4) (* 0.5 box-height))))))

(define (highlight-insane-world w x y)
  (begin (vector-fill! highlight-vec2 0) (set! highlight-back 0)
         (if (inside? back-button-location x y) (begin (set! highlight-back 1) w)
         (highlight-insane-world-helper w x y 0))))

(define (highlight-insane-world-helper w x y index)
  (if (> index 5) w
      (if (inside? (l-button-location2 index) x y) (begin (vector-set! highlight-vec2 index 1) w) (highlight-insane-world-helper w x y (+ 1 index)))))

(define (insane-window)
  (insane-window-helper 0 (place-images (list (back-button)) (list (make-posn (* 0.5 button-center-x-gap) (+ button-center-y-gap box-height))) window-cover2)))

(define (insane-window-helper index scene)
  (if (> index 5) scene
      (insane-window-helper (+ 1 index) (place-images (list (l-button2 index))
                                                    (list (make-posn (* (+ 1 (remainder index 2)) w/3) (* (+ 1 (quotient index 2)) h/4)))
                                                    scene))))


(define (update-insane-world w x y)
  (cond [(inside? back-button-location x y) (set! w "levels-world") w]
        [else (update-insane-world-helper w x y 0)])
  )

(define (update-insane-world-helper w x y index)
  (if (> index 5) w
      (if (inside? (l-button-location2 index) x y) (begin (set! level-code (cons 4 (+ 1 index)))
                                                         (set-from-where-to-guess-list!)
                                                         (set! w "levels-game-world") w)
          (update-insane-world-helper w x y (+ 1 index)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;LEVELS-GAME-WINDOW;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;(define computer-code-boolean #t)
(define computer-code-final '())
(define empty-ball-boolean #f)


(define from-where-to-guess-list '())
(define from-where-to-guess-vector (list->vector from-where-to-guess-list))
(define (set-from-where-to-guess-list!)
  (begin (let ([a (car level-code)]
        [b (cdr level-code)])
    (cond [(equal? a 1)
           (cond [(<= b 4) (set! code-no 1)]
                 [(<= b 7) (set! code-no 2)]
                 [(<= b 9) (set! code-no 3)])
           (set-balls2! (cdr (choose-from)) (car (choose-from)))
           (set! computer-code-final (choose-noduplicates (car (choose-from)) (cdr (choose-from))))]
          [(equal? a 2)
           (cond [(<= b 3) (set! code-no 1)]
                 [(<= b 6) (set! code-no 2)]
                 [(<= b 9) (set! code-no 3)])
           (set-balls2! (cdr (choose-from)) (car (choose-from)))
           (set! computer-code-final (choose-duplicates (car (choose-from)) (cdr (choose-from))))]
          [(equal? a 3)
           (cond [(<= b 2) (set! code-no 1)]
                 [(<= b 4) (set! code-no 2)]
                 [(<= b 9) (set! code-no 3)])
           (set-balls2! (cdr (choose-from)) (car (choose-from)))
           (set! computer-code-final (choose-duplicates-empty (car (choose-from)) (cdr (choose-from))))]
          [(equal? a 4)
              (set! code-no 4)
              (set-balls2! (cdr (choose-from)) (car (choose-from)))
              (set! computer-code-final (choose-duplicates-empty (car (choose-from)) (cdr (choose-from))))])
         (cond [(>= a 3) (set! empty-ball-boolean #t)])
         (set! from-where-to-guess-list (take colour-ball-list (cdr (choose-from))))
         (set! from-where-to-guess-vector (list->vector from-where-to-guess-list)))))


(define (updated-bottom-rectangle2)
  (place-images (append (vector->list from-where-to-guess-vector) (list ;(circle (/ radius 3) 'solid 'black)
                                        ;(circle (/ radius 3) 'outline 'black)
                                        (overlay (text "Done" 15 "black") (rectangle (/ box-width 2) box-height "outline" "black"))))
                (append (build-list (length from-where-to-guess-list)
                                    (lambda (x) (make-posn (+ (/ width 25) (* (+ 1 x) (/ (* 0.8 width) (+ 1 (length from-where-to-guess-list)))))
                                                           (- height radius))))
                        (list ;(make-posn (* 0.83 width) (/ height 60))
                              ;(make-posn (+ (* 4 (/ radius 3)) (* 0.83 width)) (/ height 60))
                              (make-posn (- (* 0.83 width) (* 8 (/ radius 3))) (/ height 60))))
                board))

(define (levels-game-window)

  (set! x (/ (* width 0.56) (car (choose-from))))                    ;; x is the x-length of square
  (set! y (/ (* 7 height) (* 8 (no-of-trials))))  
  (set! sq-p (rectangle x y 'outline 'black))
  (set! sq-s (rectangle (/ x 2) (/ y 2) 'outline 'black))

  (set! base-p sq-p)
  (for (set! i 1) : (< i (car (choose-from))) : (set! i (+ i 1)) : (set! base-p (beside sq-p base-p)))

  (set! grid-p base-p)
  (for (set! i 1) : (< i (no-of-trials)) : (set! i (+ i 1)) : (set! grid-p (above base-p grid-p)))

  (set! base-s sq-s)
  (begin (for (set! i 1) : (< i (ceiling (/ (car (choose-from)) 2))) : (set! i (+ i 1)) : (set! base-s (beside sq-s base-s)))
       (set! base-s (above base-s base-s)))

  (set! grid-s base-s)
  (for (set! i 1) : (< i (no-of-trials)) : (set! i (+ i 1)) : (set! grid-s (above base-s grid-s)))
  

  (set! final-board (overlay/align "left" "middle" grid-p
                                  (overlay/align "right" "middle" grid-s
                                                 (overlay/align "middle" "bottom" (updated-bottom-rectangle2)
                                                               ; (overlay/xy board (- (/ width 80) (* 0.4 width)) (- height (quotient height 40)) bottom-rectangle)))))
                                                                (overlay/align "middle" "bottom" bottom-rectangle board)))))

  (set! final-board-2 (overlay final-board window-cover4))
  final-board-2)

(define (levels-game-window2)
  (place-images (append (vector->list from-where-to-guess-vector) (list ;(circle (/ radius 3) 'solid 'black)
                                        ;(circle (/ radius 3) 'outline 'black)
                                        (overlay (text "Done" 15 "black") (rectangle (/ box-width 2) box-height "outline" "black"))))
                (append (build-list (length from-where-to-guess-list)
                                    (lambda (x) (make-posn (+ (* 0.05 width) (/ width 25) (* (+ 1 x) (/ (* 0.8 width) (+ 1 (length from-where-to-guess-list)))))
                                                           (- height radius))))
                        (list ;(make-posn (* 0.83 width) (/ height 60))
                              ;(make-posn (+ (* 4 (/ radius 3)) (* 0.83 width)) (/ height 60))
                              (make-posn (+ (* 0.05 width) (- (* 0.83 width) (* 8 (/ radius 3)))) (/ height 60))))
                final-board-2))

(define (d2) (/ (* 0.8 width) (+ 1 (cdr (choose-from)))))

(define (colour-guess-button-location index)
  (cons (cons (- (+ (* width 0.04) (* (+ 1 index) (d2))) radius) (+ (+ (* width 0.04) (* (+ 1 index) (d2))) radius))
        (cons (- (- height (quotient height 40))  radius) (+ (- height (quotient height 40)) radius))))

;;done-button-location taken from later on

(define single-guess-list '())
(define single-guess-list-no '())
(define levels-guess-no 1)

(define (input2 l)
    (set! final-board-2 (place-images l
                  (build-list (length l)
                              (lambda (z) (make-posn (+ (* 0.05 width) (/ x 2) (* x z)) (/ (+ height (* y (+ 1 (- (no-of-trials) (* 2 levels-guess-no))))) 2))))
                  final-board-2)))
  
(define pair-output (cons 0 0))

(define (update-levels-game-world w x y)
  (cond [(or (inside? done-button-location x y) (equal? (length single-guess-list) (car (choose-from))))
                  (begin
                  (set! w "levels-game-world2")
                  (input2 single-guess-list)
                  (set! single-guess-list-no (convert-to-num-list single-guess-list))
                  (set! pair-output (pegs-pair2 single-guess-list-no computer-code-final))
                  (set! white-cnt (car pair-output))
                  (set! black-cnt (cdr pair-output))
                  (place-black-white levels-guess-no)
                  (set! single-guess-list '())
                  (set! levels-guess-no (+ 1 levels-guess-no))
                  (set! from-where-to-guess-vector (list->vector from-where-to-guess-list))
                  (if (or (> levels-guess-no (no-of-trials)) (equal? (car (choose-from)) black-cnt)) (begin (set! w "levels-game-finish") w)
                      w))]
  ;(cond [(inside? back-button-location x y) (set! w "set-world") w]
        [else (update-levels-game-world-helper w x y 0)]))

(define (update-levels-game-world-helper w x y index)
  (if (>= index (cdr (choose-from))) (begin (add-empty) (input2 single-guess-list) (set! empty-ball-boolean #f) (set! w "levels-game-world2") w)
      (if (inside? (colour-guess-button-location index) (- x (* 0.05 width)) y) (begin (add-code2 index w) (input2 single-guess-list) (set! w "levels-game-world2") w) (update-levels-game-world-helper w x y (+ 1 index)))))

(define (add-code2 index w)
  (let* ([what-to-add (list-ref from-where-to-guess-list index)]
         [how-to-add (overlay (circle radius "outline" "black") what-to-add)]) 
    (begin (vector-set! from-where-to-guess-vector index how-to-add)
           ;(cond [(not (member what-to-add code-list))
                  (set! single-guess-list (append single-guess-list (list what-to-add))))
           ))

(define (add-empty)
  (cond [empty-ball-boolean (set! single-guess-list (append single-guess-list
                                                           (list (rectangle (/ (* width 0.56) (car (choose-from))) (/ (* 7 height) (* 8 (no-of-trials))) 'outline 'black))))]))

(define (convert-to-num-list l)
  (map (lambda (x) (if (index-of colour-ball-list x) (+ 1 (index-of colour-ball-list x)) 0)) l))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;levels-game-finish;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define highlight-continue 0)

(define (continue-button)
  (if (equal? highlight-continue 0) (overlay (text "Continue" 30 "white") (rectangle box-width box-height "solid" "black"))
      (overlay (text "Continue" 30 "white") (rectangle box-width box-height "solid" "blue"))))

(define continue-button-location
  (cons (cons (- button-center-x (/ box-width 2)) (+ button-center-x (/ box-width 2)))
        (cons (- (+ button-center-y (* 2 box-height)) (/ box-height 2)) (+ (+ button-center-y (* 2 box-height)) (/ box-height 2)))))

(define (highlight-levels-game-finish-world w x y)
  (begin (set! highlight-continue 0)
         (cond [(inside? continue-button-location x y) (set! highlight-continue 1)])
               w))

(define (update-levels-game-finish-world w x y)
  (cond [(inside? continue-button-location x y) (set! w "levels-world")])
        w)

(define (congratulations-board)
  (place-images (list (continue-button)
                (text "Congratulations" 50 "black")
                (text (string-append "You Guessed the Right Code in " (number->string (- levels-guess-no 1)) " Moves") 30 "black")
                (rectangle (* 5 box-width) (* 5 box-height) "solid" "LightPink"))

                (list (make-posn button-center-x (+ button-center-y (* 2 box-height)))
                      (make-posn button-center-x (- button-center-y (* 1.8 box-height)))
                      (make-posn button-center-x (- button-center-y (* 0.6 box-height)))
                      (make-posn button-center-x button-center-y))
                window-cover2))

(define (sorry-board)
  (place-images (list (continue-button)
                (text "Sorry" 50 "black")
                (text (string-append "You could Not Guess the Right Code in " (number->string (- levels-guess-no 1)) " Moves") 30 "black")
                (rectangle (* 5 box-width) (* 5 box-height) "solid" "LightPink"))

                (list (make-posn button-center-x (+ button-center-y (* 2 box-height)))
                      (make-posn button-center-x (- button-center-y (* 1.8 box-height)))
                      (make-posn button-center-x (- button-center-y (* 0.6 box-height)))
                      (make-posn button-center-x button-center-y))
                window-cover2))
                

(define (levels-game-finish-window)
  (if (> levels-guess-no (no-of-trials)) (sorry-board)
      (congratulations-board)))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;VS-WORLD;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define highlight-set-2 0)
(define highlight-set-4 0)
(define highlight-set-6 0)
(define highlight-set-8 0)
;;Back-button is also there

(define set-no 0)

(define (set-2-button)
  (if (equal? 0 highlight-set-2) (overlay (text "Set of Two" 30 "white") (rectangle box-width box-height "solid" "black"))
      (overlay (text "Set of Two" 30 "white") (rectangle box-width box-height "solid" "blue"))))
(define (set-4-button)
  (if (equal? 0 highlight-set-4) (overlay (text "Set of Four" 30 "white") (rectangle box-width box-height "solid" "black"))
      (overlay (text "Set of Four" 30 "white") (rectangle box-width box-height "solid" "blue"))))
(define (set-6-button)
  (if (equal? 0 highlight-set-6) (overlay (text "Set of Six" 30 "white") (rectangle box-width box-height "solid" "black"))
      (overlay (text "Set of Six" 30 "white") (rectangle box-width box-height "solid" "blue"))))
(define (set-8-button)
  (if (equal? 0 highlight-set-8) (overlay (text "Set of Eight" 30 "white") (rectangle box-width box-height "solid" "black"))
      (overlay (text "Set of Eight" 30 "white") (rectangle box-width box-height "solid" "blue"))))
                                
(define set-2-button-location
  (cons (cons (- button-center-x (quotient box-width 2)) (+ button-center-x (quotient box-width 2)))
        (cons (- h/5 (* 0.5 box-height)) (+ h/5 (* 0.5 box-height)))))
(define set-4-button-location
  (cons (cons (- button-center-x (quotient box-width 2)) (+ button-center-x (quotient box-width 2)))
        (cons (- (* 2 h/5) (* 0.5 box-height)) (+ (* 2 h/5) (* 0.5 box-height)))))
(define set-6-button-location
  (cons (cons (- button-center-x (quotient box-width 2)) (+ button-center-x (quotient box-width 2)))
        (cons (- (* 3 h/5) (* 0.5 box-height)) (+ (* 3 h/5) (* 0.5 box-height)))))
(define set-8-button-location
  (cons (cons (- button-center-x (quotient box-width 2)) (+ button-center-x (quotient box-width 2)))
        (cons (- (* 4 h/5) (* 0.5 box-height)) (+ (* 4 h/5) (* 0.5 box-height)))))

(define (highlight-vs-world w x y)
  (begin (set! highlight-set-2 0) (set! highlight-set-4 0) (set! highlight-set-6 0) (set! highlight-set-8 0) (set! highlight-back 0)
         (cond [(inside? set-2-button-location x y) (set! highlight-set-2 1)]
               [(inside? set-4-button-location x y) (set! highlight-set-4 1)]
               [(inside? set-6-button-location x y) (set! highlight-set-6 1)]
               [(inside? set-8-button-location x y) (set! highlight-set-8 1)]
               [(inside? back-button-location x y) (set! highlight-back 1)])
           w))

(define (update-vs-world w x y)
  (cond [(inside? back-button-location x y) (set! w "play-world1")]
        [(inside? set-2-button-location x y) (set! set-no 2) (set! w "set-world")]
        [(inside? set-4-button-location x y) (set! set-no 4) (set! w "set-world")]
        [(inside? set-6-button-location x y) (set! set-no 6) (set! w "set-world")]
        [(inside? set-8-button-location x y) (set! set-no 8) (set! w "set-world")])
  w)

(define (vs-window)
  (place-images (list (set-2-button) (set-4-button) (set-6-button) (set-8-button) (back-button))
                       (list (make-posn button-center-x h/5)
                             (make-posn button-center-x (* 2 h/5))
                             (make-posn button-center-x (* 3 h/5))
                             (make-posn button-center-x (* 4 h/5))
                             (make-posn (* 0.5 button-center-x-gap) (+ button-center-y-gap box-height)))
                       window-cover2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;SET-WORLD;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;much info is taken from levels world
(define (update-set-world w x y)
  (cond [(inside? back-button-location x y) (set! w "vs-world")]
        [(inside? easy-button-location x y) (set! code-no 1) (set! w "choose-code-world")]
        [(inside? medium-button-location x y) (set! code-no 2) (set! w "choose-code-world")]
        [(inside? hard-button-location x y) (set! code-no 3) (set! w "choose-code-world")]
        [(inside? insane-button-location x y) (set! code-no 4) (set! w "choose-code-world")])
  w)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;CHOOSE-CODE-WORLD;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define code-no 0)          ;easy(4-6)--1, medium(5-8)--2, hard(6-9)--3, insane(8-12)--4
(define (choose-from)
  (cond [(equal? code-no 1) (cons 4 6)]
        [(equal? code-no 2) (cons 5 8)]
        [(equal? code-no 3) (cons 6 9)]
        [else (cons 8 12)]))

(define (no-of-trials)
  (cond [(equal? code-no 0) 1]
        [(equal? code-no 1) 10]
        [(equal? code-no 2) 12]
        [(equal? code-no 3) 14]
        [(equal? code-no 4) 16]))

(define code-list '())

(define colour-ball-vector (list->vector colour-ball-list))
(define (d) (/ width (+ 1 (cdr (choose-from)))))

(define (colour-button-location index)
  (cons (cons (- (* (+ 1 index) (d)) radius) (+ (* (+ 1 index) (d)) radius))
        (cons (- button-center-y radius) (+ button-center-y radius))))

(define (highlight-choose-code-world w x y)
  (begin (set! highlight-back 0)
         (cond [(inside? back-button-location x y) (set! highlight-back 1)])
           w))

(define (update-choose-code-world w x y)
  (cond [(equal? (length code-list) (car (choose-from)))
                  (if (equal? (car (choose-from)) 4) (begin (set! w "game-world") (set! colour-ball-vector (list->vector colour-ball-list)) w)
                      (begin (set! w "genetic-game-world") (set! colour-ball-vector (list->vector colour-ball-list)) w))])
  (cond [(inside? back-button-location x y) (set! w "set-world") w]
        [else (update-choose-code-world-helper w x y 0)]))

(define (update-choose-code-world-helper w x y index)
  (if (>= index (cdr (choose-from))) w
      (if (inside? (colour-button-location index) x y) (begin (add-code index w) w) (update-choose-code-world-helper w x y (+ 1 index)))))

(define (add-code index w)
  (let* ([what-to-add (list-ref colour-ball-list index)]
         [how-to-add (overlay (circle radius "outline" "black") what-to-add)]) 
    (begin (vector-set! colour-ball-vector index how-to-add)
           ;(cond [(not (member what-to-add code-list))
                  (set! code-list (append code-list (list what-to-add))))
           ))
                  ;(game-board (no-of-trials) (car (choose-from)))]))))
            

(define (choose-code-window)
  (place-images (append (take (vector->list colour-ball-vector) (cdr (choose-from))) (list (back-button)))
                (append (build-list (cdr (choose-from)) (lambda (x) (make-posn (* (+ 1 x) (d)) button-center-y)))
                        (list (make-posn (* 0.5 button-center-x-gap) (+ button-center-y-gap box-height))))
                (overlay/align "middle" "top" (text/font "Choose Your Code" 50 "black" #f 'decorative "normal" "bold" #f) window-cover2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;GAME-BOARD;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 
                          ;; m is the number of rows and n is the number of columns
  
  (define x (/ (* width 0.56) (car (choose-from))))                    ;; x is the x-length of square
  (define y (/ (* 7 height) (* 8 (no-of-trials))))                ;; y is the y-length of square
  (define i 1)

  (define sq-p (rectangle x y 'outline 'black))
  (define sq-s (rectangle (/ x 2) (/ y 2) 'outline 'black))

  (define base-p sq-p)
  (for (set! i 1) : (< i (car (choose-from))) : (set! i (+ i 1)) : (set! base-p (beside sq-p base-p)))

  (define grid-p base-p)
  (for (set! i 1) : (< i (no-of-trials)) : (set! i (+ i 1)) : (set! grid-p (above base-p grid-p)))

  (define base-s sq-s)
  (begin (for (set! i 1) : (< i (ceiling (/ (car (choose-from)) 2))) : (set! i (+ i 1)) : (set! base-s (beside sq-s base-s)))
       (set! base-s (above base-s base-s)))

  (define grid-s base-s)
  (for (set! i 1) : (< i (no-of-trials)) : (set! i (+ i 1)) : (set! grid-s (above base-s grid-s)))

  (define bottom-rectangle (rectangle (* 0.8 width) (quotient height 20) 'outline 'grey))
(define (updated-bottom-rectangle)
  (place-images (append code-list (list (circle (/ radius 3) 'solid 'black)
                                        (circle (/ radius 3) 'outline 'black)
                                        (overlay (text "Done" 15 "black") (rectangle (/ box-width 2) box-height "outline" "black"))))
                (append (build-list (length code-list) (lambda (x) (make-posn (* (+ 1 x) (/ (* 0.8 width) (+ 1 (length code-list)))) (- height radius))))
                        (list (make-posn (* 0.83 width) (/ height 60))
                              (make-posn (+ (* 4 (/ radius 3)) (* 0.83 width)) (/ height 60))
                              (make-posn (- (* 0.83 width) (* 8 (/ radius 3))) (/ height 60))))
                board))

  (define board (rectangle (* 0.9 width) height 'outline 'black))
  (define final-board (overlay/align "left" "middle" grid-p
                                  (overlay/align "right" "middle" grid-s
                                                 (overlay/align "middle" "bottom" (updated-bottom-rectangle)
                                                               ; (overlay/xy board (- (/ width 80) (* 0.4 width)) (- height (quotient height 40)) bottom-rectangle)))))
                                                                (overlay/align "middle" "bottom" bottom-rectangle board)))))
;                                                                               (overlay/xy board (* 0.865 width) (- height (quotient height 40))
;                                                                                           (rectangle (* 0.07 width) (quotient height 20) 'outline 'black))))))) 


  (define final-board-2 (overlay final-board window-cover4))

  ; final-board-2)
;(cond [(or (equal? code-no 1) (equal? code-no 2)) (5-guess-algo  code-no (car (choose-from)) (cdr (choose-from)) (no-of-trials) code-list final-board-2)])

(define (game-window)
  (set! x (/ (* width 0.56) (car (choose-from))))                    ;; x is the x-length of square
  (set! y (/ (* 7 height) (* 8 (no-of-trials))))  
  (set! sq-p (rectangle x y 'outline 'black))
  (set! sq-s (rectangle (/ x 2) (/ y 2) 'outline 'black))

  (set! base-p sq-p)
  (for (set! i 1) : (< i (car (choose-from))) : (set! i (+ i 1)) : (set! base-p (beside sq-p base-p)))

  (set! grid-p base-p)
  (for (set! i 1) : (< i (no-of-trials)) : (set! i (+ i 1)) : (set! grid-p (above base-p grid-p)))

  (set! base-s sq-s)
  (begin (for (set! i 1) : (< i (ceiling (/ (car (choose-from)) 2))) : (set! i (+ i 1)) : (set! base-s (beside sq-s base-s)))
       (set! base-s (above base-s base-s)))

  (set! grid-s base-s)
  (for (set! i 1) : (< i (no-of-trials)) : (set! i (+ i 1)) : (set! grid-s (above base-s grid-s)))
  

  (set! final-board (overlay/align "left" "middle" grid-p
                                  (overlay/align "right" "middle" grid-s
                                                 (overlay/align "middle" "bottom" (updated-bottom-rectangle)
                                                               ; (overlay/xy board (- (/ width 80) (* 0.4 width)) (- height (quotient height 40)) bottom-rectangle)))))
                                                                (overlay/align "middle" "bottom" bottom-rectangle board)))))
  (set! final-board-2 (overlay final-board window-cover4))
  ;(what-to-guess)); return final board with first guess and write the condition for algorithm
  (if (equal? (car (choose-from)) 4) (begin (set! guess-no 1) (input (form-input-list (car (choose-from)) 1)))
      (begin (set! guess-no 1) (input (form-input-list2 (car (choose-from)) 1)))))

(define sc empty-image)
(define sc2 empty-image)

(define (game-window2)
  (displayln "abc")
  (begin (set-balls! (cdr (choose-from)) (car (choose-from)) pair)
         (set! guess-no (+ 1 guess-no))
  (input (what-to-guess))))

(define (game-window-bw)
  final-board-2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;levels-mode-copy;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (set-from-where-to-guess-list2!)
    (cond [(equal? (car (choose-from)) 4)
           (set-balls2! (cdr (choose-from)) (car (choose-from)))
           (set! computer-code-final (choose-noduplicates (car (choose-from)) (cdr (choose-from))))]
          [(equal? (car (choose-from)) 5)
           (set-balls2! (cdr (choose-from)) (car (choose-from)))
           (set! computer-code-final (choose-duplicates (car (choose-from)) (cdr (choose-from))))]
          [(equal? (car (choose-from)) 6)
           (set-balls2! (cdr (choose-from)) (car (choose-from)))
           (set! computer-code-final (choose-duplicates-empty (car (choose-from)) (cdr (choose-from))))]
          [(equal? (car (choose-from)) 8)
              (set-balls2! (cdr (choose-from)) (car (choose-from)))
              (set! computer-code-final (choose-duplicates-empty (car (choose-from)) (cdr (choose-from))))])
         (cond [(>= (car (choose-from)) 6) (set! empty-ball-boolean #t)])
         (set! from-where-to-guess-list (take colour-ball-list (cdr (choose-from))))
         (set! from-where-to-guess-vector (list->vector from-where-to-guess-list)))

(define (update-levels-mode-copy w x y)
  (cond [(or (inside? done-button-location x y) (equal? (length single-guess-list) (car (choose-from))))
                  (begin
                  (set! w "levels-mode-copy2")
                  (input2 single-guess-list)
                  (set! single-guess-list-no (convert-to-num-list single-guess-list))
                  (set! pair-output (pegs-pair2 single-guess-list-no computer-code-final))
                  (set! white-cnt (car pair-output))
                  (set! black-cnt (cdr pair-output))
                  (place-black-white levels-guess-no)
                  (set! single-guess-list '())
                  (set! levels-guess-no (+ 1 levels-guess-no))
                  (set! from-where-to-guess-vector (list->vector from-where-to-guess-list))
                  (if (or (> levels-guess-no (no-of-trials)) (equal? (car (choose-from)) black-cnt)) (begin (set! w "vs-game-finish")
                                                                                                            (set! user-score (+ user-score levels-guess-no))
                                                                                                            (set! set-no (- set-no 1))
                                                                                                                  w)
                      w))]
        [else (update-levels-mode-copy-helper w x y 0)]))

(define (update-levels-mode-copy-helper w x y index)
  (if (>= index (cdr (choose-from))) (begin (add-empty) (input2 single-guess-list) (set! empty-ball-boolean #f) (set! w "levels-mode-copy2") w)
      (if (inside? (colour-guess-button-location index) (- x (* 0.05 width)) y) (begin (add-code2 index w) (input2 single-guess-list) (set! w "levels-mode-copy2") w) (update-levels-mode-copy-helper w x y (+ 1 index)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;vs-game-finish;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define user-score 0)
(define computer-score 0)

;;continue button details same as levels-game-finish
(define (update-vs-game-finish-world w x y)
  (cond [(inside? continue-button-location x y)
         (cond [(equal? 0 set-no) (set! w "win/lost-world")]
               [(even? set-no) (set! w "choose-code-world")]
               [else (set-from-where-to-guess-list2!) (set! w "levels-mode-copy")])]
        )w)


(define (congratulations-board2)
  (place-images (if (even? set-no)
                    (list (continue-button)
                (text "Congratulations" 50 "black")
                (text (string-append "You Guessed the Right Code in " (number->string levels-guess-no) " Moves") 30 "black")
                (text (string-append "Your Score: " (number->string user-score)) 30 "black")
                (text (string-append "Computer Score: " (number->string computer-score)) 30 "black")
                (rectangle (* 5 box-width) (* 5 box-height) "solid" "LightPink"))

                    (list (continue-button)
                (text "Its Your Turn Now" 30 "black")
                (text (string-append "Computer Guessed the Right Code in " (number->string guess-no) " Moves") 30 "black")
                (text (string-append "Your Score: " (number->string user-score)) 30 "black")
                (text (string-append "Computer Score: " (number->string computer-score)) 30 "black")
                (rectangle (* 5 box-width) (* 5 box-height) "solid" "LightPink")))


                (list (make-posn button-center-x (+ button-center-y (* 2 box-height)))
                      (make-posn button-center-x (- button-center-y (* 1.8 box-height)))
                      (make-posn button-center-x (- button-center-y (* 0.6 box-height)))
                      (make-posn (- button-center-x (* 1.5 box-width)) (+ button-center-y (* 0.6 box-height)))
                      (make-posn (+ button-center-x (* 1.5 box-width)) (+ button-center-y (* 0.6 box-height)))
                      (make-posn button-center-x button-center-y))
                window-cover2))

(define (sorry-board2)
  (place-images (if (even? set-no)
                    (list (continue-button)
                (text "Sorry" 50 "black")
                (text (string-append "You could Not Guess the Right Code in " (number->string (- levels-guess-no 1)) " Moves") 30 "black")
                (text (string-append "Your Score: " (number->string user-score)) 30 "black")
                (text (string-append "Computer Score: " (number->string computer-score)) 30 "black")
                (rectangle (* 5 box-width) (* 5 box-height) "solid" "LightPink"))

                    (list (continue-button)
                (text "Its Your Turn Now" 30 "black")
                (text (string-append "Computer could Not Guess the Right Code in " (number->string (- guess-no 1)) " Moves") 30 "black")
                (text (string-append "Your Score: " (number->string user-score)) 30 "black")
                (text (string-append "Computer Score: " (number->string computer-score)) 30 "black")
                (rectangle (* 5 box-width) (* 5 box-height) "solid" "LightPink")))

                (list (make-posn button-center-x (+ button-center-y (* 2 box-height)))
                      (make-posn button-center-x (- button-center-y (* 1.8 box-height)))
                      (make-posn button-center-x (- button-center-y (* 0.6 box-height)))
                      (make-posn (- button-center-x (* 1.5 box-width)) (+ button-center-y (* 0.6 box-height)))
                      (make-posn (+ button-center-x (* 1.5 box-width)) (+ button-center-y (* 0.6 box-height)))
                      (make-posn button-center-x button-center-y))
                window-cover2))
                

(define (vs-game-finish-window)
  (if (> levels-guess-no (no-of-trials)) (sorry-board2)
      (congratulations-board2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;win/lost-window;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (update-won/lost-world w x y)
  (cond [(inside? continue-button-location x y) (set! w "start-world") w]
        ))

(define (won-board)
  (place-images (list (continue-button)
                      (text "You Won !!" 50 "black")
                      (text (string-append "Your Score: " (number->string user-score)) 30 "black")
                      (text (string-append "Computer Score: " (number->string computer-score)) 30 "black")
                      (rectangle (* 5 box-width) (* 5 box-height) "solid" "LightPink"))
                (list (make-posn button-center-x (+ button-center-y (* 2 box-height)))
                      (make-posn button-center-x (- button-center-y (* 1.8 box-height)))
                      (make-posn (- button-center-x (* 1.5 box-width)) (+ button-center-y (* 0.6 box-height)))
                      (make-posn (+ button-center-x (* 1.5 box-width)) (+ button-center-y (* 0.6 box-height)))
                      (make-posn button-center-x button-center-y))
                window-cover2))


(define (lost-board)
  (place-images (list (continue-button)
                      (text "You Lost !!" 50 "black")
                      (text (string-append "Your Score: " (number->string user-score)) 30 "black")
                      (text (string-append "Computer Score: " (number->string computer-score)) 30 "black")
                      (rectangle (* 5 box-width) (* 5 box-height) "solid" "LightPink"))
                (list (make-posn button-center-x (+ button-center-y (* 2 box-height)))
                      (make-posn button-center-x (- button-center-y (* 1.8 box-height)))
                      (make-posn (- button-center-x (* 1.5 box-width)) (+ button-center-y (* 0.6 box-height)))
                      (make-posn (+ button-center-x (* 1.5 box-width)) (+ button-center-y (* 0.6 box-height)))
                      (make-posn button-center-x button-center-y))
                window-cover2))

(define (draw-board)
  (place-images (list (continue-button)
                      (text "Match Drawn !!" 50 "black")
                      (text (string-append "Your Score: " (number->string user-score)) 30 "black")
                      (text (string-append "Computer Score: " (number->string computer-score)) 30 "black")
                      (rectangle (* 5 box-width) (* 5 box-height) "solid" "LightPink"))
                (list (make-posn button-center-x (+ button-center-y (* 2 box-height)))
                      (make-posn button-center-x (- button-center-y (* 1.8 box-height)))
                      (make-posn (- button-center-x (* 1.5 box-width)) (+ button-center-y (* 0.6 box-height)))
                      (make-posn (+ button-center-x (* 1.5 box-width)) (+ button-center-y (* 0.6 box-height)))
                      (make-posn button-center-x button-center-y))
                window-cover2))

(define (won/lost-window)
  (cond [(< user-score computer-score) (won-board)]
        [(> user-score computer-score) (lost-board)]
        [else (draw-board)]))
                

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;5-GUESS-ALGO;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (convert-to-ball-list l)
   (lc (list-ref colour-ball-list (- x 1)) : x <- l))

(define (input l)
    (define ball-list (convert-to-ball-list l))
    (set! final-board-2 (place-images ball-list
                  (build-list (length ball-list) (lambda (z) (make-posn (+ (* 0.05 width) (/ x 2) (* x z)) (/ (+ height (* y (+ 1 (- (no-of-trials) (* 2 guess-no))))) 2))))
                  final-board-2))
  final-board-2)

(define r/3 (/ radius 3))

(define black-button-location
  (cons (cons (- (+ (* width 0.05) (* 0.83 width)) r/3) (+ (+ (* width 0.05) (* 0.83 width)) r/3))
        (cons (- (/ height 60) r/3) (+ (/ height 60) r/3))))

(define white-button-location
  (cons (cons (- (+ (* 4 r/3) (+ (* width 0.05) (* 0.83 width))) r/3) (+ (+ (* 4 r/3) (+ (* width 0.05) (* 0.83 width))) r/3))
        (cons (- (/ height 60) r/3) (+ (/ height 60) r/3))))

(define done-button-location
  (cons (cons (- (- (+ (* width 0.05) (* 0.83 width)) (* 8 r/3)) (/ box-width 4)) (+ (- (+ (* width 0.05) (* 0.83 width)) (* 8 r/3)) (/ box-width 4)))
        (cons (- (/ height 60) (/ box-height 2)) (+ (/ height 60) (/ box-height 2)))))

(define black-cnt 0)
(define white-cnt 0)
(define call-game-world? #f)
(define call-game-world2? #f)
(define (update-game-world w x y)
  (cond [(inside? done-button-location x y) (if (equal? black-cnt (car (choose-from))) (begin (set! set-no (- set-no 1))
                                                                                              (set! w "vs-game-finish")
                                                                                              (set! computer-score (+ computer-score guess-no))
                                                                                              (set! black-cnt 0)
                                                                                              (set! white-cnt 0)
                                                                                              (set! code-list '())
                                                                                              (set! levels-guess-no 1)
                                                                                              w)
                                                
                                                (begin  (set! pair (cons white-cnt black-cnt))
                                                       (set! black-cnt 0)
                                                       (set! white-cnt 0)
                                                       (displayln "xyz")
                                                       (if (equal? 4 (car (choose-from))) (set! call-game-world? #t)
                                                           (set! call-game-world2? #t))
                                                       (set! w "game-world-bw") w))]
        ;[(equal? n (+ black-cnt white-cnt)) (set! final-board-2 (place-black-white guess-no)) (cons white-cnt black-cnt)]      ;see
        [(inside? black-button-location x y) (set! black-cnt (+ 1 black-cnt)) (displayln "black") (place-black-white guess-no) (set! w "game-world-bw") w]
        [(inside? white-button-location x y) (set! white-cnt (+ 1 white-cnt)) (displayln "white") (place-black-white guess-no) (set! w "game-world-bw") w]
        [else w]))

(define (place-black-white guess-no)
  (let ([sum (+ white-cnt black-cnt)]
        [p (car (choose-from))])
    (cond [(<= x 4)
        
  (set! final-board-2 (place-images 
   (append (build-list black-cnt (lambda (x) (circle (/ radius 2) 'solid 'black))) (build-list white-cnt (lambda (x) (circle (/ radius 2) 'outline 'black)))) 
     (cond [(<= sum (ceiling (/ p 2))) (build-list sum (lambda (z) (make-posn (- (+ (* z (/ x 2)) (* 1.05 width) (/ x 2)) (* x (ceiling (/ p 2))))
                                                                             (+ (/ (+ height (* y (+ 1 (- (no-of-trials) (* 2 guess-no))))) 2) (/ y 4)))))]
           [else (append (build-list (ceiling (/ p 2)) (lambda (z) (make-posn (- (+ (* z (/ x 2)) (* 1.05 width) (/ x 2)) (* x (ceiling (/ p 2))))
                                                                             (+  (/ (+ height (* y (+ 1 (- (no-of-trials) (* 2 guess-no))))) 2) (/ y 4)))))
                         (build-list (- sum (ceiling (/ p 2))) (lambda (z) (make-posn (- (+ (* z (/ x 2)) (* 1.05 width) (/ x 2)) (* x (ceiling (/ p 2))))
                                                                             (- (/ (+ height (* y (+ 1 (- (no-of-trials) (* 2 guess-no))))) 2) (/ y 4))))))])
   final-board-2))]
          [(= p 5)

  (set! final-board-2 (place-images 
   (append (build-list black-cnt (lambda (x) (circle (/ radius 2) 'solid 'black))) (build-list white-cnt (lambda (x) (circle (/ radius 2) 'outline 'black)))) 
     (cond [(<= sum (ceiling (/ p 2))) (build-list sum (lambda (z) (make-posn (- (+ (* z (/ x 2)) (* 1.05 width) (/ x 2)) (- (* x (ceiling (/ p 2))) (* 0.006 p width)))
                                                                             (+ (/ (+ height (* y (+ 1 (- (no-of-trials) (* 2 guess-no))))) 2) (/ y 4)))))]
           [else (append (build-list (ceiling (/ p 2)) (lambda (z) (make-posn (- (+ (* z (/ x 2)) (* 1.05 width) (/ x 2)) (- (* x (ceiling (/ p 2))) (* 0.006 p width)))
                                                                             (+  (/ (+ height (* y (+ 1 (- (no-of-trials) (* 2 guess-no))))) 2) (/ y 4)))))
                         (build-list (- sum (ceiling (/ p 2))) (lambda (z) (make-posn (- (+ (* z (/ x 2)) (* 1.05 width) (/ x 2)) (- (* x (ceiling (/ p 2))) (* 0.006 p width)))
                                                                             (- (/ (+ height (* y (+ 1 (- (no-of-trials) (* 2 guess-no))))) 2) (/ y 4))))))])
   final-board-2))]

          [else
           (set! final-board-2 (place-images 
   (append (build-list black-cnt (lambda (x) (circle (/ radius 2) 'solid 'black))) (build-list white-cnt (lambda (x) (circle (/ radius 2) 'outline 'black)))) 
     (cond [(<= sum (ceiling (/ p 2))) (build-list sum (lambda (z) (make-posn (- (+ (* z (/ x 2)) (* 1.05 width) (/ x 2)) (- (* x (ceiling (/ p 2))) (* 0.003 p width) ))
                                                                             (+ (/ (+ height (* y (+ 1 (- (no-of-trials) (* 2 guess-no))))) 2) (/ y 4)))))]
           [else (append (build-list (ceiling (/ p 2)) (lambda (z) (make-posn (- (+ (* z (/ x 2)) (* 1.05 width) (/ x 2)) (- (* x (ceiling (/ p 2))) (* 0.003 p width) ))
                                                                             (+  (/ (+ height (* y (+ 1 (- (no-of-trials) (* 2 guess-no))))) 2) (/ y 4)))))
                         (build-list (- sum (ceiling (/ p 2))) (lambda (z) (make-posn (- (+ (* z (/ x 2)) (* 1.05 width) (/ x 2)) (- (* x (ceiling (/ p 2))) (* 0.003 p width) ))
                                                                             (- (/ (+ height (* y (+ 1 (- (no-of-trials) (* 2 guess-no))))) 2) (/ y 4))))))])
   final-board-2))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Genetic Algorithm;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (genetic-game-window2)
  (displayln "abc2")
  (begin (genetic-set-balls! (cdr (choose-from)) (car (choose-from)) pair)
         (set! guess-no (+ 1 guess-no))
  (input (what-to-guess-genetic))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;TO-DRAW RENDER;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (next-window w)
  (cond [(equal? w "start-world") (start-window)]
        [(equal? w "play-world1") (play-window1)]
        [(equal? w "levels-world") (levels-window)]
        [(equal? w "easy-world") (emh-window)]
        [(equal? w "medium-world") (emh-window)]
        [(equal? w "hard-world") (emh-window)]
        [(equal? w "insane-world") (insane-window)]
        [(equal? w "vs-world") (vs-window)]
        [(equal? w "set-world") (levels-window)]
        [(equal? w "choose-code-world") (choose-code-window)]
        [(equal? w "game-world") (game-window)]
        [(equal? w "game-world2") sc] ;(displayln "hi") (game-window2)]
        [(equal? w "game-world-bw") (game-window-bw)]
        [(equal? w "levels-game-world") (levels-game-window)]
        [(equal? w "levels-game-world2") (levels-game-window2)]
        [(equal? w "levels-game-finish") (levels-game-finish-window)]
        [(equal? w "vs-game-finish") (vs-game-finish-window)]
        [(equal? w "levels-mode-copy") (levels-game-window)]
        [(equal? w "levels-mode-copy2") (levels-game-window2)]
        [(equal? w "win/lost-world") (won/lost-window)]
        [(equal? w "online-world") (online-window)]
        [(equal? w "instructions-world") (instructions-window)]
        [(equal? w "genetic-game-world") (game-window)]
        [(equal? w "genetic-game-world2") sc2]
        [(equal? w "about-world") (about-window)]
        ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;QUIT-FN;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (quit-fn w)
  (if (equal? w "quit-world") #t #f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;CHOOSE-NEXT;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (choose-next w x y mouse-input)
  (cond [(equal? mouse-input "button-down") (update w x y)]
        [(equal? mouse-input "move") (highlight w x y)]
        [else w]))

(define (highlight w x y)
  (cond [(equal? w "start-world") (highlight-start-world w x y)]
        [(equal? w "play-world1") (highlight-play-world1 w x y)]
        [(equal? w "levels-world") (highlight-levels-world w x y)]
        [(equal? w "easy-world") (highlight-emh-world w x y)]
        [(equal? w "medium-world") (highlight-emh-world w x y)]
        [(equal? w "hard-world") (highlight-emh-world w x y)]
        [(equal? w "insane-world") (highlight-insane-world w x y)]
        [(equal? w "vs-world") (highlight-vs-world w x y)]
        [(equal? w "set-world") (highlight-levels-world w x y)]
        [(equal? w "choose-code-world") (highlight-choose-code-world w x y)]
        [(equal? w "levels-game-finish") (highlight-levels-game-finish-world w x y)]
        [(equal? w "vs-game-finish") (highlight-levels-game-finish-world w x y)]
        [(equal? w "won/lost-world") (highlight-levels-game-finish-world w x y)]
        [(equal? w "online-world") (highlight-online-world w x y)]
        [(equal? w "instructions-world") (highlight-instructions-world w x y)]
        [(equal? w "about-world") (highlight-about-world w x y)]
        [else w]))

(define (update w x y)
  (cond [(equal? w "start-world") (update-start-world w x y)]
        [(equal? w "play-world1") (update-play-world1 w x y)]
        [(equal? w "levels-world") (update-levels-world w x y)]
        [(equal? w "easy-world") (update-easy-world w x y)]
        [(equal? w "medium-world") (update-medium-world w x y)]
        [(equal? w "hard-world") (update-hard-world w x y)]
        [(equal? w "insane-world") (update-insane-world w x y)]
        [(equal? w "vs-world") (update-vs-world w x y)]
        [(equal? w "set-world") (update-set-world w x y)]
        [(equal? w "choose-code-world") (update-choose-code-world w x y)]
        [(equal? w "game-world") (update-game-world w x y)]
        [(equal? w "game-world2") (update-game-world w x y)]
        [(equal? w "game-world-bw") (update-game-world w x y)]
        [(equal? w "levels-game-world") (update-levels-game-world w x y)]
        [(equal? w "levels-game-world2") (update-levels-game-world w x y)]
        [(equal? w "levels-game-finish") (update-levels-game-finish-world w x y)]
        [(equal? w "vs-game-finish") (update-vs-game-finish-world w x y)]
        [(equal? w "levels-mode-copy") (update-levels-mode-copy w x y)]
        [(equal? w "levels-mode-copy2") (update-levels-mode-copy w x y)]
        [(equal? w "won/lost-world") (update-won/lost-world w x y)]
        [(equal? w "online-world") (update-online-world w x y)]
        [(equal? w "instructions-world") (update-instructions-world w x y)]
        [(equal? w "genetic-game-world") (update-game-world w x y)]
        [(equal? w "genetic-game-world2") (update-game-world w x y)]
        [(equal? w "about-world") (update-about-world w x y)]
        [else w]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;big-bang;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(big-bang "start-world"
  (to-draw next-window)
  (on-mouse choose-next)
  (on-tick (lambda (stat) (cond [call-game-world? (set! call-game-world? #f)
                                             (set! sc (game-window2))
                                             "game-world2"]
                                [call-game-world2? (set! call-game-world2? #f)
                                                   (set! sc2 (genetic-game-window2))
                                                   "genetic-game-world2"]
                                [else stat])))
  (stop-when quit-fn)
  (close-on-stop #t))