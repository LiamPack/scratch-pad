#lang racket

(require racket/math
         plot
         racket/match
         racket/format
         srfi/1)

(define (f-complex E A B k n2 n0)
  (+ A
     (* B E
        (+ (cos
            (* k
               (+ n0 (* n2 (sqr (magnitude E))))))
           (* 0+1i
              (sin
               (* k
                  (+ n0 (* n2 (sqr (magnitude E)))))))))))

(define (f I I-in C k n2 n0)
  (* I-in
     (+ 1
        (* C (cos
              (* k
                 (+  n0 (* n2 I))))))))


(define (df I I-in C k n2 n0)
  (- (* I-in C k n2 (sin
                     (* k
                        (+  n0 (* n2 I)))))))

;; box distance of N_x(\epsilon)
(define (num-closest-points x epsilon l)
  (count (lambda (y)
           (and
            (and (>= (first y) (- (first x) epsilon))
                 (<= (first y) (+ (first x) epsilon)))
            (and (>= (second y) (- (second x) epsilon))
                 (<= (second y) (+ (second x) epsilon)))))
         l))

(define (correlation-of-set epsilon l n-samples)
  (/ (foldl + 0
            (map (lambda (_) (num-closest-points (list-ref l (random (length l))) epsilon l))
                 (iota n-samples))) n-samples))

(define (plot-correlation-curve l n-samples params)
  (let* ([epsilons (stream->list (in-range 1e-2 1 1e-1))]
         [Cs (map (lambda (e)
                    (correlation-of-set e l n-samples)) epsilons)]
         [log-Cs (map log Cs)]
         [log-epsilons (map log epsilons)]
         [slope      (/ (- (last log-Cs) (first log-Cs))
                        (- (last log-epsilons) (first log-epsilons)))])
    (plot (list (lines (map list log-epsilons log-Cs)
                       #:label "C(epsilon)")
                (lines (list
                        (list (first log-epsilons) (first log-Cs))
                        (list (last log-epsilons) (last log-Cs)))
                       #:style 'dot-dash
                       #:label (format "Line, slope = ~s" (~r #:precision '(= 3) slope))))
          #:title (format "Correlation Curve, {A = ~s ; B = ~s ; k = ~s}"
                          (first params) (second params) (third params))
          #:x-label "log epsilon"
          #:y-label "log Correlation"
          #:out-file (format "~s_~s_~s_correlation.png" (first params) (second params) (third params)))))


(define (complex-to-list x)
  (list (real-part x) (imag-part x)))

(define (get-cycle-points l)
  (define tolerance 1e-3)
  (define cutoff-ref (floor (* 9/10 (length l))))
  (define L (list-tail l cutoff-ref))
  (for/fold ([lst '()]) ([(x i) (in-indexed L)])
    (if (memf (lambda (y) (<= (abs (- x y)) tolerance)) (list-tail L (+ i 1)))
        lst
        (cons x lst))))

(define (liapunov-from-cycle-points df params points)
  (* (/ 1 (length points)) (fold + 0 (map (lambda (x) (log (abs (apply df (cons x params))))) points))))

(define (derivative-from-cycle-points df params points)
  (fold * 1 (map (lambda (x) (abs (apply df (cons x params)))) points)))

(define (iterate-function f x0 n-iters)
  (reverse (for/fold ([lst (list x0)]) ([_ (in-range 1 n-iters)])
             (cons (f (first lst)) lst))))

(define (plot-orbit f x0 start end step #:id [fn "garbage"])
  (let* ([params (stream->list (in-range start end step))]
         [sequences (map
                     (lambda (p)
                       (list-tail (iterate-function (lambda (x) (f x p)) x0 2000) 1600))
                     params)])
    (plot (map (lambda (p s)
                 (points (map (lambda (x) (list p x)) s)
                         #:sym 'dot
                         #:alpha 0.1))
               params sequences)
          #:title "Orbit Diagram"
          #:x-label "C"
          #:y-label "I_n"
          #:out-file (format "~s.png" fn))))

(define (cobweb n-final I0 I-in C k n2 n0)
  (define (helper n In Is)
    (let ([Inp1 (f In I-in C k n2 n0)])
      (if (>= n n-final)
          (cons (list In Inp1) (cons (list In In) Is))
          (helper (+ 1 n) Inp1 (cons (list In Inp1) (cons (list In In) Is))))))
  (helper 0 I0 '()))

(define (plot-cobweb I0 params n-iters #:cutoff [cutoff 0] #:ofile [ofile ""])
  (define Is (reverse (apply cobweb (cons n-iters (cons I0 params)))))
  (values Is (plot (list (function (lambda (x) (apply f (cons x params)))
                                   -1 5 #:color 'red)
                         (function (lambda (x) x) #:color 'blue)
                         (lines (list-tail Is cutoff) #:alpha 0.8 #:color 'black))
                   #:out-file ofile
                   #:x-label "I_n"
                   #:y-label "I_{n+1}"
                   #:title (format "Cobweb, I_0 = ~s, {I_in = ~s ; C = ~s ; k = ~s}"
                                   I0 (first params) (second params) (third params)))))

(define (plot-complex I0 params n-iters
                      #:box [box '()] #:sym [sym 'dot] #:alpha [alpha 1.0]
                      #:id [id ""])
  (define Is-complex
    (for/fold ([lst (list I0)]) ([_ (in-range 1 n-iters)])
      (cons (apply f-complex (cons (first lst) params)) lst)))
  (define Is-listed (map complex-to-list Is-complex))
  (values (plot (list (points Is-listed #:sym sym #:alpha alpha)
                      (if (not (empty? box))
                          (rectangles
                           (list
                            (vector (ivl (first box) (second box)) (ivl (third box) (fourth box))))
                           #:style 'transparent
                           #:alpha 1.0)
                          (points '((0 0)))))
                #:out-file (format "~s_~s_~s_~s_~s.png" I0 (first params) (second params) (third params) id)
                #:title (format "Complex map, z_0 = ~s, {A = ~s ; B = ~s ; k = ~s}"
                                I0 (first params) (second params) (third params)))
          (plot-correlation-curve Is-listed 1000 params)
          Is-listed
          (if (not (empty? box))
              (plot (points Is-listed
                            #:x-min (first box) #:x-max (second box)
                            #:y-min (third box) #:y-max (fourth box)
                            #:sym 'dot)
                    #:out-file (format "~s_~s_~s_~s_~s_zoom.png" I0 (first params) (second params) (third params) id)
                    #:title (format "Complex map, z_0 = ~s, {A = ~s ; B = ~s ; k = ~s}"
                                    I0 (first params) (second params) (third params)))
              -1)))

(define (remove-cobweb Is)
  (let ([xs '()])
    (for-each
     (lambda (x)
       (when (not (= (first x) (second x)))
         (set! xs (cons (first x) xs)))) Is)
    (reverse xs)))

(define param-list
  '((0.4 (0.8 1 1 1 0) 1000 #:cutoff 0 #:ofile "map-stable.png");; pretty stable regime
    (0.4 (1 1.2 1 1 0) 10000 #:cutoff 1000 #:ofile "map-2cycle.png") ;;2-cycle
    (0.4 (1 1.9 1 1 0) 10000 #:cutoff 1000 #:ofile "map-4cycle.png") ;;4-cycle
    (0.4 (1 2.2 1 1 0) 10000 #:cutoff 1000 #:ofile "map-8cycle.png") ;; 8-cycle
    (0.4 (1 2.4 1 1 0) 1000 #:cutoff 1000 #:ofile "map-chaos.png") ;; chaos
    (0.4 (1 2.7 1 1 0) 10000 #:cutoff 1000 #:ofile "map-inter-chaos.png") ;; interspersed non-chaos
    (0.4 (1 2.8 1 1 0) 10000 #:cutoff 1000 #:ofile "map-ghosts.png") ;; ghosts
    ))

;; (define l (map (lambda (x)
;;                  (match x
;;                    ((list I0 params n-iters #:cutoff cutoff #:ofile ofile)
;;                     (define-values (Is plts) (plot-cobweb I0 params n-iters #:cutoff cutoff #:ofile ofile))
;;                     (define cycle-points (get-cycle-points (remove-cobweb Is)))
;;                     (list
;;                      plts
;;                      (liapunov-from-cycle-points df params cycle-points)
;;                      (derivative-from-cycle-points df params cycle-points))
;;                     ;; (print (liapunov-from-cycle-points df params cycle-points))
;;                     ;; (print (derivative-from-cycle-points df params cycle-points))
;;                     )))
;;                param-list))
;;l

;;;; period 2
;; ((lambda ()
;;    (define-values (plt1 correlation-plot Is zoom)
;;      (plot-complex 5.0+0i '(1.5 0.4 1 1 0) 10000  #:sym 'circle #:alpha 0.1 #:id "2cycle"))
;;    plt1 correlation-plot
;;    ))
;; ;; ;;;; smearing into higher period
;; ((lambda ()
;;    (define-values (plt1 correlation-plot Is zoom) (plot-complex 5.0 '(1.64 0.4 1 1 0) 10000 #:sym 'circle #:alpha 0.1 #:id "2smear"))
;;    plt1 correlation-plot
;;    ))
;; ;; ;;;; period 4 (doubled, smear shown)
;; ((lambda () (define-values (plt1 correlation-plot Is-listed p2) (plot-complex 5.0 '(1.643 0.4 1 1 0) 10000 #:sym 'circle #:alpha 0.1 #:id "2smear-again"))
;;    plt1 correlation-plot
;;    ))
;; ((lambda () (define-values (plt1 correlation-plot Is-listed p2) (plot-complex 5.0 '(1.6435 0.4 1 1 0) 10000 #:sym 'circle #:alpha 0.1 #:id "4cycle"))
;;    plt1 correlation-plot))


;; ;; ;;;; period 8
;; ((lambda () (define-values (plt1 correlation-plot Is-listed p2) (plot-complex 5.0 '(1.7 0.4 1 1 0) 10000 #:sym 'circle #:alpha 0.1 #:id "complex-8cycle.png"))
;;    plt1 correlation-plot
;;    ))

;; ;; ;;;; ... chaos
;; ((lambda () (define-values (plt1 correlation-plot Is-listed p2) (plot-complex 5.0 '(1.9 0.4 1 1 0) 10000 #:sym 'dot #:alpha 0.1 #:box '(1.7 2.2 0.4 0.6) #:id "chaos"))
;;    plt1 p2 correlation-plot
;;    ))

;; ;; ;;;; already getting strange
;; ((lambda ()  (define-values (plt1 correlation-plot Is-listed p2) (plot-complex 5.0 '(2.0 0.4 1 1 0) 10000 #:box '(1.5 2.5 0.4 0.6) #:alpha 0.1 #:id "minchaos"))
;;    plt1 p2 correlation-plot
;;    ))

;; ;; ;;;; re-producing paper result
;; ((lambda () (define-values (plt1 correlation-plot Is-listed p2) (plot-complex 5.0 '(3.9 0.4 1 1 0) 15000 #:box '(3 4 -0.5 0.5) #:alpha 0.1 #:id "paper.png"))
;;    plt1 p2 correlation-plot
;;    ))

;; ;; heavy chaos
;; ((lambda () (define-values (plt1 correlation-plot Is-listed p2) (plot-complex 5.0 '(10.0 0.4 1 1 0) 30000 #:box '(7 8 -1 1) #:alpha 0.1 #:id "paper.png"))
;;    (values plt1 p2 correlation-plot)
;;    ))

;; n=0.05 gives logistic map back
;;(define g (lambda (x p) (f x 1.0 p 1.0 1.0 0)))
;; (plot-orbit g (/ (random 4000) 2000) 20.0 50.00 0.025 #:id "finite-map-orbit-plot-small-n2")
;;(plot-orbit g (/ (random 4000) 2000) 2.26 2.29 0.00001 #:id "finite-map-orbit-plot-small-2.26-2.29-zoom")
;;(define logistic (lambda (x r) (* r x (- 1 x))))
;;(plot-orbit logistic 0.4 2.75 5.0 0.0015 #:id "logistic-map-orbit-plot")


;; (define gp (lambda (x) (g x 3.0)))
;; (define lp (lambda (x) (logistic x 3.0)))
;; (plot (function (compose gp gp gp gp gp) -1 4))
;; (plot (function (compose lp lp lp lp lp) 0 1))
