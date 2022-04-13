#lang racket
;; inspired from https://github.com/Racket-Cookbooks/Plot-cookbook/blob/6ca895e0bef798cae22a914a12a5699f01f4c97b/examples/lorenz/README.md

(require (planet williams/science/ode-initval)
         plot)

(define (lorenz t y dy params)
  (let ((sigma (car params))
        (rho (second params))
        (beta (third params))
        (y0 (vector-ref y 0))
        (y1 (vector-ref y 1))
        (y2 (vector-ref y 2)))
    (vector-set! dy 0 (* sigma (- y1 y0)))
    (vector-set! dy 1 (- (* y0 (- rho y2)) y1))
    (vector-set! dy 2 (- (* y0 y1) (* beta y2)))))

(define y0-values '())
(define y1-values '())
(define y2-values '())
(define t-values '())
(define y (vector 1.0 0.0 0.5))

(define (reset-values)
  (set! y0-values '())
  (set! y1-values '())
  (set! y2-values '())
  (set! t-values '())
  (set! y (vector 1.0 0.0 0.5))
  )
(define (main sigma rho beta fn-prefix)
  (let* ((type rk4-ode-type)
         (step (make-ode-step type 3))
         (params (list sigma rho beta))
         (system (make-ode-system lorenz #f 3 params))
         (t 0.0)
         (t1 150.0)
         (h 0.0001)
         (y-err (make-vector 3))
         (dydt-in (make-vector 3))
         (dydt-out (make-vector 3)))
    (ode-system-function-eval system t y dydt-in)
    (set! y0-values (cons (vector-ref y 0) y0-values))
    (set! y1-values (cons (vector-ref y 1) y1-values))
    (set! y2-values (cons (vector-ref y 2) y2-values))
    (set! t-values (cons t t-values))
    (let loop ()
      (when (< t t1)
        (ode-step-apply step t h
                        y y-err
                        dydt-in
                        dydt-out
                        system)
        (set! y0-values (cons (vector-ref y 0) y0-values))
        (set! y1-values (cons (vector-ref y 1) y1-values))
        (set! y2-values (cons (vector-ref y 2) y2-values))
        (set! t-values (cons t t-values))
        (vector-set! dydt-in 0 (vector-ref dydt-out 0))
        (vector-set! dydt-in 1 (vector-ref dydt-out 1))
        (vector-set! dydt-in 2 (vector-ref dydt-out 2))
        (set! t (+ t h))
        (loop)))
    (define x-t (plot (lines (map vector t-values y0-values) #:color "blue")
                      #:x-label "t"
                      #:y-label "x"
                      #:title (format "Trajectory for y0 = ~s ~s ~s" (last y0-values) (last y1-values) (last y2-values))
                      #:out-file (format "~a-~a-~a-~a-x-t.png" fn-prefix  (last y0-values) (last y1-values) (last y2-values))))
    (define y-t (plot (lines (map vector t-values y1-values) #:color "blue")
                      #:x-label "t"
                      #:y-label "y"
                      #:title (format "Trajectory for y0 = ~s ~s ~s" (last y0-values) (last y1-values) (last y2-values))
                      #:out-file (format "~a-~a-~a-~a-y-t.png" fn-prefix  (last y0-values) (last y1-values) (last y2-values))))
    (define x-z (plot (lines (map vector y0-values y2-values) #:color "blue")
                      #:x-label "x"
                      #:y-label  "z"
                      #:title (format "Trajectory for y0 = ~s ~s ~s" (last y0-values) (last y1-values) (last y2-values))
                      #:out-file (format "~a-~a-~a-~a-x-z.png" fn-prefix  (last y0-values) (last y1-values) (last y2-values))))
    (values x-t y-t x-z)))


;; #2 exploring sigma=10, rho=22, b=8/3
(set! y (vector 1.0 0.0 0.5))
(main 10.0 22. (/ 8. 3.) "problem-2")
(reset-values)

(set! y (vector -1.0 0.0 0.5))
(main 10.0 22. (/ 8. 3.) "problem-2")
(reset-values)

(set! y (vector 0.2 0.2 -0.7))
(main 10.0 22. (/ 8. 3.) "problem-2")
(reset-values)

;; #3 exploring sigma=10, b=8/3, r=100
(set! y (vector 1.0 0.0 0.5))
(main 10.0 100. (/ 8. 3.) "problem-3")
(reset-values)

(set! y (vector -1.0 0.0 0.5))
(main 10.0 100. (/ 8. 3.) "problem-3")
(reset-values)

(set! y (vector 10.0 10.0 -0.7))
(main 10.0 100. (/ 8. 3.) "problem-3")
(reset-values)

;; #5 exploring sigma=10, b=8/3, r=166.3
(set! y (vector 1.0 0.0 0.5))
(main 10.0 166.3 (/ 8. 3.) "problem-5")
(reset-values)

(set! y (vector -1.0 0.0 0.5))
(main 10.0 166.3 (/ 8. 3.) "problem-5")
(reset-values)

(set! y (vector 10.0 10.0 -0.7))
(main 10.0 166.3 (/ 8. 3.) "problem-5")
