#lang racket

(require plot
         plot/utils
         racket/math)

(define (vector-foldl f f0 v)
  (foldl f f0 (vector->list v)))

(define (normalize-vector v)
  (let ([norm (sqrt (vector-foldl + 0 (vector-map sqr v)))])
    (vector-map (lambda (x) (/ x norm)) v)))


(define (plot-magnets K)
  (plot
   (vector-field
    (lambda (x y) (vnormalize
                   (vector (- (* K (sin (- x y))) (sin x)) (- (* K (sin (- y x))) (sin y)))))
    (/ (- pi) 2) (/ pi 2)
    (/ (- pi) 2) (/ pi 2))
   #:out-file (string-append (number->string K) "-field.png")
   #:title (string-append "Magnets for K = " (number->string K))
   #:x-label "theta_1"
   #:y-label "theta_2"))

(plot-magnets 0.1)
(plot-magnets 0.5)
(plot-magnets 1.0)

(define (p3-diffeq mu theta omega)
  (vector
   omega
   (+
    (- (* omega (- 1 (* mu (cos theta)))))
    (- (sin theta)))))

(define (plot-p3 mu)
  (plot
   (vector-field
    (lambda (x y) (vnormalize
                   (p3-diffeq mu x y)))
    (- pi) pi
    -5 5)
   #:out-file (string-append (number->string mu) "-8-4-4.png")
   #:title (string-append "Vector field for \\mu = " (number->string mu))
   #:x-label "theta"
   #:y-label "omega"))
(plot (function (lambda (x) (sqr (cos x)))
                 0
                 1

                           )
                 #:out-file "wrong-homework-lol.png"
                 #:x-label "ΔL"
                 #:y-label "Intensity"
                 #:title "Plot of intensity cos^2(ωΔL/λ) = cos^2(2ᴨ ΔL)")
