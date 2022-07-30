#lang racket

(require plot)
(require math)

(plot-new-window? #t)

(define (rhoz z)
  (sqrt (+ (sqr (sinh z)) 1)))

(plot3d (parametric-surface3d
         (lambda (z phi)
           (list (* (rhoz z) (cos phi))
                 (* (rhoz z) (sin phi))
                 z))
         -5 5 #:s-samples 50
         0 (* 2 pi)))
