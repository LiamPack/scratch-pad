;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright (c) 2021 daemon
;; SPDX-License-Identifier: MIT
#!r6rs

(import (rnrs (6))
        (only (srfi :13) string-map string-filter string-fold string-tokenize))

(define (get-point-line s)
  (map string->number
       (remove "->"
               (string-tokenize
                (string-map (lambda (c) (if (char=? c #\,) #\space c)) s)))))

(define (get-input fn)
  (call-with-input-file fn
    (lambda (p)
      (let f ([x (get-line p)] [result '()])
        (if (eof-object? x)
            result
            (f (get-line p) (cons (get-point-line x) result)))))))

(define (sign x2 x1)
  (if (= x2 x1)
      0
      (/ (- x2 x1) (abs (- x2 x1)))))

(define (point-pair-to-line ps)
  (let* ([x1 (car ps)]
         [y1 (cadr ps)]
         [x2 (caddr ps)]
         [y2 (cadddr ps)]
         [x-direction (sign x2 x1)]
         [y-direction (sign y2 y1)])
    (do ([i 0 (+ x-direction i)]
         [j 0 (+ y-direction j)]
         [result '()])
        ((and (>= (abs i) (abs (- x2 x1)))
              (>= (abs j) (abs (- y2 y1))))
         (cons (list
                (+ x1 i)
                (+ y1 j))
               result))
      (set! result
            (cons (list
                   (+ x1 i)
                   (+ y1 j))
                  result)))))

(define (point-pair-to-line-no-diags ps)
  (let* ([x1 (car ps)]
         [y1 (cadr ps)]
         [x2 (caddr ps)]
         [y2 (cadddr ps)])
    (if (or (= x2 x1) (= y2 y1))
        (point-pair-to-line ps)
        '())))

(define (add-one position-ht position)
  (hashtable-set! position-ht position
                  (+ 1 (hashtable-ref position-ht position 0))))

(define (make-position-hashtable)
  (make-hashtable
   (lambda (ps) (+ (car ps) (* 10000 (cadr ps)))) ;; number big enough that p,0 never hashes to it
   (lambda (ps qs) (and (= (car ps) (car qs)) (= (cadr ps) (cadr qs))))))

(define (get-solution-1-ht fn)
  (let* ([input (get-input fn)]
         [ht (make-position-hashtable)]
         [position-lists (map point-pair-to-line-no-diags input)]
         [_ (map (lambda (x) (for-each (lambda (y) (add-one ht y)) x)) position-lists)])
    (call-with-values
        (lambda () (hashtable-entries ht))
      (lambda (_ vals)
        (fold-left (lambda (x y) (if (>= y 2) (+ x 1) x)) 0 (vector->list vals))))))

(define (get-solution-2-ht fn)
  (let* ([input (get-input fn)]
         [ht (make-position-hashtable)]
         [position-lists (map point-pair-to-line input)]
         [_ (map (lambda (x) (for-each (lambda (y) (add-one ht y)) x)) position-lists)])
    (call-with-values
        (lambda () (hashtable-entries ht))
      (lambda (_ vals)
        (fold-left (lambda (x y) (if (>= y 2) (+ x 1) x)) 0 (vector->list vals))))))
