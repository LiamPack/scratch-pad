;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright (c) 2021 daemon
;; SPDX-License-Identifier: MIT
#!r6rs

(import (rnrs (6))
        (only (srfi :13) string-map string-filter string-fold string-tokenize))

(define (get-input fn)
  (call-with-input-file fn
    (lambda (p)
      (map string->number
           (string-tokenize (string-map (lambda (c) (if (char=? c #\,) #\space c)) (get-line p)))))))

(define (find-R crab-positions dist-fn)
  (do ([i 0 (+ 1 i)]
       [result 0]
       [last-add +inf.0])
      ((>= i (apply max crab-positions)) result)
    (let ([new-add (fold-left (lambda (x y) (+ x (dist-fn y i))) 0 crab-positions)])
      (when (<= new-add last-add)
        (set! result new-add))
      (set! last-add new-add))))

(define (n-dist-quadratic n R)
  (let ([dR (n-dist-linear n R)])
    (/ (* dR (+ dR 1)) 2)))

(define (n-dist-linear n R)
  (abs (- n R)))

;; part1 (alternatively, just compute the median and fold on euclidean distance
;; directly...).
(find-R (get-input "p7-input.txt") n-dist-linear)

;; part 2. if the input weren't small, idk what the minimizing criterion for (n*(n+1)/2) norm distance
(find-R (get-input "p7-input.txt") n-dist-quadratic)
