;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright (c) 2021 daemon
;; SPDX-License-Identifier: MIT
#!r6rs

(import (rnrs (6))
        (only (srfi :13) string-map string-filter string-fold string-tokenize))

;; Represent fish in groups of (timer . #fish with this timer). This is
;; basically just rudimentary list compression so that part 2 is doable in
;; human-length time. Instead of counting an exponentially-sized list, we pack
;; the size as a list of numbers which we can count in polynomial time in the
;; input (O(N*m), N is the input length, m is the number of steps you'd like to
;; push forward the simulation (since a new cons pair is made every iteration))

;; Thinkign back on it, this should be doable with just 9 list elements, where
;; the number in each element represents the number of fish in that
;; position. Then you can just shift the list around and add to the tail to get
;; a really slick O(1) (more like O(log n) if you count stored bits) memory
;; solution
(define (get-input fn)
  (call-with-input-file fn
    (lambda (p)
      (map (lambda (x) (cons (string->number x) 1))
           (string-tokenize (string-map (lambda (c) (if (char=? c #\,) #\space c)) (get-line p)))))))

(define days-to-new-fish 7)
(define new-fish-timer 8)

(define (update-fish-state state)
  (let* ([num-new-fish
          (fold-left
           (lambda (x y)
             (if (= 0 (car y))
                 (+ (cdr y) x)
                 x)) 0
                 state)]
         ;; It doesn't matter if it always makes one since we only count the
         ;; second of the pair for #fish. If there are no new fish, this is 0!
         [new-fish-list (cons new-fish-timer num-new-fish)])
    (cons new-fish-list
          (map (lambda (x) (if (= 0 (car x))
                               (cons (- days-to-new-fish 1) (cdr x))
                               (cons (- (car x) 1) (cdr x))))
               state))))

(define (apply-n g lst n-times)
  (let f ([i 0] [result lst])
    (if (>= i n-times)
        result
        (f (+ 1 i) (g result)))))

(define (get-num-fish state)
  (fold-left (lambda (x y)
               (+ x (cdr y)))
             0
             state))

(get-num-fish (apply-n update-fish-state (get-input "p6-input.txt") 256))
