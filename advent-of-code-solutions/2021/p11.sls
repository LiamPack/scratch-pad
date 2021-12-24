
;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright (c) 2021 daemon
;; SPDX-License-Identifier: MIT
#!r6rs

(import (rnrs (6))
        (only (srfi :1) delete-duplicates)
        (only (srfi :13) string-map string-filter string-fold string-tokenize)
        (chez-matrices)
        (srfi :14)
        (srfi :26))

(define-record-type octopi
  (fields (mutable board) n m))

(define (get-input fn)
  (let ([data (call-with-input-file fn
                (lambda (p)
                  (let f ([input-line (get-line p)]
                          [result '()])
                    (if (eof-object? input-line)
                        (list->vector result)
                        (f (get-line p)
                           (cons (list->vector (map (lambda (x) (- (char->integer x) 48)) (string->list input-line))) result))))))])
    (make-octopi data (vector-length data) (vector-length (vector-ref data 0)))))

(define (octopi-ref? l i j)
  (if (or (>= i (octopi-n l))
          (>= j (octopi-m l))
          (< j 0)
          (< i 0))
      #f
      (matrix-ref (octopi-board l) i j)))

(define (octopi-set! l i j v)
  (matrix-set! (octopi-board l) i j v))

(define (increment-neighborhood l n m)
  (if (> (octopi-ref? l n m) 9)
      (let ([indices-over-9 '()])
        (octopi-set! l n m 0)
        (do ([i (- n 1) (+ 1 i)])
            ((>= i (+ n 2)) indices-over-9)
          (do ([j (- m 1) (+ 1 j)])
              ((>= j (+ m 2)))
            (let ([octo-val (octopi-ref? l i j)])
              (when (and octo-val
                         (not (= 0 octo-val)))
                (octopi-set! l i j (+ 1 octo-val)))
              (when (and octo-val (> (octopi-ref? l i j) 9))
                (set! indices-over-9 (cons (list i j) indices-over-9)))))))
      '()))

(define (increment-board l)
  (let ([indices-over-9 '()])
    (do-matrix (octopi-board l) (i j)
               (octopi-set! l i j (+ 1 (octopi-ref? l i j)))
               (when (> (octopi-ref? l i j) 9)
                 (set! indices-over-9 (cons (list i j) indices-over-9))))
    indices-over-9))

(define (octopi-dfs l indices)
  (let f ([indices indices] [num-flashes 0])
    (if (null? indices)
        num-flashes
        (let ([indices-over-9 (apply increment-neighborhood (cons l (car indices)))])
          (f (filter (lambda (x) (not (null? x))) (delete-duplicates (append indices-over-9 (cdr indices))))
             (+ num-flashes 1))))))

(define (simulate l n-times)
  (let ([num-flashes 0])
    (do ([i 0 (+ 1 i)])
        ((>= i n-times) (list num-flashes l))
      (let ([indices-over-9 (increment-board l)])
        (set! num-flashes (+ num-flashes (octopi-dfs l indices-over-9))))
      (when (= 0 (matrix-fold + 0 (octopi-board l)))
        (display (+ 1 i))
        (display "\n")
        (display l)
        (display "\n")))))

(define p1-soln (simulate (get-input "p11-input.txt") 100))
(define p2-soln (simulate (get-input "p11-input.txt") 250)) ;; it'll print, mine was 242
