
;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright (c) 2021 daemon
;; SPDX-License-Identifier: MIT
#!r6rs

(import (rnrs (6))
        (only (srfi :1) lset-union lset-intersection lset-difference)
        (only (srfi :13) string-map string-filter string-fold string-tokenize)
        (srfi :14)
        (srfi :26))

(define-record-type lava
  (fields board n m))

(define (get-input fn)
  (let ([data (call-with-input-file fn
                 (lambda (p)
                   (let f ([input-line (get-line p)]
                           [result '()])
                     (if (eof-object? input-line)
                         result
                         (f (get-line p)
                            (cons (map (lambda (x) (- (char->integer x) 48)) (string->list input-line)) result))))))])
    (make-lava  data (length data) (length (list-ref data 0)))))


(define (display-lava l)
  (display (lava-board l)))

(define (lava-ref? l i j)
  (if (or (>= i (lava-n l))
          (>= j (lava-m l)))
      #f
      (list-ref (list-ref (lava-board l) i) j)))
