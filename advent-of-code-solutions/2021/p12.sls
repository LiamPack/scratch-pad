
;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright (c) 2021 daemon
;; SPDX-License-Identifier: MIT
#!r6rs

(import (rnrs (6))
        (only (srfi :13) string-map string-tokenize)
        (srfi :26) ;; cut
        )

(define (edge-to-node g edge)
  (let* ([in (car edge)]
         [out (cadr edge)])
    (hashtable-update! g in (lambda (l) (cons out l)) '())
    (hashtable-update! g out (lambda (l) (cons in l)) '())))

(define (get-edge line)
  (map string->symbol
       (string-tokenize
        (string-map
         (lambda (x) (if (char=? x #\-) #\space x))
         line))))

(define (get-input fn)
  (let ([data (call-with-input-file fn
                (lambda (p)
                  (let f ([input-line (get-line p)]
                          [result '()])
                    (if (eof-object? input-line)
                        result
                        (f (get-line p)
                           (cons (get-edge input-line) result))))))]
        [g (make-hash-table)])
    (for-each (cut edge-to-node g <>) data)
    g))

(define member?
  (lambda (x ls)
    (and (member x ls) #t)))

(define (big-cave? sym)
  (string=? (string-upcase (symbol->string sym)) (symbol->string sym)))

(define (visited-enough? sym visited-lst)
  (if (big-cave? sym)
      #f
      (member? sym visited-lst)))

(define (check-two-small-caves? lst)
  (let ([smalls-ht (make-hash-table)])
    (for-each (lambda (x)
                (unless (big-cave? x)
                  (hashtable-update! smalls-ht x (cut + 1 <>) 0))) lst)
    (exists (lambda (x) (>= x 2)) (vector->list (hashtable-values smalls-ht)))))

(define (visited-enough-pt2? sym visited-lst)
  (if (big-cave? sym)
      #f
      (if (check-two-small-caves? visited-lst)
          (member? sym visited-lst)
          (if (or (eq? sym 'start) (eq? sym 'end))
              (member? sym visited-lst)
              #f))))

(define (dfs g visited-criterion?)
  (let f ([cur-node 'start] [visited '(start)])
    (if (eq? cur-node 'end)
        1
        (fold-left (lambda (x y)
                     (+ x (f y (cons y visited))))
                   0
                   (filter (lambda (x)
                             (not (visited-criterion? x visited)))
                           (hashtable-ref g cur-node '()))))))

(define pt1-soln (dfs (get-input "p12-input.txt") visited-enough?))
(define pt2-soln (dfs (get-input "p12-input.txt") visited-enough-pt2?))
