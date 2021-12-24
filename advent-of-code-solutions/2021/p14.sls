
;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright (c) 2021 daemon
;; SPDX-License-Identifier: MIT
#!r6rs

(import (rnrs (6))
        (only (srfi :13) string-map string-tokenize)
        (srfi :26) ;; cut
        )

(define (process-line line)
  (if (or (string=?  "" line) (string=? (car (string-tokenize line)) line))
      line
      (remove "->" (string-tokenize line))))

(define (get-input fn)
  (let ([data (call-with-input-file fn
                (lambda (p)
                  (let f ([input-line (get-line p)]
                          [result '()])
                    (if (eof-object? input-line)
                        (reverse result)
                        (f (get-line p)
                           (cons (process-line input-line) result))))))])
    ;; input, rules
    (values (car data) (cddr data))))

(define (apply-rules-lil rules s)
  (let ([rule (find (lambda (x) (string=? (car x) s)) rules)])
    (if rule
        (string (string-ref s 0) (string-ref (cadr rule) 0))
        s)))

(define (apply-rules rules s)
  (let ([result ""])
    (do ([i 0 (+ 1 i)])
        ((>= i (- (string-length s) 1)) result)
      (set! result
            (string-append result (apply-rules-lil rules (substring s i (+ i 2))))))
    (string-append result (string (string-ref s (- (string-length s) 1))))))

(define (apply-rules-n-times rules s n)
  (let f ([i 0] [result s])
    (if (>= i n)
        result
        (f (+ i 1) (apply-rules rules result)))))

(define (count-most-and-least-freq s)
  (let ([frequency-ht (make-hash-table)])
    (for-each (lambda (x) (hashtable-update! frequency-ht x (cut + 1 <>) 0)) (string->list s))
    (cons (vector-ref (vector-sort < (hashtable-values frequency-ht)) 0) (vector-ref (vector-sort > (hashtable-values frequency-ht)) 0))))

(define pt1-soln
  (let-values ([(s rules) (get-input "input-p14.txt")])
    (let* ([least-most (count-most-and-least-freq (apply-rules-n-times rules s 10))])
      (- (cdr least-most) (car least-most)))))

;; (define pt2-soln
;;   (let-values ([(s rules) (get-input "input-p14.txt")])
;;     (let* ([least-most (count-most-and-least-freq (apply-rules-n-times rules s 40))])
;;       (- (cdr least-most) (car least-most)))))
