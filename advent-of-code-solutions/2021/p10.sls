;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright (c) 2021 daemon
;; SPDX-License-Identifier: MIT
#!r6rs

(import (rnrs (6))
        (only (srfi :1) lset-union lset-intersection lset-difference)
        (only (srfi :13) string-map string-filter string-fold string-tokenize)
        (srfi :14)
        (srfi :26))

(define (get-input fn)
  (call-with-input-file fn
    (lambda (p)
      (let f ([input-line (get-line p)]
              [result '()])
        (if (eof-object? input-line)
            result
            (f (get-line p)
               (cons (string->list input-line) result)))))))

(define reward-alist
  '((#\) . 3)
    (#\] . 57)
    (#\} . 1197)
    (#\> . 25137)))

(define part2-reward-alist
  '((#\) . 1)
    (#\] . 2)
    (#\} . 3)
    (#\> . 4)))

(define open-parens-closed-parens-alist
  '((#\( . #\))
    (#\[ . #\])
    (#\{ . #\})
    (#\< . #\>)))

(define open-parens-list
  (map car open-parens-closed-parens-alist))
(define closed-parens-list
  (map cdr open-parens-closed-parens-alist))
(define closed-parens-open-parens-alist
  (map (lambda (x) (cons (cdr x) (car x))) open-parens-closed-parens-alist))

(define (get-closing-pair c)
  (cdr (assq c open-parens-closed-parens-alist)))
(define (get-opening-pair c)
  (cdr (assq c closed-parens-open-parens-alist)))

(define member?
  (lambda (x ls)
    (and (member x ls) #t)))

(define (find-first-mismatch lst)
  (let f ([lst (cdr lst)] [accum-lst (list (car lst))])
    (if (null? lst)
        #f
        (if (member? (car lst) closed-parens-list)
            (if (not (char=? (car accum-lst) (get-opening-pair (car lst))))
                (car lst)
                (f (cdr lst) (cdr accum-lst)))
            (f (cdr lst) (cons (car lst) accum-lst))))))

(define part1
  (fold-left (lambda (x y)
               (let ([mismatch-val (assq (find-first-mismatch y) reward-alist)])
                 (if mismatch-val
                     (+ x (cdr mismatch-val))
                     x))) 0 (get-input "p9-input.txt")))

(define (repair-line lst)
  (let f ([lst (cdr lst)]
          [remaining-open-parens (list (car lst))])
    (if (null? lst)
        (map (lambda (x) (get-closing-pair x)) remaining-open-parens)
        (if (member? (car lst) closed-parens-list)
            (f (cdr lst) (cdr remaining-open-parens))
            (f (cdr lst) (cons (car lst) remaining-open-parens))))
    ))

(define part2
  (let ([sol-lst
         (sort
          <
          (map
           (lambda (y)
             (fold-left
              (lambda (x z)
                (+ (* 5 x) (cdr (assq z part2-reward-alist))))
              0
              (repair-line y)))
           (filter (lambda (x) (not (find-first-mismatch x))) (get-input "p9-input.txt"))))])
    (list-ref sol-lst (floor (/ (length sol-lst) 2)))))
;; doesn't work. make a stack and just start popping off the top
