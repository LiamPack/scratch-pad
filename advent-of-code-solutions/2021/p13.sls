
;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright (c) 2021 daemon
;; SPDX-License-Identifier: MIT
#!r6rs

(import (rnrs (6))
        (only (srfi :1) delete-duplicates)
        (only (srfi :13) string-map string-tokenize)
        (srfi :26) ;; cut
        )

(define (get-pair line)
  (if (and (not (string=? "" line)) (string=? line (car (string-tokenize line))))
      (map string->number
           (string-tokenize
            (string-map
             (lambda (x) (if (char=? x #\,) #\space x))
             line)))
      (if (not (string=? "" line))
          (cddr (string-tokenize (string-map (lambda (x) (if (char=? x #\=) #\space x)) line)))
          '())))

(define (get-folds-from-list lst)
  (let f ([lst lst] [folds '()])
    (if (null? (car lst))
        (values folds (cdr lst))
        (f (cdr lst) (cons (list (string->symbol (caar lst)) (string->number (cadar lst)) ) folds)))))

(define (get-input fn)
  (call-with-input-file fn
    (lambda (p)
      (let f ([input-line (get-line p)]
              [result '()])
        (if (eof-object? input-line)
            result
            (f (get-line p)
               (cons (get-pair input-line) result)
               ))))))

(define member?
  (lambda (x ls)
    (and (member x ls) #t)))

(define (fold-number axis val pair)
  (if (eq? axis 'x)
      (if (> (car pair) val)
          (list (+ (car pair) (* 2 (- val (car pair) )))
                (cadr pair))
          pair)
      (if (> (cadr pair) val)
          (list
           (car pair)
           (+ (cadr pair) (* 2 (- val (cadr pair) ))))
          pair)
      ))

(define (apply-fold axis val lst)
  (delete-duplicates (map (cut fold-number axis val <>) lst)))

(define pt1 (call-with-values
                (lambda () (get-folds-from-list (get-input "p13-input.txt")))
              (lambda (folds pairs)
                (let ([first-fold (car folds)])
                  (length (apply-fold (car first-fold) (cadr first-fold) pairs))))))

(define pt2 (call-with-values
                (lambda () (get-folds-from-list (get-input "p13-input.txt")))
              (lambda (folds pairs)
                (fold-left (lambda (x y) (apply-fold (car y) (cadr y) x)) pairs folds)
                )))

(define (display-values lst)
  (do ([i 0 (+ 1 i)])
      ((>= i 40))
    (do ([j 0 (+ 1 j)])
        ((>= j 40))
      (if (member? (list j i) lst)
          (display "#")
          (display "."))
      (display " "))
    (display "
")))

(display-values pt2)
