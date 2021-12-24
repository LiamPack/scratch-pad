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
               (cons (cons (list-head (string-tokenize input-line) 10) (list-tail (string-tokenize input-line) 11)) result)))))))

(define letters-number-alist
  '(("abcefg" . 0)
    ("cf" . 1)
    ("acdeg" . 2)
    ("acdfg" . 3)
    ("bcdf" . 4)
    ("abdfg" . 5)
    ("abdefg" . 6)
    ("acf" . 7)
    ("abcdefg" . 8)
    ("abcdfg" . 9)))

(define (count-frequencies-in-list lst)
  (let ([letter-map (make-hash-table)])
    (for-each
     (lambda (x)
       (for-each
        (lambda (y)
          (hashtable-update! letter-map y (cut + 1 <>) 0))
        (string->list x)))
     lst)
    letter-map))

(define letter-frequency-map
  (count-frequencies-in-list (map car letters-number-alist)))

(define (letters-to-number s)
  (fold-left  (lambda (x y) (+ x (hashtable-ref letter-frequency-map y 0))) 0 (string->list s)))

(define unique-number-to-letter-map
  (let ([number-map (make-hash-table)])
    (for-each (lambda (x) (hashtable-set! number-map (letters-to-number (car x)) (cdr x))) letters-number-alist)
    number-map))

(define (get-number-from-string s frequency-map)
  (hashtable-ref unique-number-to-letter-map
                 (fold-left
                  (lambda (x y)
                    (+ x
                       (hashtable-ref frequency-map y 0)))
                  0
                  (string->list s)) 0))

(define (count-numbers lsts)
  (let ([letter-frequency-map (count-frequencies-in-list (car lsts))])
    (fold-left
     (lambda (x y)
       (+ (* 10 x) (get-number-from-string y letter-frequency-map)))
     0
     (cdr lsts))))

(fold-left (lambda (x y) (+ x (count-numbers y))) 0 (get-input "p8-input.txt"))
