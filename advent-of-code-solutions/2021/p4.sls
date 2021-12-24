;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright (c) 2021 daemon
;; SPDX-License-Identifier: MIT
#!r6rs
(import (rnrs (6))
        (only (srfi :13) string-map string-filter string-fold string-tokenize))

(define (get-input fn)
  (call-with-input-file fn
    (lambda (p)
      ;; first lines the most annoying
      (let f ([x (map
                  string->number
                  (string-tokenize
                   (string-map (lambda (s) (if (char=? s #\,) #\  s))
                               (get-line p))))])
        (if (eof-object? x)
            '()
            (cons x (f (read p))))))))

(define (group-boards lst side-length)
  (letrec ([get-board
            (lambda (src lst)
              (if (or (null? src)
                      (>= (length lst) (* side-length side-length)))
                  (values src (reverse lst))
                  (get-board (cdr src) (cons (car src) lst))))])
    (let f ([source lst]
            [result '()])
      (if (null? source)
          (reverse result)
          (call-with-values
              (lambda () (get-board source '()))
            (lambda (source-rest next-board)
              (f source-rest (cons next-board result))))))))

(define (get-row-fn lst start side-length increment)
  (let f ([i 0] [result '()])
    (if (>= i side-length)
        (reverse result)
        (f (+ i 1) (cons (list-ref lst  (+ start (* i increment))) result)))))

(define (get-col lst start side-length) (get-row-fn lst start side-length side-length))
(define (get-row lst start side-length) (get-row-fn lst start side-length 1))

(define (get-all-bingo-lines lst side-length)
  (let ([result '()])
    ;; rows
    (do ([i 0 (+ 1 i)])
        ((>= i side-length))
      (set! result (cons (get-row lst (* i side-length) side-length) result)))
    ;; col
    (do ([i 0 (+ 1 i)])
        ((>= i side-length))
      (set! result (cons (get-col lst i side-length) result)))
    result))

(define member?
  (lambda (x ls)
    (and (member x ls) #t)))

(define (check-bingo line hit-numbers)
  (fold-left (lambda (x y) (and x y)) #t
             (map (lambda (x) (member? x hit-numbers)) line)))

(define (check-bingo-in-board board hit-numbers)
  (if (null? (filter (lambda (x) (check-bingo x hit-numbers)) (get-all-bingo-lines board global-side-length)))
      '()
      board))

(define traverse-boards-aggregate
  (lambda (boards numbers)
    (let f ([boards boards] [numbers numbers] [accum '()])
      (if (null? boards)
          accum
          (let ([maybe-bingos (if (not (null? (check-bingo-in-board (car boards) numbers)))
                                  (cons (car boards) accum)
                                  accum)])
            (f (cdr boards) numbers maybe-bingos))))))

(define (find-bingo boards hit-numbers end-cond)
  (let f ([numbers '()]
          [rest-numbers hit-numbers]
          [winning-boards (traverse-boards-aggregate boards '())]
          [boards-remaining boards])
    (if (or (null? rest-numbers) (end-cond winning-boards boards-remaining))
        (values (car winning-boards) numbers)
        (f (cons (car rest-numbers) numbers)
           (cdr rest-numbers)
           (traverse-boards-aggregate boards-remaining (cons (car rest-numbers) numbers))
           (fold-left (lambda (x y) (remove y x)) boards-remaining winning-boards)))))

(define (get-solution cond-fn)
  (call-with-values
      (lambda () (find-bingo input-boards input-numbers cond-fn))
    (lambda (board winning-numbers)
      (* (car winning-numbers)
         (fold-left + 0
                    (filter (lambda (x) (not (member? x winning-numbers))) board))))))

(define global-side-length 5)
(define input-tmp (get-input "p4-input.txt"))
(define input-numbers (car input-tmp))
(define input-boards  (group-boards (cdr input-tmp) global-side-length))
;; sol1
(get-solution (lambda (winning-boards _)
                (not (null? winning-boards))))
;; sol2
(get-solution (lambda (winning-boards boards-remaining)
                (and (null? boards-remaining)
                     (= 1 (length winning-boards)))))
