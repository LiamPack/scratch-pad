#lang racket

(require racket/list)

(define/contract (search nums target)
  (-> (listof exact-integer?) exact-integer? boolean?)
  (let ([m (member target nums)])
    (if m #t #f)))
(search '(2 5 6 0 0 1 2) 0)
