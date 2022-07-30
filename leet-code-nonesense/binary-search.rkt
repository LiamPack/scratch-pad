#lang racket


(define/contract (search nums target)
  (-> (listof exact-integer?) exact-integer? exact-integer?)
  (define (helper L R)
    (if (<= L R)
        (let* ([m (floor (/ (+ L R) 2))]
               [mid (list-ref nums m)])

          (cond
            [(< mid target)
             (helper (+ m 1) R)]
            [(> mid target)
             (helper L (- m 1))]
            [#t m]))
        -1))
  (helper 0 (- (length nums) 1)))

(search '(-1 0 3 5 9 12) 3)
(search '(5) -5)
