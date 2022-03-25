#lang racket

(require racket/contract
         racket/list)


(define/contract (two-city-sched-cost costs)
  (-> (listof (listof exact-integer?)) exact-integer?)
  (define (refund x)
    (- (second x) (first x)))
  (let* ([N (/ (length costs) 2)]
         [refunds (foldl + 0 (take (sort (map refund costs) <) N))]
         [min-cost (foldl + 0 (map first costs))])
    (+ refunds min-cost)))

(define in '((10 20) (30 200) (400 50) (30 20)))
;; (define in '((10 10) (30 30) (400 400) (30 30)))

(two-city-sched-cost in)
