#lang racket

(define/contract (k-weakest-rows mat k)
  (-> (listof (listof exact-integer?)) exact-integer? (listof exact-integer?))
  (let* ([rows-added (map (lambda (x) (foldl + 0 x)) mat)]
         [rows-zipped (map list rows-added (range 0 (length mat)))]
         [sorted-1 (sort rows-zipped
                         < #:key first)])
    (map second (take sorted-1 k))))

(k-weakest-rows '((1 1 0 0 0)
                  (1 1 1 1 0)
                  (1 0 0 0 0)
                  (1 1 0 0 0)
                  (1 1 1 1 1))
                3)
(k-weakest-rows '((1 0 0 0)
                  (1 1 1 1)
                  (1 0 0 0)
                  (1 0 0 0))
                2)
