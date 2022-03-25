#lang racket


(define/contract (partition-labels s)
  (-> string? (listof exact-integer?))
  (let* ([lasts (for/hash ([(s i) (in-indexed s)])
                  (values s i))])
    
    '(0)))

(define s "ababcbacadefegdehijhklij")
(define s-list (string->list "ababcbacadefegdehijhklij"))
(define ss '("ababcbaca" "defegde" "hijhklij"))

(partition-labels s)
