#lang racket

(define/contract (is-match s p)
  (-> string? string? boolean?)
  (define (is-match-list s p)
    (cond
      [(empty? p) (empty? s)]
      [(empty? s) (and (> (length p) 1)
                       (char=? (list-ref p 1) #\*)
                       (is-match-list s (list-tail p 2)))]
      [else
       (let ([matched (or (char=? (car p) #\.)
                          (char=? (car p) (car s)))])
         (if (or (and  (> (length p) 1)
                       (char=? #\* (list-ref p 1))))
             (or (and matched (is-match-list (cdr s) p))
                 (is-match-list s (list-tail p 2)))
             (and matched (is-match-list (cdr s) (cdr p)))))]))
  (is-match-list (string->list s) (string->list p)))


(is-match "ab" "a*b")
(is-match "b" "c*a*b")
(is-match "cab" "ca.*")
(is-match "aaa" "a*a")
