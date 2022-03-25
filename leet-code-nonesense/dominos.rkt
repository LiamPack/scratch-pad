#lang racket
(require racket/contract
         racket/set)

(define/contract (min-domino-rotations tops bottoms)
  (-> (listof exact-integer?) (listof exact-integer?) exact-integer?)
  (let* ([set-die (map set tops bottoms)]
         [left-elts (for/fold ([s (set-first set-die)])
                              ([s2 (in-set (set-rest set-die))])
                      (set-intersect s s2))])
    (if (set-empty? left-elts)
        -1
        (let* ([top-and-bottom-list (map list tops bottoms)]
               [dice-num (set-first left-elts)]
               [num-bottom-diffs
                (count (lambda (x) (and
                                    (not (= (first x) dice-num))
                                    (= (second x) dice-num))) top-and-bottom-list)]
               [num-top-diffs
                (count (lambda (x) (and
                                    (not (= (second x) dice-num))
                                    (= (first x) dice-num))) top-and-bottom-list)])
          (if (<= num-top-diffs num-bottom-diffs)
              num-top-diffs
              num-bottom-diffs)))))

(min-domino-rotations '(1 2 3 4) '(2 2 2 2))
(min-domino-rotations '(3 5 1 2 3) '(3 6 3 3 4))
(min-domino-rotations '(2 1 2 4 2 2) '(5 2 6 2 3 2))
