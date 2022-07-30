#lang racket

;; first leet-code attempt. didn't know they accepted racket. so here we are :) 

(define/contract (two-sum nums target)
  (-> (listof exact-integer?) exact-integer? (listof exact-integer?))
  (define (find-sum n nums-rest j)
    (if (empty? nums-rest)
        -1
        (if (= (+ n (car nums-rest)) target)
            j
            (find-sum n (cdr nums-rest) (+ j 1)))))

  (define (iterate-through-nums nums i)
    (if (<= (length nums) 1)
        #f
        (let* ([n (car nums)]
               [j (find-sum n (cdr nums) (+ 1 i))])
          (if (not (negative? j))
              (list i j)
              (iterate-through-nums (cdr nums) (+ 1 i))))))
                 
  (iterate-through-nums nums 0))
