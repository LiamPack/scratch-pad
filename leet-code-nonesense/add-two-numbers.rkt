#lang racket

;;; super terribly ugly solution. still getting the hang of racket <2022-03-20 Sun>

; val : integer?
; next : (or/c list-node? #f)
(struct list-node
  (val next) #:mutable #:transparent)

; constructor
(define (make-list-node [val 0])
  (list-node val #f))


(define/contract (add-two-numbers l1 l2)
  (-> (or/c list-node? #f) (or/c list-node? #f) (or/c list-node? #f))

  (define (make-list-nodes vals)
    (define (make-list-node-rec rest-vals cur-node)
      (cond
        [(empty? rest-vals) cur-node]
        [#t (make-list-node-rec (cdr rest-vals) (list-node (car rest-vals) cur-node))]))
    (make-list-node-rec vals #f))

  (define (add-two-numbers-rec l1 l2 digit cur-sum)
    (cond
      [(and (not l1) (not l2)) cur-sum]
      [(not l1) (add-two-numbers-rec
                 #f
                 (list-node-next l2)
                 (+ 1 digit)
                 (+ cur-sum (* (expt 10 digit) (list-node-val l2))))]
      [(not l2) (add-two-numbers-rec
                 (list-node-next l1)
                 #f
                 (+ 1 digit)
                 (+ cur-sum (* (expt 10 digit) (list-node-val l1))))]
      [#t (add-two-numbers-rec
           (list-node-next l1)
           (list-node-next l2)
           (+ 1 digit)
           (+ cur-sum (* (expt 10 digit)
                         (+ (list-node-val l1) (list-node-val l2)))))]))

  (make-list-nodes
   (for/list ([c (in-string
                  (number->string (add-two-numbers-rec l1 l2 0 0)))])
     (- (char->integer c) 48))))
