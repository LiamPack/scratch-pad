;; the below code stack-overflows at ~150 "many"s in kawa. Needs to have
;; tail-recursive definition, but idc

(define (return v) (lambda (s ks kf) (ks v s)))
(define fail (lambda (s ks kf) (kf)))
(define empty/p (return '()))

;; >>=
(define (bind a f)
  (lambda (s ks kf)
    (a s
       (lambda (av s1) ((f av) s1 ks kf))
       kf)))

;; >>|
(define (lift a f)
  (bind a (lambda (x) (return (f x)))))


;; <|>
(define (either/p a b)
  (lambda (s ks kf)
    (a s ks
       (lambda () (b s ks kf)))))

;; <*>
(define (and/p a b)
  (lambda (s ks kf)
    (a s
       (lambda (av s1)
         (b s1
            (lambda (bv s2) (ks (cons av bv) s2))
            kf))
       kf)))

(define (one-of/p . as)
  (fold-left either/p fail as))

(define (all-of/p . as)
  (fold-right and/p empty/p as))

(define (many/p p)
  (either/p
   (bind
    p (lambda (pv)
        (lift (many/p p) (lambda (pvs) (cons pv pvs)))))
   empty/p))

(define (psym pred)
  (lambda (s ks kf)
    (if (null? s)
        (kf)
        (if (pred (car s))
            (ks (car s) (cdr s))
            (kf)))))

(define (char/p a) (psym (lambda (c) (eq? a c))))

(define any-char/p
  (lambda (s ks kf)
    (if (not (null? s))
        (ks (car s) (cdr s))
        (begin
          (kf)))))

(define (take n)
  (define (helper n1)
    (either/p
     (bind
      any-char/p (lambda (pv)
                   (bind (helper (- n1 1))
                         (lambda (pvs) (if (<= n1 0) fail (return (cons pv pvs)))))))
     empty/p))
  (lift (helper n) (lambda (pv) (apply string pv))))
(define take1 (take 1))


(define uint8/p
  (lift take1 (lambda (x) (bytevector-u8-ref (u8-list->bytevector (map char->integer (string->list x))) 0))))
(define uint16/p
  (lift (take 2) (lambda (x) (bytevector-u16-ref (u8-list->bytevector (map char->integer (string->list x))) 0 (endianness big))))
  )
(define uint32/p
  (lift (take 4) (lambda (x) (bytevector-u32-ref (u8-list->bytevector (map char->integer (string->list x))) 0 (endianness big)))))

(define int8/p
  (lift take1 (lambda (x) (bytevector-s8-ref (u8-list->bytevector (map char->integer (string->list x))) 0))))
(define int16/p
  (lift (take 2) (lambda (x) (bytevector-s16-ref (u8-list->bytevector (map char->integer (string->list x))) 0 (endianness big))))
  )
(define int32/p
  (lift (take 4) (lambda (x) (bytevector-s32-ref (u8-list->bytevector (map char->integer (string->list x))) 0 (endianness big)))))


(define (run-parser p str)
  (p (string->list str)
     (lambda (v s) (format #t "Parser ran successfully. Result is ~a, remainder is ~a.~%" v (list->string s)))
     (lambda () (format #t "Parser failed.~%"))))
