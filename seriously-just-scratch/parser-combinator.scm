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

(define (pbyte pred)
  (lambda (bv ks kf)
    (if (= 0 (bytevector-length bv))
        (kf)
        (if (pred (bytevector-ref 0 bv))
            (ks (bytevector-ref 0 bv) (byte))))))
(define (byte/p b) (psym (lambda (c) (= b c))))

(define any-char/p
  (lambda (s ks kf)
    (if (not (null? s))
        (ks (car s) (cdr s))
        (begin
          (kf)))))

(define (repeat n p)
  (define (helper n1)
    (either/p
     (bind
      p (lambda (pv)
          (if (<= n1 0)
              fail
              (lift (helper (- n1 1))
                    (lambda (pvs) (cons pv pvs))))))
     empty/p))
  (helper n))
(define (take n)
  (lift (repeat n any-char/p) (lambda (x) (apply string x))))
(define take1 (take 1))



(define uint8/p
  (bind take1 (lambda (x)
                (if (< (string-length x) 1)
                    fail
                    (return (bytevector-u8-ref (string->utf8 x) 0))))))
(define uint16/p
  (bind (take 2) (lambda (x)  
                   (format #t "~a~%" (bytevector-u16-ref (string->utf8 x) 0 'big))
                   (if (< (string-length x) 2)
                       fail (return (bytevector-u16-ref (string->utf8 x) 0 (endianness big)))))))
(define uint32/p
  (bind (take 4) (lambda (x)  
                   (if (< (string-length x) 4)
                       fail (return (bytevector-u32-ref (string->utf8 x) 0 (endianness big)))))))
(define uint64/p
  (bind (take 8) (lambda (x)  
                   (if (< (string-length x) 8)
                       fail (return (bytevector-u64-ref (string->utf8 x) 0 (endianness big)))))))


(define int8/p
  (bind take1 (lambda (x)
                (if (< (string-length x) 1)
                    fail
                    (return (bytevector-s8-ref (string->utf8 x) 0))))))
(define int16/p
  (bind (take 2) (lambda (x)  
                   (if (< (string-length x) 2)
                       fail (return (bytevector-s16-ref (string->utf8 x) 0 (endianness big)))))))
(define int32/p
  (bind (take 4) (lambda (x)  
                   (if (< (string-length x) 4)
                       fail (return (bytevector-s32-ref (string->utf8 x) 0 (endianness big)))))))
(define int64/p
  (bind (take 8) (lambda (x)  
                   (if (< (string-length x) 8)
                       fail (return (bytevector-s64-ref (string->utf8 x) 0 (endianness big)))))))



(define float32/p
  (bind (take 4) (lambda (x)
                   (if (< (string-length x) 4)
                       fail
                       (bytevector-ieee-single-native-ref (string->utf8 x) 0)))))

(define float64/p
  (lift (take 8) (lambda (x)
                   (if (< (string-length x) 8)
                       fail
                       (bytevector-ieee-double-native-ref (string->utf8 x) 0)))))


(define in-file "/home/liamp/projects/slippi_parser/test/raw.slp")
(define (read-file in-file)
  (let ([tx (make-transcoder (latin-1-codec))])
    (call-with-port (open-file-input-port in-file (file-options) (buffer-mode block) tx)
      (lambda (p) (get-string-all p)))))

(define (run-parser p str)
  (p (string->list str)
     (lambda (v s) (format #t "Parser ran successfully. Result is ~a, remainder is.~%" (length v) ))
     (lambda () (format #t "Parser failed.~%"))))


(define event-payload-size/p (bind (char/p #\5) (lambda (x) uint8/p)))
(define other-event-payload/p
  (and/p take1 uint16/p))
(define (payload-sizes-map/p n)
  (repeat n other-event-payload/p))
(define all-payload-sizes/p
  (bind event-payload-size/p
        (lambda (size)
          (payload-sizes-map/p (/ (- size 1) 3)))))

(define-record-type version (fields major minor build unused))
(define-record-type game-start
  (fields
   version
   game-block-info
   random-seed
   dashback-fix
   shield-drop-fix
   nametags
   pal
   frozen-ps
   minor-scene
   major-scene
   display-names
   connect-codes
   slippi-uids
   language-option))

(define version/p
  (lift (repeat 4 uint8/p)
        (lambda (x) (apply make-version x))))
(define game-info-block/p
  (take 312))

(define game-start/p
  (lift
   (all-of/p
    version/p
    game-info-block/p
    uint32/p
    (repeat 4 uint32/p)
    (repeat 4 uint32/p)
    (repeat 4 (take 16))
    uint8/p
    uint8/p
    uint8/p
    uint8/p
    (repeat 4 (take 31))
    (repeat 4 (take 10))
    (return 0) ;; (repeat 4 (take 29))
    (return 0) ;; uint8/p
    )
   (lambda (x) (apply make-game-start x)))
  
  )

(define slippi/p
  (bind
   all-payload-sizes/p
   (lambda (payloads)
     (format #t "~a ~%" payloads)
     (let ([event/p
            (bind
             take1
             (lambda (s)
               (if (= 0 (string-length s))
                   fail
                   (case (string-ref s 0)
                     ;; cases 054 -> 061 and 016
                     [(#\6) game-start/p]
                     ;; [(#\7) take1 ]
                     ;; [(#\8) take1 ]
                     ;; [(#\9) take1 ]
                     ;; [(#\:) take1 ]
                     ;; [(#\;) ]
                     ;; [(#\<) take1 ]
                     ;;[(#\=) ]
                     ;; [(#\x10)]
                     [else  (take (cdr (assoc s payloads)))]))))])
       (many/p event/p)))))
