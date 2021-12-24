;; part 2 only here
(define (get-input fn)
  (call-with-input-file fn
    (lambda (p)
      (let f ([x (get-line p)])
        (if (eof-object? x)
            '()
            (cons x (f (get-line p))))))))

(define (bit-ref str i)
  (- (char->integer (string-ref str i)) 48))

(define (most-frequent-bit-at-i lst i)
  (if (>= (fold-left + 0
                     (map (lambda (x) (bit-ref x i)) lst))
          (/ (length lst) 2))
      1 0))

(define (find-density lst lst-reducer)
  (let find-density-helper ([lst lst] [i 0])
    (if (= (length lst) 1)
        (car lst)
        (find-density-helper
         (lst-reducer lst i)
         (1+ i)))))

(define (oxygen-reducer lst i)
  (filter (lambda (x) (= (bit-ref x i) (most-frequent-bit-at-i lst i))) lst))

(define (c02-reducer lst i)
  (filter (lambda (x) (not (= (bit-ref x i) (most-frequent-bit-at-i lst i)))) lst))

(define (bit-string->int s)
  (fold-left (lambda (x y)
               (+ (bitwise-arithmetic-shift-left x 1) (- (char->integer y) 48)))
             0
             (string->list s)))

(define oxygen-bs (find-density (get-input "input.txt") oxygen-reducer))
(define c02-bs (find-density (get-input "input.txt") c02-reducer))

(display (* (bit-string->int oxygen-bs) (bit-string->int c02-bs)))
