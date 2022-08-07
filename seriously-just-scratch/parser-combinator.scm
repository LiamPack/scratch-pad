;; the below code stack-overflows at ~150 "many"s in kawa. Needs to have
;; tail-recursive definition, but idc

(define (return v) (lambda (s ks kf) (ks v s)))
(define fail (lambda (s ks kf) (kf)))
(define empty/p (return '()))
(define peek1 (lambda (s ks kf)
                (if (null? s)
                    (kf)
                    (ks (car s) s))))

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
    (if (null? s)
        (kf)
        (ks (car s) (cdr s)))))

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
                       (return (bytevector-ieee-single-ref (string->utf8 x) 0 (endianness big)))))))

(define float64/p
  (lift (take 8) (lambda (x)
                   (if (< (string-length x) 8)
                       fail
                       (return (bytevector-ieee-double-ref (string->utf8 x) 0 (endianness big)))))))


(define in-file "/home/liamp/projects/slippi_parser/test/raw.slp")
(define (read-file in-file)
  (let ([tx (make-transcoder (latin-1-codec))])
    (call-with-port (open-file-input-port in-file (file-options) (buffer-mode block) tx)
      (lambda (p) (get-string-all p)))))

(define (run-parser p str)
  (p (string->list str)
     (lambda (v s) (format #t "Parser ran successfully. Result is ~a, remainder is ~a.~%" v (list->string s) ))
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
    (return '()) ;; (repeat 4 (take 29)) TODO: version-dependent parse..
    (return '()) ;; uint8/p
    )
   (lambda (l) (apply make-game-start l))))

(define-record-type pre-frame-update
  (fields
   frame_number
   player_index
   is_follower
   random_seed
   action_state_id
   x_position
   y_position
   facing_direction
   joystick_x
   joystick_y
   c_stick_x
   c_stick_y
   trigger
   processed_buttons
   physical_buttons
   physical_l_trigger
   physical_r_trigger
   x_analog_ucf
   percent))

(define pre-frame-update/p
  (lift (all-of/p
         int32/p
         int8/p
         int8/p
         int32/p
         int16/p
         float32/p
         float32/p
         float32/p
         float32/p
         float32/p
         float32/p
         float32/p
         float32/p
         int32/p
         int16/p
         float32/p
         float32/p
         int8/p
         float32/p
         ) (lambda (l) (apply make-pre-frame-update l))))

(define-record-type post-frame-update
  (fields
   frame_number 
   player_index 
   is_follower 
   internal_character_id 
   action_state_id 
   x_position 
   y_position 
   facing_direction 
   percent 
   shield_size 
   last_hitting_attack_id 
   current_combo_count 
   last_hit_by 
   stocks_remaining 
   action_state_frame_counter 
   state_bit_flags_1 
   state_bit_flags_2 
   state_bit_flags_3 
   state_bit_flags_4 
   state_bit_flags_5 
   misc_as 
   ground_air_state 
   last_ground_id 
   jumps_remaining 
   l_cancel_status 
   hurtbox_collision_state 
   self_induced_air_x_speed 
   self_induced_air_y_speed 
   attack_based_x_speed 
   attack_based_y_speed 
   self_induced_ground_x_speed 
   hitlag_frames_remaining 
   animation_index))

(define post-frame-update/p
  (lift (all-of/p
    int32/p
    uint8/p
    uint8/p
    uint8/p
    uint16/p
    float32/p
    float32/p
    float32/p
    float32/p
    float32/p
    uint8/p
    uint8/p
    uint8/p
    uint8/p
    float32/p
    uint8/p
    uint8/p
    uint8/p
    uint8/p
    uint8/p
    float32/p
    uint8/p
    uint16/p
    uint8/p
    uint8/p
    uint8/p
    float32/p
    float32/p
    float32/p
    float32/p
    float32/p
    float32/p
    (return '()))
        (lambda (l) (apply make-post-frame-update l))))

(define-record-type game-end (fields game-end-method lras-initiator))
(define game-end/p (lift (all-of/p uint8/p int8/p) (lambda (l) (apply make-game-end l))))

(define-record-type frame-start
  (fields
   frame-number
   random-seed
   scene-frame-counter))
(define frame-start/p
  (lift (all-of/p int32/p uint32/p (return '()))
        (lambda (l) (apply make-frame-start l))))

(define-record-type frame-bookend (fields frame-number latest-finalized-frame))
(define frame-bookend/p (lift (all-of/p int32/p uint32/p) (lambda (l) (apply make-frame-bookend l))))

(define slippi/p
  (bind
   all-payload-sizes/p
   (lambda (payloads)
     (let ([event/p
            (bind
             take1
             (lambda (s)
               (if (= 0 (string-length s))
                   fail
                   (case (string-ref s 0)
                     ;; cases 054 -> 061 and 016
                     [(#\6) game-start/p]
                     [(#\7) pre-frame-update/p]
                     [(#\8) post-frame-update/p ]
                     [(#\9) game-end/p ]
                     ;; [(#\:)  ]
                     ;; [(#\;)  ]
                     ;; [(#\<)  ]
                     ;; [(#\=)  ]
                     ;; [(#\x10)]
                     [else  (take (cdr (assoc s payloads)))]))))])
       (many/p event/p)))))
