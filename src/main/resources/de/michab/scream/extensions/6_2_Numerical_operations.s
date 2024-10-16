;
; Scream @ https://github.com/urschleim/scream
;
; Copyright © 1998-2024 Michael G. Binz
;

;;
;; r7rs 6.2.6 Numerical operations, p35
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scream definitions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Support operation for implementing the min and max functions.
 |
 | compare is a comparison function (cmp a b) => boolean.
 |#
(define (scream:min-max compare inexact-seen first . rest)

    (if (null? rest)
      (if inexact-seen
        (inexact first)
        first)
      (let ((next (car rest)))
        (apply scream:min-max
          compare
          (or inexact-seen (inexact? first) (inexact? next))
          (if (compare first next)
            first
            next)
          (cdr rest)))))

(define (scream:assert-number n)
  (if (number? n)
    n
    (error "TYPE_ERROR" scream:type:number n)))
   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; r7rs definitions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | (number? obj) procedure; r7rs 35
 |#
(define number?
  scream:number?)

#|
 | (complex? obj) procedure; r7rs 35
 |#
(define (complex? obj)
  (error "NOT_IMPLEMENTED" 'complex?))

#|
 | (real? obj) procedure; r7rs 35
 |#
(define (real? obj)
  (number? obj))

#|
 | (rational? obj) procedure; r7rs 35
 |#
(define (rational? obj)
  (error "NOT_IMPLEMENTED" 'rational?))

#|
 | (integer? obj) procedure; r7rs 35
 |#
(define (integer? obj)
  (if (number? obj)
    (= (round obj) obj)
    #f))

#|
 | exact?
 |#
(define (exact? x)
  (if (number? x)
      ((object x) ("isExact"))
      (error "TYPE_ERROR"
             scream:type:number
             (scream:typename x))))

#|
 | inexact?
 |#
(define (inexact? x)
  (not (exact? x)))

#|
 | (exact-integer? z) library procedure; r7rs 38
 |#
(define (exact-integer? z) 
  (and
    (integer? z)
    (exact? z)))

#|
 | (finite? z) inexact library procedure; r7rs 35
 |#
(define (finite? obj)
  (error "NOT_IMPLEMENTED" 'finite?))

#|
 | (infinite? z) inexact library procedure; r7rs 35
 |#
(define (infinite? obj)
  (error "NOT_IMPLEMENTED" 'infinite?))

#|
 | (nan? z) inexact library procedure; r7rs 36
 |#
(define (nan? obj)
  (error "NOT_IMPLEMENTED" 'nan?))

#|
 | (= z₁ z₂ ...)  procedure; r7rs 36
 |#
(define =
  scream:=
)

#|
 | (< z₁ z₂ ...)  procedure; r7rs 36
 |#
(define <
  scream:<
)

#|
 | (> z₁ z₂ ...)  procedure; r7rs 36
 |#
(define >
  scream:>
)

#|
 | (<= z₁ z₂ ...)  procedure; r7rs 36
 |#
(define <= 
  scream:<=
)
  
#|
 | (>= z₁ z₂ ...)  procedure; r7rs 36
 |#
(define >=
  scream:>=
)

#|
 | (zero? z)
 |#
(define (zero? z)
  (= 0 z))

#|
 | positive? - library procedure - r7rs p36
 |#
(define (positive? x)
  (> x 0))

#|
 | negative? - library procedure - r7rs p36
 |#
(define (negative? x)
  (< x 0))

#|
 | (odd? n) procedure; r7rs p36
 |#
(define (odd? n)
  (if (integer? n)
    ((object (exact n)) ("r7rsOddQ"))
    (error "TYPE_ERROR"
             scream:type:integer
             (scream:typename n))))

#|
 | (even? n) procedure; r7rs p36
 |#
(define (even? n)
  (not (odd? n)))

#|
 | (max x₁ x₂ ...)  procedure; r7rs p36
 |#
(define (max n . rest)
  (apply scream:min-max > #f n rest))

#|
 | (min x₁ x₂ ...)  procedure; r7rs p36
 |#
(define (min n . rest)
  (apply scream:min-max < #f n rest))

#|
 | (abs x) procedure; r7rs p36
 |#
(define (abs x)
  (cond
    ((exact? x)
      (scream:java:lang:math ("abs:long" x)))
    ((inexact? x)
      (scream:java:lang:math ("abs:double" x)))
    (else
      (error "TYPE_ERROR" scream:type:number (scream:typename x)))
  )
)

#|
 | (floor/ n₁ n₂) procedure r7rs p36
 |#
(define (floor/ n1 n2)
  (values
    (floor-quotient n1 n2)
    (floor-remainder n1 n2)))

#|
 | (floor-quotient n₁ n₂) procedure r7rs p36
 |#
(define (floor-quotient n1 n2)
  (if (not (integer? n1))
    (error "TYPE_ERROR" scream:type:integer n1))
  (if (not (integer? n2))
    (error "TYPE_ERROR" scream:type:integer n2))
    
  ((if (and (exact? n1) (exact? n2))
      exact
      inexact)
  (scream:java:lang:math ("floorDiv:long,long" (exact n1) (exact n2)))))

#|
 | (floor-remainder n₁ n₂) procedure r7rs p36
 |#
(define (floor-remainder n1 n2)
  (let ((nq (floor-quotient n1 n2)))
    (- n1 (* n2 nq))))

#|
 | (truncate/ n₁ n₂) procedure r7rs p36
 |#
(define (truncate/ n1 n2)
  (values
    (truncate-quotient n1 n2)
    (truncate-remainder n1 n2)))

#|
 | (truncate-quotient n₁ n₂) procedure r7rs p36
 |#
(define (truncate-quotient n1 n2)
  ((if (and (exact? n1) (exact? n2))
      exact
      inexact)
  (truncate (/ n1 n2))))

#|
 | (truncate-remainder n₁ n₂) procedure r7rs p36
 |#
(define (truncate-remainder n1 n2)
  (let ((nq (truncate-quotient n1 n2)))
    (- n1 (* n2 nq))))

#|
 | (quotient n₁ n₂) procedure; r7rs 6.2.6 p37
 |#
(define quotient truncate-quotient)

#|
 | (remainder n₁ n₂) procedure; r7rs 6.2.6 p37
 |#
(define remainder truncate-remainder)

#|
 | (modulo n₁ n₂) procedure; r7rs 6.2.6 p37
 |#
(define modulo floor-remainder)

#|
 | (gcd n₁ ...) procedure 6.2.6 p37
 |#
(define gcd
  (letrec 
    (
      (_gcd 
        (lambda (a b)
          (abs
            (if (zero? b)
              a
              (_gcd b (remainder a b))))))
            
      (_gcd-transitive
        (scream:to-transitive _gcd))
    )

    (lambda arguments
      (cond
        ((null? arguments)
          0)
        ((= 1 (length arguments))
          (if (number? (car arguments))
            (car arguments)
            (error "TYPE_ERROR" scream:type:integer n1)))
        (else
          (apply _gcd-transitive arguments))))
  )
)

#|
 | (lcm n₁ ...) procedure 6.2.6 p37
 |#
(define lcm
  (letrec 
    (
      (_lcm 
        (lambda (a b)
          (if (or (zero? a) (zero? b))
            0
            (abs (* b (floor (/ a (gcd a b))))))))

      (_lcm-exact
        (lambda (a b)
          (let
            (
              (inexact-seen (or (inexact? a) (inexact? b)))
              (result (_lcm a b))
            )

            (if inexact-seen
              result
              (exact result)))))

      (_lcm-transitive
        (scream:to-transitive _lcm-exact))
    )

    (lambda arguments
      (cond
        ((null? arguments)
          1)
        ((= 1 (length arguments))
          (if (number? (car arguments))
            (car arguments)
            (error "TYPE_ERROR" scream:type:integer n1)))
        (else
          (apply _lcm-transitive arguments))))
  )
)

#|
 | (numerator q)
 |#
(define (numerator q)
  (error "NOT_IMPLEMENTED" 'numerator))

#|
 | (denominator q)
 |#
(define (denominator q)
  (error "NOT_IMPLEMENTED" 'denominator))

#|
 | (floor x) procedure 6.2.6 p37
 |#
(define (floor x)
  (cond
    ((not (number? x))
      (error "TYPE_ERROR" scream:type:number x))
    ((integer? x)
      x)
    (else
      (round (scream:java:lang:math ("floor:double" x))))))

#|
 | (ceiling x) procedure - 6.2.6 p37
 |#
(define (ceiling x)
  (cond
    ((not (number? x))
      (error "TYPE_ERROR" scream:type:number x))
    ((integer? x)
      x)
    (else
      (round (scream:java:lang:math ("ceil:double" x))))))

#|
 | (truncate x) procedure; r7rs 6.2.6 p37
 |#
(define (truncate x)
  (cond
    ((exact? x)
      x)
    ((positive? x)
      (floor x))
    (else
      (ceiling x))))

#|
 | (round x) procedure r7rs p37
 |#
(define (round x)
  (if (exact? x)
    x
    (scream:java:lang:math ("rint:double" x))))

#|
 | (rationalize x y)
 |#
(define (rationalize x y)
  (error "NOT_IMPLEMENTED" 'rationalize))

#|
 | (exp z) inexact library procedure r7rs p38
 |#
(define (exp x)
  (scream:java:lang:math 
    ("exp:double" (scream:assert-number x))))

#|
 | (log z) inexact library procedure r7rs p38
 | (log z₁ z₂)
 |#
(define log

  (scream:delay-op (delay ; -->

  (case-lambda

    ((z)
      (scream:java:lang:math ("log:double" (scream:assert-number z))))

    ((z1 z2)
      (/ 
        (log (scream:assert-number z1))
        (log (scream:assert-number z2))))

  ) ; case-lambda

  )) ; <--
)

#|
 | (sin z) procedure r7rs p38
 |#
(define (sin z)
  (scream:java:lang:math 
    ("sin:double" (scream:assert-number z))))

#|
 | (cos z) procedure r7rs p38
 |#
(define (cos z)
  (scream:java:lang:math 
    ("cos:double" (scream:assert-number z))))

#|
 | (tan z) procedure r7rs p38
 |#
(define (tan z)
  (scream:java:lang:math
    ("tan:double" (scream:assert-number z))))

#|
 | (asin z) procedure r7rs p38
 |#
(define (asin z)
  (scream:java:lang:math 
    ("asin:double" (scream:assert-number z))))

#|
 | (acos z) procedure r7rs p38
 |#
(define (acos z)
  (scream:java:lang:math 
    ("acos:double" (scream:assert-number z))))

#|
 | (atan z) procedure r7rs p38
 |#
(define atan

  (scream:delay-op (delay ; -->

  (case-lambda

    ((z)
      (scream:java:lang:math 
        ("atan:double" (scream:assert-number z))))

    ((x y)
      (scream:java:lang:math 
        ("atan2:double,double" 
          (scream:assert-number x) 
          (scream:assert-number y))))

  ) ; case-lambda

  )) ; <--
)

#|
 | (square z)
 |#
(define (square z)
  (* (scream:assert-number z) z))

#|
 | (sqrt z) inexact library procedure; r7rs 38
 |#
(define (sqrt z) 
  (let ((result (scream:java:lang:math ("sqrt:double" (scream:assert-number z)))))
    (if (and (exact? z) (integer? result))
  	  (exact result)
      result
    )
  )
)

#|
 | (exact-integer-sqrt k) inexact library procedure; r7rs 38
 |#
(define (exact-integer-sqrt k)

  (if (not (and (integer? k) (>= k 0) (exact? k)))
    (error "TYPE_ERROR" "nonnegative exact integer" k))

  (let*
    (
      (result
        (exact (truncate (scream:java:lang:math ("sqrt:double" k)))))
      (result-square (* result result))
      (rest (- k result-square))
    )

    (values result rest)
  )
)

#|
 | (expt z₁ z₂) procedure; r7rs 6.2.6 p38
 |#
(define (expt z1 z2)
  (let 
    (
      (result 
        (scream:java:lang:math ("pow:double,double" (scream:assert-number z1) (scream:assert-number z2))))
    )
    
    (if (and (exact? z1) (exact? z2) (integer? result))
      (exact result)
      result)
  )
)

#|
 | (make-rectangular x₁ x₂)
 |#
(define (make-rectangular x1 x2)
  (error "NOT_IMPLEMENTED" 'make-rectangular))

#|
 | (make-polar x3 x4)
 |#
(define (make-polar x1 x2)
  (error "NOT_IMPLEMENTED" 'make-polar))

#|
 | (real-part z)
 |#
(define (real-part z)
  (error "NOT_IMPLEMENTED" 'real-part))

#|
 | (imag-part z)
 |#
(define (imag-part z)
  (error "NOT_IMPLEMENTED" 'imag-part))

#|
 | (magnitude z)
 |#
(define (magnitude z)
  (error "NOT_IMPLEMENTED" 'magnitude))

#|
 | (angle z)
 |#
(define (angle z)
  (error "NOT_IMPLEMENTED" 'angle))

#|
 | (inexact z) procedure; r7rs p39
 |#
(define (inexact z)
  (if (inexact? z)
    z
    (+ .0 z)))

#|
 | (exact z) procedure; r7rs p39
 |#
(define (exact z)
  (if (number? z)
    ((object z) ("r7rsExact"))
    (error "TYPE_ERROR" scream:type-number z)))

; string->number supports a radix argument up to the number of entries
; in this vector.
(define %character-table
  #( #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\A #\B #\C #\D #\E #\F #\G))

(define (%number->string number char-list radix)
  (if (> radix (vector-length %character-table))
  	(error "RADIX_NOT_SUPPORTED" radix (vector-length %character-table)))
  (if (zero? number)
    ; If we received an empty character list...
    (if (null? char-list)
      ; ...we actually return a zero character.
      (cons (vector-ref %character-table 0) char-list)
      ; ...in the other case we return only the character
      ; list that we computed yet.
      char-list)
    (%number->string
      (quotient number radix)
      (cons
         (vector-ref %character-table (modulo number radix))
         char-list)
      radix)))

;;
;; TODO handle floating point
;;
(define (number->string number . opt-radix)
  (let* (
         (radix
           (cond
             ; If no optional radix was defined...
             ((null? opt-radix)
                ; ...we use the human 10 as default.
                10)
             ((= 1 (length opt-radix))
                (car opt-radix))
             (else
               (error "TOO_MANY_ARGUMENTS" 2))))

         (abs-character-list
           (%number->string (abs number) () radix))
       )
       (if (negative? number)
         (set! abs-character-list
         	(cons #\- abs-character-list)))
       (list->string abs-character-list)))
