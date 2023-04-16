; Scream @ https://github.com/urschleim/scream
;
; Copyright © 2023 Michael G. Binz

;;
;; r7rs 6.9 p49
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scream definitions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Init type name.
(define scream:type-bytevector
  ((make-object de.michab.scream.fcos.Bytevector) TYPE_NAME))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; r7rs definitions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; (bytevector? obj) library procedure; 6.9 r7rs 49
;;
(define bytevector?
  (typePredicateGenerator "de.michab.scream.fcos.Bytevector" #t))

#|
 | (make-bytevector k)
 | (make-bytevector k byte) library procedure; r7rs 6.3 p40
 |#

(define scream:int-max #x7fffffff)

(define (make-bytevector k . byte)
  (if (not (integer? k))
    (error "TYPE_ERROR" k scream:type-integer))
  (if (or (< k 0) (> k scream:int-max))
    (error "RANGE_EXCEEDED" k "[0..#x7fffffff]"))

  (cond
    ;; If the optional argument is not given.
    ((null? byte)
      (make-object (de.michab.scream.fcos.Bytevector k)))
    ;; If the optional argument exists.
    ((= (length byte) 1)
      (let ((byte (car byte)))
        (cond
          ((not (integer? byte))
             (error "TYPE_ERROR" byte scream:type-integer))
          ((and
             (>= byte 0)
             (<= byte 255))
             (make-object (de.michab.scream.fcos.Bytevector k byte)))
          (else
             (error "RANGE_EXCEEDED" byte "[0..255]" ))
        )
      ))
    ;; If there are more than one optional arguments.
    (else (error "TOO_MANY_ARGUMENTS" 2))
  ) ; cond
)
