; Scream @ https://github.com/urschleim/scream
;
; Copyright © 2024 Michael G. Binz

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; scream definitions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | (scream:files:validate-exists filename)
 |
 | Checks if a file exists.  If the file exists a file-object is returned.
 | Otherwise an IO_ERROR is thrown.
 |#
(define (scream:files:validate-exists filename)
  (let
    (
      (file (make-object (java.io.File filename)))
    )

    (if (not (file (exists)))
      (error "IO_ERROR" 'does-not-exist)
      file)))

#|
 | scream:files:current-dir
 |#
(define scream:files:current-dir
  "."
)

#|
 | (scream:files:list)
 | (scream:files:list filename)
 |
 | First form uses scream:files:current-dir.
 |#
(define scream:files:list

  (scream:delay-op (delay ; -->

  (case-lambda

    (()
      (scream:files:list scream:files:current-dir))

    ((dir)
      ((scream:files:validate-exists dir) (list)))

  ) ; case-lambda

  )) ; <--
)

#|
 | (scream:files:create filename)
 |
 | Creates the named file.  Returns #t if the file was created, #f if it
 | already existed.
 |#
(define (scream:files:create filename)
  (let
    (
      (file (make-object (java.io.File filename)))
    )
    
    (file (createNewFile)))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; r7rs definitions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | (load filename)
 | (load filename environment-specifier)  file library procedure; r7rs p59
 |#
(define (load filename)
  (let
    (
      (evaluator ((make-object de.michab.scream.ScreamEvaluator) (EVAL)))
    )
    
    (evaluator (load filename (interaction-environment))))
)

#|
 | (file-exists? filename)  file library procedure; r7rs p60
 |#
(define (file-exists? filename)
  (let
    (
      (file (make-object (java.io.File filename)))
    )
    
    (file (exists)))
)

#|
 | (delete-file filename)  file library procedure; r7rs p60
 |#
(define (delete-file filename)
  ((scream:files:validate-exists filename) (delete)))

;;
;; Environment variable operations.
;;

#|
 | (get-environment-variable name)  process context library procedure; r7rs p60
 |#
(define (get-environment-variable name)
  (let*
    (
      (system
        (make-object java.lang.System))
      (result 
        (system (getenv name)))
    )
    
    (if (null? result)
      #f
      result)
  )
)

;; TODO Candidate for string operations.
;; Get first index of char ins string.  Return index or #f.
(define (string-first char string)
  (define (_string-first idx char string)
    (cond
      ((= idx (string-length string))
        #f)
      ((eqv? char (string-ref string idx))
        idx) 
      (else
        (_string-first (+ idx 1) char string))))
  (_string-first 0 char string)
)

;; TODO Candidate for string operations.
;; Get last index of char ins string.  Return index or #f.
(define (string-last char string)
  (define (_string-last idx char string)
    (cond
      ((= idx -1)
        #f)
      ((eqv? char (string-ref string idx))
        idx)
      (else
        (_string-last (- idx 1) char string))))
  (_string-last (- (string-length string) 1) char string)
)

;; TODO Candidate for string operations.
;; MIT Scheme -- https://groups.csail.mit.edu/mac/ftpdir/scheme-7.4/doc-html/scheme_7.html
(define (substring? pattern string)
;  (scream:display-ln 'substring? pattern string)
  (call/cc
(begin
;    (scream:display-ln 'call/cc pattern string)

    (lambda (exit)
;      (scream:display-ln 'lambda pattern string)
      (let*
        (
          (p-len
            (string-length pattern))
          (p-first
            (if (> p-len 0)
              (string-ref pattern 0)
              (exit 0)))
          (s-len
             (string-length string))
        )

;       (scream:display-ln p-len p-first s-len)

        (if (> p-len s-len)
          (exit #f))

        (define (_substring? position)
          (let*
            (
              (current-pos
                (string-first
                  p-first
                  (substring string position (string-length string))))
            )

            (if (not current-pos)
              (exit #f))
              
            (if (string-prefix? pattern (substring string current-pos (string-length string)))
              (exit current-pos)
              (_substring (+1 current-pos)))))

        (_substring? 0)
      ) ; let*
    ) ; lambda
  ) ; begin
  ) ; call/cc
)

;; TODO Candidate for string operations.
;; MIT Scheme -- https://groups.csail.mit.edu/mac/ftpdir/scheme-7.4/doc-html/scheme_7.html
(define (string-prefix? prefix string)
  (let 
    (
      (prefix-len (string-length prefix))
      (string-len (string-length string))
    )
    (if (> prefix-len string-len)
      #f
      (equal?
        prefix
        (substring string 0 prefix-len)))
  )
)

; TODO Candidate for string operations.
(define (string-split pattern string)
  (define (_string-split result string)
    (let
      (
        (sub-position (substring? pattern string))
      )
      
      (if (not sub-position)
        (cons string result)
        (_string-split
          (cons
            (substring string 0 sub-position)
            result)
          (substring
            string
            (+ sub-position (string-length pattern))
            (string-length string)))))
  )

  (_string-split '() string)
)

#|
 | (get-environment-variables)  process context library procedure; r7rs p60
 |#
(define (get-environment-variables)
  (let*
    (
      (system (make-object java.lang.System))
      (env (system (getenv)))
      (keySet (env (keySet)))
      (keyVector (keySet (toArray)))
    )

    (vector->list (vector-map
      (lambda (key)
        (cons key (env (get key))))
      keyVector))
  )
)

#|
(define (get-environment-variables)

  ; Support: "a=b" => ("a" . "b")
  (define (split-at-equals string)
    (let
      (
        (split
          (string-split "=" string))
      )
    
      ; Compensate for (string-split ...) delivering
      ; its elements in reverse.
      (cons (cadr split) (car split))
    )
  )

  (let*
    (
      ; Get a reference to java.lang.System
      (system
        (make-object java.lang.System))
      ; Query the environment as a map.
      (env-map
        (system (getenv)))
      ; Create a buffer for converting the map into
      ; the result.
      (buffer (open-output-string))
      ; Write the toString representation into the buffer.
      (_ (write env-map buffer))
      ; Convert the toString presentation into a string.
      (environment-string
        (get-output-string buffer))
      ; Get the position of the first {.
      (brace-op
        (string-first #\{ environment-string))
      ; Get the position of the lsst {.
      (brace-cl
        (string-last #\} environment-string))
    )
    
;    (scream:display-ln brace-op brace-cl)
    (let*
      (
        ; Get the contents between the braces.
        (content-string
          (substring environment-string (+ brace-op 1) brace-cl))
        ; Split this at the ', ' positions.
        (variable-list
          (string-split ", " content-string))
      )

      ; Create the result list.
      (map split-at-equals variable-list)
    )
  )
)
|#

;;
;; Time operations
;;

#|
 | (current-jiffy)  time library procedure; r7rs p60
 |#
(define (current-jiffy)
  ((make-object java.lang.System) (currentTimeMillis)))

#|
 | (jiffies-per-second)  time library procedure; r7rs p60
 |#
(define (jiffies-per-second)
  1000)

#|
 | (current-second)  time library procedure; r7rs p60
 |#
(define (current-second)
  (/ (current-jiffy) (jiffies-per-second)))
