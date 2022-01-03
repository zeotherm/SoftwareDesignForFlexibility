;(define (compose f g)
;  (define (the-composition . args)
;    (call-with-values (lambda () (apply g args))
;      f))
;  (restrict-arity the-composition (get-arity g)))

(define (compose2 f g)
  (restrict-arity
   (lambda args (call-with-values (lambda () (apply g args)) f))
   (get-arity g)))

(define (compose . fs)
  (if (null? fs)
      values
      (let ((gs (reverse fs)))
        (fold compose2 (car gs) (cdr gs)))))


(define ((iterate n) f)
  (if (= n 0)
      identity
      (compose f ((iterate (- n 1)) f))))

(define (identity x) x)

(define (restrict-arity proc nargs)
  (hash-table-set! arity-table proc nargs)
  proc)

(define (get-arity proc)
  (or (hash-table-ref/default arity-table proc #f)
      (let ((a (procedure-arity proc))) ; arity not in the table
        (assert (eqv? (procedure-arity-min a)
                      (procedure-arity-max a)))
        (procedure-arity-min a))))

(define arity-table (make-key-weak-eqv-hash-table))

(define (gas-law-volume pressure temperature amount)
  (/ (* amount gas-constant temperature) pressure))

(define gas-constant 8.3144621) ;J/mol*K

(define (sphere-radius volume)
  (expt (/ volume (* 4/3 pi)) 1/3))

(define pi (* 4 (atan 1 1)))

;(define (make-unit-conversion fwd rev)
;  (let ((uc (cons fwd rev)))
;    (lambda (#!optional x)
;      (if (eq? x 'invert)
;          ((cdr uc))
;          ((car uc))))))

(define (make-unit-conversion fwd rev)
  (lambda (val)
    (if (eq? val 'invert) ; special case, return rev case
        (make-unit-conversion rev fwd)
        (fwd val))))

(define (unit:invert u)
  (u 'invert))

(define farenheit-to-celcius
  (make-unit-conversion (lambda (f) (* 5/9 (- f 32)))
                        (lambda (c) (+ (* c 9/5) 32))))

(define celcius-to-kelvin
  (let ((zero-celcius 273.15)) ;K
    (make-unit-conversion (lambda (c) (+ c zero-celcius))
                          (lambda (k) (- k zero-celcius)))))

(define farenheit-to-kelvin (compose celcius-to-kelvin farenheit-to-celcius))
