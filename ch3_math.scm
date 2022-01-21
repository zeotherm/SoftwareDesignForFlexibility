;;(load "./sdf/manager/load")

;;(manage 'new-environment 'combining-arithmetics)

(define (stormer-2 F h)
  (lambda (history)
    (+ (* 2 (x 0 history))
       (* -1 (x 1 history))
       (* (/ (expt h 2) 12)
          (+ (* 13 (F (t 0 history) (x 0 history)))
             (* -2 (F (t 1 history) (x 1 history)))
             (F (t 2 history) (x 2 history)))))))

(define (stepper h integrator)
  (lambda (history)
    (extend-history (+ (t 0 history) h)
                    (integrator history)
                    history)))

(define (make-initial-history t h xt xt-h xt-2h)
  (list (cons t xt)
        (cons (- t h) xt-h)
        (cons (- t (* 2 h)) xt-2h)))

(define (extend-history t+h xt+h history)
  (cons (cons t+h xt+h) history))

(define (t index history)
  (car (list-ref history index)))

(define (x index history)
  (cdr (list-ref history index)))

;;(define (n:> l r)
;; (> l r))

;;(define (n:- l r)
;;  (- l r))

(define (evolver F h make-integrator)
  (let ((integrator (make-integrator F h)))
    (let ((step (stepper h integrator)))
      (define (evolve history n-steps)
        (if (n:> n-steps 0)
            (evolve (step history) (n:- n-steps 1))
            history))
      evolve)))

(define (F t x) (- x)) ;; d^2(x(t))/dt^2 = -x(t)

(define numeric-s0
  (make-initial-history 0 0.1 (sin 0) (sin -0.01) (sin -0.02)))


(define symbolic-arithmetic-1
  (make-arithmetic-1 'symbolic
		     (lambda (operator)
                       (lambda args (cons operator args)))))

;;(install-arithmetic! symbolic-arithmetic-1)

;;(pp (x 0 ((evolver F 'h stormer-2) (make-initial-history 't 'h 'xt 'xt-h 'xt-2h) 1)))

(define (make-operation operator applicability procedure)
  (list 'operation operator applicability procedure))

(define (operation-applicability operation)
  (caddr operation))

(define (simple-operation operator predicate procedure)
  (make-operation operator
                  (all-args (operator-arity operator)
                            predicate)
                  procedure))

(define numeric-arithmetic
  (make-arithmetic 'numeric number? '()
                   (lambda (name)
                     (case name
                       ((additive-identity) 0)
                       ((multiplicative-identity) 1)
                       (else (default-object))))
                   (lambda (operator)
                     (simple-operation operator number?
                                       (get-implementation-value
                                        (operator->procedure-name operator))))))

(define (symbolic-extender base-arithmetic)
  (make-arithmetic 'symbolic symbolic? (list base-arithmetic)
                   (lambda (name base-constant) ; constant generator
                     base-constant)
                   (let ((base-predicate
                          (arithmetic-domain-predicate base-arithmetic)))
                     (lambda (operator base-operation) ; operator generator
                       (make-operation operator
                                       (any-arg (operator-arity operator)
                                                symbolic?
                                                base-predicate)
                                       (lambda args
                                         (cons operator args)))))))

(define (add-arithmetics . arithmetics)
  (add-arithmetics* arithmetics))

(define (add-arithmetics* arithmetics)
  (if (n:null? (cdr arithmetics))
      (car arithmetics) ; only one arithmetic to handle
      (make-arithmetic 'add
                       (disjoin*
                        (map arithmetic-domain-predicate
                             arithmetics))
                       arithmetics
                       constant-union
                       operation-union)))

(define (constant-union name . constants)
  (let ((unique
         (remove default-object?
                 (delete-duplicates constants eqv?))))
    (if (n:pair? unique)
        (car unique)
        (default-object))))

(define (operation-union operator . operations)
  (operation-union* operator operations))

(define (operation-union* operator operations)
  (make-operation operator
                  (applicability-union*
                   (map operation-applicability operations))
                  (lambda args
                    (operation-union-dispatch operator
                                              operations
                                              args))))

(define (operation-union-dispatch operator operations args)
  (let ((operation
         (find (lambda (operation)
                 (is-operation-applicable? operation args))
               operations)))
    (if (not operation)
        (error "Inapplicable operation:" operator args))
    (apply-operation operation args)))

(define (extend-arithmetic extender base-arithmetic)
  (add-arithmetics base-arithmetic
                   (extender base-arithmetic)))

(define combined-arithmetic
  (extend-arithmetic symbolic-extender numeric-arithmetic))

; (install-arithmetic! combined-arithmetic)
