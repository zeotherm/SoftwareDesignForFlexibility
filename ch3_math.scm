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
