#lang racket

(provide p%q)
(provide sturm-chain)
(provide count-roots)
(provide find-roots)

; All polynomes are represented by the list of size (d+1) of their coefficients
; in increasing order of exponent, where d is the highest exponent whose
; coefficient is non-zero.
; For instance, the polynme P(x) = 2 + 3x + x^3  is represented by (2 3 0 1).


; if p and q are respectively the representation of P(x) and Q(x),
; (p%q p q) returns the representation of R(x), the remainder of the division
; of P(x) by Q(x).
(define p%q
  (lambda(p q)
    (let* ([p (reverse p)] [q (reverse q)])
      (reverse (p%q_aux p q )))))

; if p and q are respectively the representation of P(x) and Q(x)
;with decreasing powers from left to right,
; (p%q p q) returns the representation of R(x) ,the remainder of
;the division of P(x) by Q(x) with decreasing powers from left to right
; .
(define p%q_aux
  (lambda(p q)
    (cond ((eq? (length p) 1) p)
          ((zero? (car p)) (p%q_aux (cdr p) q))
          ((< (length p) (length q)) p)
          (else(let* ([subls (sublist p q)]
                      [next (cdr (substract_lists p subls))]) ;Get the next iteration
                 (p%q_aux next q)))))) 


;Substracts the list p by the list subls,
;element by element. If either one of the lists
;is longer than the other, the rest is appended to
;the difference

(define substract_lists
  (lambda (p subls)
    (cond ((null? subls) p)
          ((null? p) subls)
          (else (cons (- (car p) (car subls))
                      (substract_lists (cdr p) (cdr subls)))))))



;If p1 and q1 are the first coefficient of the lists p and q
;this method returns the list where each element of q is
;multiplied by (p1/q1)
(define sublist
  (lambda (p q)
    (let ((div  (/ (car p) (car q)) ))
      (map (lambda (x) (* div x)) q))))

; if p is a polynome, (sturm-chain p) returns the Sturm chain of p by decreasing
; order of powers.
(define sturm-chain
  (lambda (p)
    (let ([base (cons p (list (cdr (deriv p))))])
      (sturm-chain-acc base))))
    
(define sturm-chain-acc
  (lambda (acc)
    (let* ([n (length acc)]
           [i-1 (list-ref acc (- n 1))]
           [i-2 (list-ref acc (- n 2))]
           [i  (map - (p%q i-2 i-1))])
      (if (<= (length i) 1) (append acc (list i))
          (sturm-chain-acc (append acc (list i)))))))
      
        
                          
                             

;If p is a polynome in increasing order of powers from left to right,
;(deriv p) gives the derivative of p |#
(define deriv
  (lambda (p)
    (define (deriv-aux n p)              
      (if (null? p) '()
          (cons (* n (car p))
                (deriv-aux (+ n 1) (cdr p)))))
    (deriv-aux 0 p)))

; Evaluates the sturm-chain sturm at a given value x and returns a list
; containing the result of each polynome
(define eval-sturm
  (lambda (x sturm)
    (if (null? sturm) '()
        (cons (eval-poly x (car sturm)) (eval-sturm x (cdr sturm))))
    ))

; Evaluates the polynome p,with increasing powers from left to right
; using the value x
(define eval-poly
  (lambda (x p)
    (define (eval-poly-aux x p n)
      (cond ((null? p) 0)
            ((or (= x +inf.0) (= x -inf.0)) (* (car (reverse p)) (expt x (- (length p) 1))))
            (else (+ (* (car p) (expt x n))
                     (eval-poly-aux x (cdr p) (+ n 1))))))
    (eval-poly-aux x p 0)))
                             

;If p is a list of real numbers,
;(signs p) returns the number of sign changes
;from one number to the other, left to right
;0 is considered positive
(define signs
  (lambda (p)
    (if (null? (cdr p)) 0
        (if (or (and (>= (car p) 0)
                     (< (cadr p) 0))
                (and (< (car p) 0)
                     (>= (cadr p) 0))
                )
            (+ 1 (signs (cdr p)))
            (+ 0 (signs (cdr p)))))))
           
                     
               
          
               

; if p is a polynome and both a and b are numbers such that a < b,
; (count-roots p a b) returns the number of roots of p
; on ]a b]
(define count-roots
  (lambda (p a b)
    (let* ([sturm (sturm-chain p)]
           [sign_a (signs (eval-sturm a sturm))]
           [sign_b (signs (eval-sturm b sturm))])
      (- sign_a sign_b)
      )))
                      
 

; if p is a polynome, both a and b are numbers (such that a < b) and eps
; is a positive real, (find-roots p a b eps) returns the ordered list
; of roots of p on the ]a, b] interval with precision eps
(define find-roots
  (lambda(p a b eps)
    (let ([nbroots (count-roots p a b)])
      (cond ((zero? nbroots) '())
            ((eq? nbroots 1) (bisection p a b eps))
            ((<= (abs (- b a)) eps) (list a))
            (else (let ([m (/ (+ a b) 2)])
                    (append (find-roots p a m eps) (find-roots p m b eps))))))))




; If p is a polynome, a and b are real numbers such that a < b and eps
; is a positive real, (bisection p a b eps) returns the root
; of p on the interval ]a, b]  with precision eps
(define bisection
  (lambda (p a b eps)
    (let ([m (/ (+ a b) 2)] [eval-a (eval-poly a p)])
      (cond ((<= (abs (- b a)) eps) (list a))
            ((positive? (* eval-a (eval-poly b p))) '())
            ((<= (* eval-a (eval-poly m p)) 0) (bisection p a m eps))
            (else (bisection p m b eps))))))
