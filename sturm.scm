#lang racket

(provide p%q)
(provide sturm-chain)
(provide count-roots)
(provide find-roots)

; All polynomes are represented by the list of size (d+1) of their coefficients
; in increasing order of exponent, where d is the highest exponent whose
; coefficient is non-zero.
; For instance, the polynme P(x) = 2 + 3x + x^3  is represented by (2 3 0 1).


; if p and q are respectively the repsentation of P(x) and Q(x),
; (p%q p q) returns the representation of R(x), the remainer of the division
; of P(x) by Q(x).
(define p%q (lambda(p q)
              (if (< (length p) (length q)) p 
                  (let ([p (reverse p)] [q (reverse q)])
                    (let ([subls (sublist p q)]) ;Get the sublist
                      (p%q_aux (substract_lists p subls) q )))))) ;Substract each coefficient of p by those of subls


(define p%q_aux (lambda(p q)
              (if (< (length (cdr p)) (length q)) (reverse (cdr p))
                  (let ([subls (sublist p q)]) ;Get the sublist
                    (p%q_aux (substract_lists (cdr p) subls) q ))))) ;Substract each coefficient of p by those of subls


#| Multiplies each element of the list ls by [[n]]|#
(define (multiply_list ls n)
  (map (lambda (x) (* n x)) ls))

#| Substracts the list p by the list subls, element by element. If the list subls is too
short, the remainder is appended to to the result
|#
(define substract_lists (lambda (p subls)
                          (if (null? subls) p
                              (cons (- (car p) (car subls))
                                    (substract_lists (cdr p) (cdr subls))))))


#|
If p1 and q1 are the first coefficient of the lists,
this method returns the list where each element of ls is multiplied by (p1/q1)
|#
(define sublist (lambda (p q)
                  (if (zero? (car p)) (sublist (cdr p) q) ;Can't divide by 0
                  (let ((div  (/ (car p) (car q)) ))
                    (multiply_list q div)))))

; if p is a polynome, (sturm-chain p) returns the Sturm chain of p by decreasing
; order of powers.
(define sturm-chain (lambda (p)
                      (cons p (cons (cdr (deriv 0 p)) (sturm-chain-aux p)))))

(define sturm-chain-aux (lambda (p)
                        (let ([dp (cdr (deriv 0 p))])
                          (let ([p_i (p%q p dp)])
                            (if (or (null? p_i)
                                    (zero? (car p_i)))
                                '()
                                (cons p_i (sturm-chain-aux dp)))))))
                          
                             

#| If [[n]] = 0 and p a polynome, (deriv n p) gives the derivative of p |#
(define deriv (lambda (n p)
                (if (null? p) '()
                      (cons (* n (car p))
                            (deriv (+ n 1) (cdr p))))))

; Evaluates the sturm-chain sturm at a given value x and returns a list
; containing the result of each polynome
(define eval-sturm (lambda (x sturm)
               (if (null? sturm) '()
                   (cons (eval-poly x (car sturm) 0) (eval-sturm x (cdr sturm))))
               ))

; Evaluates the polynome p using the value x
(define eval-poly (lambda (x p n)
                   (if (null? p) 0
                       (+ (* (car p) (expt x n))
                             (eval-poly x (cdr p) (+ n 1))
                             ))))

;If p is a list of numbers, this function returns the number of sign changes from one number to the other
(define signs (lambda (p)
                (if (null? (cdr p)) 0
                (if (or (and (> (car p) 0)
                             (< (cadr p) 0))
                        (and (< (car p) 0)
                             (> (cadr p) 0))
                        )
                    (+ 1 (signs (cdr p)))
                    (+ 0 (signs (cdr p)))))))
                    
                        
                        
                    
                             
                        
                     
               
          
               

; if p is a polynome and both a and b are numbers such that a < b,
; (count-roots p a b) returns the number of roots of p
; on ]a b]
(define count-roots (lambda (p a b)
                      (let ([sturm (sturm-chain p)])
                        (let ([sign_a (signs (eval-sturm a sturm))])
                          (let ([sign_b (signs (eval-sturm b sturm))])
                            (- sign_a sign_b)
                         )))))
                      
 

; if p is a polynome, both a and b are numbers (such that a < b) and eps
; is a positive real, (find-roots p a b eps) returns the ordered list
; of roots of p on the ]a, b] interval with precision eps
(define find-roots (lambda (p a b eps)
                     (if (<= (- b a) eps) a
                         (let ([m (/ (+ a b) 2)])
                            (if (<= (* (eval-poly a p 0) (eval-poly m p 0)) 0)
                               (find-roots p a m eps)
                               (find-roots p m b eps))))))
                          
                     
                        
                            
                        
                      
                      


#|
(define pol '(1 2 3))
'pol pol
(define sturm (sturm-chain pol))
'sturm sturm
(define eval (eval-sturm 5 sturm))
'eval eval
(count-roots eval -5 5)
|#

(find-roots '(1 2 1) -2 2 0.0001)