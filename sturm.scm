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


(define eval (lambda (x p)
               (if (null? p) '()
                   (cons (eval-aux x (car p) 0) (eval x (cdr p))))
               ))

(define eval-aux (lambda (x p n)
                   (if (null? p) 0
                       (+ (* (car p) (expt x n))
                             (eval-aux x (cdr p) (+ n 1))
                             ))))
               
          
               

; if p is a polynome and both a and b are numbers such that a < b,
; (count-roots p a b) returns the number of roots of p
; on ]a b]
(define count-roots
  (lambda (p a b) "TODO"))
 

; if p is a polynome, both a and b are numbers (such that a < b) and eps
; is a positive real, (find-roots p a b eps) returns the ordered list
; of roots of p on the ]a, b] interval with precision eps
(define find-roots
  (lambda (p a b eps) "POSSIBLY TODO"))



(define pol '(1 2 3))
'pol pol
(define sturm (sturm-chain pol))
'sturm sturm
'eval (eval 5 sturm)
