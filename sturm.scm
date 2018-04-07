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
(define p%q
  (lambda (p q) "TODO"))


; if p is a polynome, (sturm-chain p) returns the Sturm chain of p by decreasing
; order of powers.
(define sturm-chain
  (lambda (p) "TODO"))


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


