#lang racket
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



(define p '(1 2 0 3 4))
'p p
(define q '(2 1 1))
'q q

'(p%q p q) (p%q p q)

                  
                    