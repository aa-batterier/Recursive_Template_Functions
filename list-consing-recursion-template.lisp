; List-Consing Recursion (A Special Case of Augmenting Recursion).

; Template:
; (DEFUN func (N)
;   (COND (end-test NIL)
;         (T (CONS new-element
;                  (func reduce-n)))))

; LIST-CONSING-RECURSION-TEMPLATE creates functions which cons together
; new-element n number of times until an end-test is fulfilled.
(defun list-consing-recursion-template (end-test new-element recfn)
  (labels ((self (n)
             (if (not (funcall end-test n))
               (cons (funcall new-element n) ; new-element can be both a function and a variable.
                     (self (funcall recfn n))))))
    #'self))

; LIST-CONSING creates an easier interface for LIST-CONSING-RECURSION-TEMPLATE.
(defmacro list-consing (end-test new-element recfn)
  `(list-consing-recursion-template
     #'(lambda (it) ,end-test)
     #'(lambda (it) ,new-element)
     #'(lambda (it) ,recfn)))

; Example:
; (setq laugh (list-consing (zerop it) 'ha (1- it)))
; (funcall laugh 7)
; => (HA HA HA HA HA HA HA)
; Instead of:
; (defun laugh (n)
;   (cond ((zerop n) nil)
;         (t (cons 'ha (laugh (- n 1))))))
