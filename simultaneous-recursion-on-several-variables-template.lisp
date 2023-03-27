; Simultaneous Recursion on Several Variables (Using the Single Test Tail Recursion Template).

; Template:
; (DEFUN func (N X)
;   (COND (end-test end-value)
;         (T (func reduced-n reduced-x))))

; SIMULTANEOUS-RECURSION-ON-SEVERAL-VARIABLES-TEMPLATE does a single test tail recursion on several variables.
(defun simultaneous-recursion-on-several-variables-template (end-test end-value reduced-n reduced-x)
  (labels ((self (n x)
             (if (funcall end-test n x)
               (funcall end-value n x)
               (self (funcall reduced-n n)
                     (funcall reduced-x x)))))
    #'self))

; SEVERAL-VARIABLES is an easier interface for SIMULTANEOUS-RECURSION-ON-SEVERAL-VARIABLES-TEMPLATE.
(defmacro several-variables (end-test end-value reduced-n reduced-x)
  `(simultaneous-recursion-on-several-variables-template
     #'(lambda (it1 it2) ,end-test)
     #'(lambda (it1 it2) ,end-value)
     #'(lambda (it) ,reduced-n)
     #'(lambda (it) ,reduced-x)))

; Example:
; (setq my-nth (several-variables (zerop it1) (first it2) (1- it) (rest it)))
; (funcall my-nth 3 '(1 2 3 4 5 6))
; => 4
; Instead of:
; (defun my-nth (n x)
;   (cond ((zerop n) (first x))
;         (t (my-nth (- n 1) (rest x)))))
