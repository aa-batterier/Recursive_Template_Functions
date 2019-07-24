; Double-Test Augmenting Recursion.

; Template:
; (DEFUN func (x)
;   (COND (end-test-1 end-value-1)
;         (end-test-2 end-value-2)
;         (T (aug-fun aug-val
;                     (func reduced-x))))

; DOUBLE-TEST-AUGMENTING-RECURSION-TEMPLATE returns a function based on the template above.
(defun double-test-augmenting-recursion-template (end-test-1 end-value-1 end-test-2 end-value-2 recfn)
  (labels ((self (lst)
             (cond ((funcall end-test-1 lst) (funcall end-value-1 lst))
                   ((funcall end-test-2 lst) (funcall end-value-2 lst))
                   (t (funcall recfn (car lst)
                                   #'(lambda ()
                                       (self (cdr lst))))))))
    #'self))

; DOUBLE-TEST creates an easier interface for DOUBLE-TEST-AUGMENTING-RECURSION-TEMPLATE.
(defmacro double-test (end-test-1 end-value-1 end-test-2 end-value-2 recfn)
  `(double-test-augmenting-recursion-template
     #'(lambda (it) ,end-test-1)
     #'(lambda (it) ,end-value-1)
     #'(lambda (it) ,end-test-2)
     #'(lambda (it) ,end-value-2)
     ,recfn))

; Example:
; (setq anyoddp (double-test (null it) nil (oddp (first it)) t #'(lambda (element fun) (funcall fun))))
; (funcall anyoddp '(1 2 3 4))
; => T
; Instead of:
; (defun anyoddp (x)
;   (cond ((null x) nil)
;         ((oddp (first x)) t)
;         (t (anyoddp (rest x)))))
