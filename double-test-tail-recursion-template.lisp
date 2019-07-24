; Double-Test Tail Recursion.

; Template:
; (DEFUN func (x)
;   (COND (end-test-1 end-value-1)
;         (end-test-2 end-value-2)
;         (T (func reduced-x))))

; DOUBLE-TEST-TAIL-RECURSION-TEMPLATE returns a function based on the template above.
(defun double-test-tail-recursion-template (end-test-1 end-value-1 end-test-2 end-value-2)
  (labels ((self (lst)
             (cond ((funcall end-test-1 lst) (funcall end-value-1 lst))
                   ((funcall end-test-2 lst) (funcall end-value-2 lst))
                 ; (t (self (car lst))))))
                   (t (self (cdr lst))))))
    #'self))

; DOUBLE-TEST creates an easier interface for DOUBLE-TEST-TAIL-RECURSION-TEMPLATE.
(defmacro double-test-tail (end-test-1 end-value-1 end-test-2 end-value-2)
  `(double-test-tail-recursion-template
     #'(lambda (it) ,end-test-1)
     #'(lambda (it) ,end-value-1)
     #'(lambda (it) ,end-test-2)
     #'(lambda (it) ,end-value-2)))

; Example:
; (setq anyoddp (double-test (null it) nil (oddp (first it)) t))
; (funcall anyoddp '(1 2 3 4))
; => T
; Instead of:
; (defun anyoddp (x)
;   (cond ((null x) nil)
;         ((oddp (first x)) t)
;         (t (anyoddp (rest x)))))
