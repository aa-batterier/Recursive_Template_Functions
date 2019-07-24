; Single Test Tail Recursion.

; Template:
; (DEFUN func (X)
;   (COND (end-test end-value)
;         (T (func reduced-x))))

; SINGLE-TEST-TAIL-RECURSION-TEMPLATE returns a function based on the template above.
(defun single-test-tail-recursion-template (end-test end-value recfn)
  (labels ((self (lst)
             (if (funcall end-test lst)
               (funcall end-value lst)
               (funcall recfn #'(lambda ()
                                  (self (car lst)))))))
                                ; (self (cdr lst)))))))
    #'self))

; SINGLE-TEST creates an easier interface for DOUBLE-TEST-TAIL-RECURSION-TEMPLATE.
(defmacro single-test (end-test end-value recfn)
  `(single-test-tail-recursion-template
     #'(lambda (it) ,end-test)
     #'(lambda (it) ,end-value)
     ,recfn))

; Example:
; (setq find-first-atom (single-test (atom it) it #'(lambda (fun) (funcall fun))))
; (funcall find-first-atom '(((1)) (2) 3))
; => 1
; Instead of:
; (defun find-first-atom (x)
;   (cond ((atom x) x)
;         (t (find-first-atom (first x)))))
