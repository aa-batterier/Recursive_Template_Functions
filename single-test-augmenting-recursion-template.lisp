; Single Test Augmenting Recursion.

; Template:
; (DEFUN func (X)
;   (COND (end-test end-value)
;         (T (aug-fun aug-val
;                     (func reduced-x)))))

; SINGLE-TEST-AUGMENTING-RECURSION-TEMPLATE returns a function based on the template above.
(defun single-test-augmenting-recursion-template (end-test end-value recfn)
  (labels ((self (lst)
             (if (funcall end-test lst)
               (funcall end-value lst)
               (funcall recfn (car lst)
                              #'(lambda ()
                                  (self (cdr lst)))))))
    #'self))

; SINGLE-TEST creates an easier interface for SINGLE-TEST-AUGMENTING-RECURSION-TEMPLATE.
(defmacro single-test (end-test end-value recfn)
  `(single-test-augmenting-recursion-template
     #'(lambda (it) ,end-test)
     #'(lambda (it) ,end-value)
     ,recfn))

; Example:
; (setq count-slices (single-test (null it) 0 #'(lambda (element fun) (+ 1 (funcall fun)))))
; (funcall count-slices '(1 2 3 4 5))
; => 5
; Instead of:
; (defun count-slices (x)
;   (cond ((null x) 0)
;         (t (+ 1 (count-slices (rest x))))))
