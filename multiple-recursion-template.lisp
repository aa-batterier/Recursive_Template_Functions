; Multiple Recursion.¨

; Template:
; (DEFUN func (N)
;   (COND (end-test-1 end-value-1)
;         (end-test-2 end-value-2)
;         (T (combiner (func first-reduced-n)
;                      (func second-reduced-n)))))

; MULTIPLE-RECURSION-TEMPLATE creates functions which does recursions on multiple varibles.
(defun multiple-recursion-template (end-test-1 end-value-1 end-test-2 end-value-2 combiner)
  (labels ((self (element)
             (cond ((funcall end-test-1 element) (funcall end-test-1 element))
                   ((funcall end-test-2 element) (funcall end-test-2 element))
                   (t (funcall combiner #'(lambda ()
                                            (self (funcall first-reduced-n)))
                                        #'(lambda ()
                                            (self (funcall second-reduced-n))))))))
    #'self))
