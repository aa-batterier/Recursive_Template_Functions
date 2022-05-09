; Multiple Recursion.¨

; Template:
; (DEFUN func (N)
;   (COND (end-test-1 end-value-1)
;         (end-test-2 end-value-2)
;         (T (combiner (func first-reduced-n)
;                      (func second-reduced-n)))))

; MULTIPLE-RECURSION-TEMPLATE creates functions which does recursions on multiple varibles.
(defun multiple-recursion-template (end-test-1 end-value-1 end-test-2 end-value-2 combiner first-reduced-n second-reduced-n)
  (labels ((self (element)
             (cond ((funcall end-test-1 element) (funcall end-value-1 element))
                   ((funcall end-test-2 element) (funcall end-value-2 element))
                   (t (funcall combiner #'(lambda ()
                                            (self (funcall first-reduced-n element)))
                                        #'(lambda ()
                                            (self (funcall second-reduced-n element))))))))
    #'self))

; MULTIPLE-RECURSION is an easier interface for MULTIPLE-RECURSION-TEMPLATE.
(defmacro multiple-recursion (end-test-1 end-value-1 end-test-2 end-value-2 combiner first-reduced-n second-reduced-n)
  `(multiple-recursion-template
     #'(lambda (it) ,end-test-1)
     #'(lambda (it) ,end-value-1)
     #'(lambda (it) ,end-test-2)
     #'(lambda (it) ,end-value-2)
     ,combiner
     #'(lambda (it) ,first-reduced-n)
     #'(lambda (it) ,second-reduced-n)))

; Example:
; (setq fib (multiple-recursion (equal it 0) 1 (equal it 1) 1 #'(lambda (first-reduced second-reduced)
;                                                                 (+ (funcall first-reduced)
;                                                                    (funcall second-reduced))) (1- it) (- it 2)))
; (funcall fib 7)
; => 21
; Instead of:
; (defun fib (n)
;   (cond ((equal n 0) 1)
;         ((equal n 1) 1)
;         (t (+ (fib (- n 1))
;               (fib (- n 2))))))
