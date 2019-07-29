; CAR/CDR Recursion (A Special Case of Multiple Recursion).

; Template:
; (DEFUN func (X)
;   (COND (end-test-1 end-value-1)
;         (end-test-2 end-value-2)
;         (T (combiner (func (CAR X))
;                      (func (CDR X))))))

; CAR-CDR-RECURSION-TEMPLATE creates functions which does
; recursion down both CAR of the list and CDR of the list.
; Perfekt for tree recursion.
(defun car-cdr-recursion-template (end-test-1 end-value-1 end-test-2 end-value-2 combiner)
  (labels ((self (tree)
             (cond ((funcall end-test-1 tree) (funcall end-value-1 tree))
                   ((funcall end-test-2 tree) (funcall end-value-2 tree))
                   (t (funcall combiner tree
                                        #'(lambda ()
                                            (self (car tree)))
                                        #'(lambda ()
                                            (self (cdr tree))))))))
    #'self))

; TREE-REC is an easier interface for CAR-CDR-RECURSION-TEMPLATE.
(defmacro tree-rec (end-test-1 end-value-1 end-test-2 end-value-2 combiner)
  (let ((left (gensym))
        (right (gensym)))
    `(car-cdr-recursion-template
       #'(lambda (it) ,end-test-1)
       #'(lambda (it) ,end-value-1)
       #'(lambda (it) ,end-test-2)
       #'(lambda (it) ,end-value-2)
       #'(lambda (it ,left ,right) (,@combiner (funcall ,left)
                                               (funcall ,right))))))

; Example:
; (setq find-number (tree-rec (numberp it) it (atom it) nil (or)))
; (funcall find-number '(((a b) c) (d e) f (1)))
; => 1
; Instead of:
; (defun find-number (x)
;   (cond ((numberp x) x)
;         ((atom x) nil)
;         (t (or (find-number (car x))
;                (find-number (cdr x))))))
