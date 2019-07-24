; Conditional Augmentation.

; Template:
; (DEFUN func (X)
;   (COND (end-test end-value)
;         (aug-test (aug-fun aug-val
;                            (func reduced-x)))
;         (T (func reduced-x))))

; CONDITIONAL-AUGMENTATION-TEMPLATE creates a functions which can recurse
; differently deppending on if the aug-fun condition is fullfiled.
(defun conditional-augmentation-template (end-test end-value aug-test aug-fun)
  (labels ((self (lst)
             (cond ((funcall end-test lst) (funcall end-value lst))
                   ((funcall aug-test (car lst))
                    (funcall aug-fun (car lst)
                                     #'(lambda ()
                                         (self (cdr lst)))))
                   (t (funcall #'(lambda ()
                                   (self (cdr lst))))))))
    #'self))

; CONDITIONAL-AUGMENTATION is an easier interface for CONDITIONAL-AUGMENTATION-TEMPLATE.
(defmacro conditional-augmentation (end-test end-value aug-test aug-fun)
  `(conditional-augmentation-template
     #'(lambda (it) ,end-test)
     #'(lambda (it) ,end-value)
     #'(lambda (it) ,aug-test)
     ,aug-fun))

; Example:
; (setq extract-symbols (conditional-augmentation (null it) nil (symbolp it) #'(lambda (element fun)
;                                                                                (cons element (funcall fun)))))
; (funcall extract-symbols '(1 2 3 nil a b c nil))
; => (NIL A B C NIL)
; Instead of:
; (defun extract-symbols (x)
;   (cond ((null x) nil)
;         ((symbolp (first x))
;          (cons (first x)
;                (extract-symbols (rest x))))
;         (t (extract-symbols (rest x)))))
