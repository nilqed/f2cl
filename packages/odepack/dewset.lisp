;;; Compiled by f2cl version:
;;; ("" "" "" "" "" "" "")

;;; Using Lisp SBCL 1.4.2
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing t) (:declare-common nil)
;;;           (:float-format single-float))

(in-package "ODEPACK")


(defun dewset (n itol rtol atol ycur ewt)
  (declare (type (array double-float (*)) ewt ycur atol rtol)
           (type (f2cl-lib:integer4) itol n))
  (f2cl-lib:with-multi-array-data
      ((rtol double-float rtol-%data% rtol-%offset%)
       (atol double-float atol-%data% atol-%offset%)
       (ycur double-float ycur-%data% ycur-%offset%)
       (ewt double-float ewt-%data% ewt-%offset%))
    (prog ((i 0))
      (declare (type (f2cl-lib:integer4) i))
      (f2cl-lib:computed-goto (label10 label20 label30 label40) itol)
     label10
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i n) nil)
        (tagbody
         label15
          (setf (f2cl-lib:fref ewt-%data% (i) ((1 n)) ewt-%offset%)
                  (+
                   (* (f2cl-lib:fref rtol-%data% (1) ((1 *)) rtol-%offset%)
                      (abs
                       (f2cl-lib:fref ycur-%data% (i) ((1 n)) ycur-%offset%)))
                   (f2cl-lib:fref atol-%data% (1) ((1 *)) atol-%offset%)))))
      (go end_label)
     label20
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i n) nil)
        (tagbody
         label25
          (setf (f2cl-lib:fref ewt-%data% (i) ((1 n)) ewt-%offset%)
                  (+
                   (* (f2cl-lib:fref rtol-%data% (1) ((1 *)) rtol-%offset%)
                      (abs
                       (f2cl-lib:fref ycur-%data% (i) ((1 n)) ycur-%offset%)))
                   (f2cl-lib:fref atol-%data% (i) ((1 *)) atol-%offset%)))))
      (go end_label)
     label30
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i n) nil)
        (tagbody
         label35
          (setf (f2cl-lib:fref ewt-%data% (i) ((1 n)) ewt-%offset%)
                  (+
                   (* (f2cl-lib:fref rtol-%data% (i) ((1 *)) rtol-%offset%)
                      (abs
                       (f2cl-lib:fref ycur-%data% (i) ((1 n)) ycur-%offset%)))
                   (f2cl-lib:fref atol-%data% (1) ((1 *)) atol-%offset%)))))
      (go end_label)
     label40
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i n) nil)
        (tagbody
         label45
          (setf (f2cl-lib:fref ewt-%data% (i) ((1 n)) ewt-%offset%)
                  (+
                   (* (f2cl-lib:fref rtol-%data% (i) ((1 *)) rtol-%offset%)
                      (abs
                       (f2cl-lib:fref ycur-%data% (i) ((1 n)) ycur-%offset%)))
                   (f2cl-lib:fref atol-%data% (i) ((1 *)) atol-%offset%)))))
      (go end_label)
     end_label
      (return (values nil nil nil nil nil nil)))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dewset
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo :arg-types
                                            '((fortran-to-lisp::integer4)
                                              (fortran-to-lisp::integer4)
                                              (array double-float (*))
                                              (array double-float (*))
                                              (array double-float (*))
                                              (array double-float (*)))
                                            :return-values
                                            '(nil nil nil nil nil nil) :calls
                                            'nil)))

