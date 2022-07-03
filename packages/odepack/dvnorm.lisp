;;; Compiled by f2cl version:
;;; ("" "" "" "" "" "" "")

;;; Using Lisp SBCL 1.4.2
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing t) (:declare-common nil)
;;;           (:float-format single-float))

(in-package "ODEPACK")


(defun dvnorm (n v w)
  (declare (type (array double-float (*)) w v)
           (type (f2cl-lib:integer4) n))
  (f2cl-lib:with-multi-array-data
      ((v double-float v-%data% v-%offset%)
       (w double-float w-%data% w-%offset%))
    (prog ((sum 0.0d0) (i 0) (dvnorm 0.0d0))
      (declare (type (f2cl-lib:integer4) i)
               (type (double-float) dvnorm sum))
      (setf sum 0.0d0)
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i n) nil)
        (tagbody
         label10
          (setf sum
                  (+ sum
                     (expt
                      (* (f2cl-lib:fref v-%data% (i) ((1 n)) v-%offset%)
                         (f2cl-lib:fref w-%data% (i) ((1 n)) w-%offset%))
                      2)))))
      (setf dvnorm (f2cl-lib:fsqrt (/ sum n)))
      (go end_label)
     end_label
      (return (values dvnorm nil nil nil)))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dvnorm
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo :arg-types
                                            '((fortran-to-lisp::integer4)
                                              (array double-float (*))
                                              (array double-float (*)))
                                            :return-values '(nil nil nil)
                                            :calls 'nil)))

