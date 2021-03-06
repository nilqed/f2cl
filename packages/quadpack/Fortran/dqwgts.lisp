;;; Compiled by f2cl version:
;;; ("" "" "" "" "" "" "")

;;; Using Lisp SBCL 1.4.2
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing t) (:declare-common nil)
;;;           (:float-format single-float))

(in-package "QUADPACK")


(defun dqwgts (x a b alfa beta integr)
  (declare (type (f2cl-lib:integer4) integr)
           (type (double-float) beta alfa b a x))
  (prog ((bmx 0.0d0) (xma 0.0d0) (dqwgts 0.0d0))
    (declare (type (double-float) dqwgts xma bmx))
    (setf xma (- x a))
    (setf bmx (- b x))
    (setf dqwgts (* (expt xma alfa) (expt bmx beta)))
    (f2cl-lib:computed-goto (label40 label10 label20 label30) integr)
   label10
    (setf dqwgts (* dqwgts (f2cl-lib:dlog xma)))
    (go label40)
   label20
    (setf dqwgts (* dqwgts (f2cl-lib:dlog bmx)))
    (go label40)
   label30
    (setf dqwgts (* dqwgts (f2cl-lib:dlog xma) (f2cl-lib:dlog bmx)))
   label40
    (go end_label)
   end_label
    (return (values dqwgts nil nil nil nil nil nil))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dqwgts
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo :arg-types
                                            '((double-float) (double-float)
                                              (double-float) (double-float)
                                              (double-float)
                                              (fortran-to-lisp::integer4))
                                            :return-values
                                            '(nil nil nil nil nil nil) :calls
                                            'nil)))

