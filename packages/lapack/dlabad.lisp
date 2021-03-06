;;; Compiled by f2cl version:
;;; ("" "" "" "" "" "" "")

;;; Using Lisp SBCL 1.4.2
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing t) (:declare-common nil)
;;;           (:float-format single-float))

(in-package "LAPACK")


(defun dlabad (small large)
  (declare (type (double-float) large small))
  (prog ()
    (declare)
    (cond
     ((> (f2cl-lib:log10 large) 2000.0d0) (setf small (f2cl-lib:fsqrt small))
      (setf large (f2cl-lib:fsqrt large))))
    (go end_label)
   end_label
    (return (values small large))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dlabad
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo :arg-types
                                            '((double-float) (double-float))
                                            :return-values
                                            '(fortran-to-lisp::small
                                              fortran-to-lisp::large)
                                            :calls 'nil)))

