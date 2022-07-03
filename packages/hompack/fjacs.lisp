;;; Compiled by f2cl version:
;;; ("" "" "" "" "" "" "")

;;; Using Lisp SBCL 1.4.2
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing t) (:declare-common nil)
;;;           (:float-format single-float))

(in-package "HOMPACK")


(defun fjacs (x qr lenqr pivot)
  (declare (type (array f2cl-lib:integer4 (*)) pivot)
           (type (f2cl-lib:integer4) lenqr)
           (type (array double-float (*)) qr x))
  (f2cl-lib:with-multi-array-data
      ((x double-float x-%data% x-%offset%)
       (qr double-float qr-%data% qr-%offset%)
       (pivot f2cl-lib:integer4 pivot-%data% pivot-%offset%))
    (prog ((n 0))
      (declare (type (f2cl-lib:integer4) n))
      (go end_label)
     end_label
      (return (values nil nil nil nil)))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::fjacs fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo :arg-types
                                            '((array double-float (*))
                                              (array double-float (*))
                                              (fortran-to-lisp::integer4)
                                              (array fortran-to-lisp::integer4
                                               (*)))
                                            :return-values '(nil nil nil nil)
                                            :calls 'nil)))

