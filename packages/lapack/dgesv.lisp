;;; Compiled by f2cl version:
;;; ("" "" "" "" "" "" "")

;;; Using Lisp SBCL 1.4.2
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing t) (:declare-common nil)
;;;           (:float-format single-float))

(in-package "LAPACK")


(defun dgesv (n nrhs a lda ipiv b ldb$ info)
  (declare (type (array f2cl-lib:integer4 (*)) ipiv)
           (type (array double-float (*)) b a)
           (type (f2cl-lib:integer4) info ldb$ lda nrhs n))
  (f2cl-lib:with-multi-array-data
      ((a double-float a-%data% a-%offset%)
       (b double-float b-%data% b-%offset%)
       (ipiv f2cl-lib:integer4 ipiv-%data% ipiv-%offset%))
    (prog ()
      (declare)
      (setf info 0)
      (cond ((< n 0) (setf info -1)) ((< nrhs 0) (setf info -2))
            ((< lda (max (the f2cl-lib:integer4 1) (the f2cl-lib:integer4 n)))
             (setf info -4))
            ((< ldb$ (max (the f2cl-lib:integer4 1) (the f2cl-lib:integer4 n)))
             (setf info -7)))
      (cond
       ((/= info 0) (xerbla "DGESV " (f2cl-lib:int-sub info)) (go end_label)))
      (multiple-value-bind (var-0 var-1 var-2 var-3 var-4 var-5)
          (dgetrf n n a lda ipiv info)
        (declare (ignore var-0 var-1 var-2 var-3 var-4))
        (setf info var-5))
      (cond
       ((= info 0)
        (multiple-value-bind
            (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8)
            (dgetrs "No transpose" n nrhs a lda ipiv b ldb$ info)
          (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7))
          (setf info var-8))))
      (go end_label)
     end_label
      (return (values nil nil nil nil nil nil nil info)))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dgesv fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo :arg-types
                                            '((fortran-to-lisp::integer4)
                                              (fortran-to-lisp::integer4)
                                              (array double-float (*))
                                              (fortran-to-lisp::integer4)
                                              (array fortran-to-lisp::integer4
                                               (*))
                                              (array double-float (*))
                                              (fortran-to-lisp::integer4)
                                              (fortran-to-lisp::integer4))
                                            :return-values
                                            '(nil nil nil nil nil nil nil
                                              fortran-to-lisp::info)
                                            :calls
                                            '(fortran-to-lisp::dgetrs
                                              fortran-to-lisp::dgetrf
                                              fortran-to-lisp::xerbla))))

