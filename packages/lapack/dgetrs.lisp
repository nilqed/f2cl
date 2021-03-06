;;; Compiled by f2cl version:
;;; ("" "" "" "" "" "" "")

;;; Using Lisp SBCL 1.4.2
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing t) (:declare-common nil)
;;;           (:float-format single-float))

(in-package "LAPACK")


(let* ((one 1.0d0))
  (declare (type (double-float 1.0d0 1.0d0) one)
           (ignorable one))
  (defun dgetrs (trans n nrhs a lda ipiv b ldb$ info)
    (declare (type (array f2cl-lib:integer4 (*)) ipiv)
             (type (array double-float (*)) b a)
             (type (f2cl-lib:integer4) info ldb$ lda nrhs n)
             (type (string *) trans))
    (f2cl-lib:with-multi-array-data
        ((trans character trans-%data% trans-%offset%)
         (a double-float a-%data% a-%offset%)
         (b double-float b-%data% b-%offset%)
         (ipiv f2cl-lib:integer4 ipiv-%data% ipiv-%offset%))
      (prog ((notran nil))
        (declare (type f2cl-lib:logical notran))
        (setf info 0)
        (setf notran (lsame trans "N"))
        (cond
         ((and (not notran) (not (lsame trans "T")) (not (lsame trans "C")))
          (setf info -1))
         ((< n 0) (setf info -2)) ((< nrhs 0) (setf info -3))
         ((< lda (max (the f2cl-lib:integer4 1) (the f2cl-lib:integer4 n)))
          (setf info -5))
         ((< ldb$ (max (the f2cl-lib:integer4 1) (the f2cl-lib:integer4 n)))
          (setf info -8)))
        (cond
         ((/= info 0) (xerbla "DGETRS" (f2cl-lib:int-sub info))
          (go end_label)))
        (if (or (= n 0) (= nrhs 0))
            (go end_label))
        (cond
         (notran (dlaswp nrhs b ldb$ 1 n ipiv 1)
          (dtrsm "Left" "Lower" "No transpose" "Unit" n nrhs one a lda b ldb$)
          (dtrsm "Left" "Upper" "No transpose" "Non-unit" n nrhs one a lda b
           ldb$))
         (t
          (dtrsm "Left" "Upper" "Transpose" "Non-unit" n nrhs one a lda b ldb$)
          (dtrsm "Left" "Lower" "Transpose" "Unit" n nrhs one a lda b ldb$)
          (dlaswp nrhs b ldb$ 1 n ipiv -1)))
        (go end_label)
       end_label
        (return (values nil nil nil nil nil nil nil nil info))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dgetrs
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo :arg-types
                                            '((string)
                                              (fortran-to-lisp::integer4)
                                              (fortran-to-lisp::integer4)
                                              (array double-float (*))
                                              (fortran-to-lisp::integer4)
                                              (array fortran-to-lisp::integer4
                                               (*))
                                              (array double-float (*))
                                              (fortran-to-lisp::integer4)
                                              (fortran-to-lisp::integer4))
                                            :return-values
                                            '(nil nil nil nil nil nil nil nil
                                              fortran-to-lisp::info)
                                            :calls
                                            '(fortran-to-lisp::dtrsm
                                              fortran-to-lisp::dlaswp
                                              fortran-to-lisp::xerbla
                                              fortran-to-lisp::lsame))))

