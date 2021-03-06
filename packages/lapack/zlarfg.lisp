;;; Compiled by f2cl version:
;;; ("" "" "" "" "" "" "")

;;; Using Lisp SBCL 1.3.18
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing t) (:declare-common nil)
;;;           (:float-format single-float))

(in-package "LAPACK")


(let* ((one 1.0d0) (zero 0.0d0))
  (declare (type (double-float 1.0d0 1.0d0) one)
           (type (double-float 0.0d0 0.0d0) zero)
           (ignorable one zero))
  (defun zlarfg (n alpha x incx tau)
    (declare (type (array f2cl-lib:complex16 (*)) x)
             (type (f2cl-lib:complex16) tau alpha)
             (type (f2cl-lib:integer4) incx n))
    (f2cl-lib:with-multi-array-data
        ((x f2cl-lib:complex16 x-%data% x-%offset%))
      (prog ((alphi 0.0d0) (alphr 0.0d0) (beta 0.0d0) (rsafmn 0.0d0)
             (safmin 0.0d0) (xnorm 0.0d0) (j 0) (knt 0))
        (declare (type (double-float) alphi alphr beta rsafmn safmin xnorm)
                 (type (f2cl-lib:integer4) j knt))
        (cond
         ((<= n 0) (setf tau (coerce zero 'f2cl-lib:complex16))
          (go end_label)))
        (setf xnorm (dznrm2 (f2cl-lib:int-sub n 1) x incx))
        (setf alphr (f2cl-lib:dble alpha))
        (setf alphi (f2cl-lib:dimag alpha))
        (cond
         ((and (= xnorm zero) (= alphi zero))
          (setf tau (coerce zero 'f2cl-lib:complex16)))
         (t (setf beta (- (f2cl-lib:sign (dlapy3 alphr alphi xnorm) alphr)))
          (setf safmin (/ (dlamch "S") (dlamch "E")))
          (setf rsafmn (/ one safmin)) (setf knt 0)
          (cond
           ((< (abs beta) safmin)
            (tagbody
             label10
              (setf knt (f2cl-lib:int-add knt 1))
              (zdscal (f2cl-lib:int-sub n 1) rsafmn x incx)
              (setf beta (* beta rsafmn))
              (setf alphi (* alphi rsafmn))
              (setf alphr (* alphr rsafmn))
              (if (< (abs beta) safmin)
                  (go label10))
              (setf xnorm (dznrm2 (f2cl-lib:int-sub n 1) x incx))
              (setf alpha (f2cl-lib:dcmplx alphr alphi))
              (setf beta
                      (- (f2cl-lib:sign (dlapy3 alphr alphi xnorm) alphr))))))
          (setf tau
                  (f2cl-lib:dcmplx (/ (- beta alphr) beta) (/ (- alphi) beta)))
          (setf alpha (zladiv (f2cl-lib:dcmplx one) (- alpha beta)))
          (zscal (f2cl-lib:int-sub n 1) alpha x incx)
          (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                        ((> j knt) nil)
            (tagbody (setf beta (* beta safmin)) label20))
          (setf alpha (coerce beta 'f2cl-lib:complex16))))
        (go end_label)
       end_label
        (return (values nil alpha nil nil tau))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::zlarfg
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo :arg-types
                                            '((fortran-to-lisp::integer4)
                                              (fortran-to-lisp::complex16)
                                              (array fortran-to-lisp::complex16
                                               (*))
                                              (fortran-to-lisp::integer4)
                                              (fortran-to-lisp::complex16))
                                            :return-values
                                            '(nil fortran-to-lisp::alpha nil
                                              nil fortran-to-lisp::tau)
                                            :calls
                                            '(fortran-to-lisp::zscal
                                              fortran-to-lisp::zdscal
                                              fortran-to-lisp::dlamch
                                              fortran-to-lisp::dlapy3
                                              fortran-to-lisp::dznrm2))))

