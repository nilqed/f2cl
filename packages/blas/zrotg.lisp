;;; Compiled by f2cl version:
;;; ("" "" "" "" "" "" "")

;;; Using Lisp SBCL 1.4.2
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing t) (:declare-common nil)
;;;           (:float-format single-float))

(in-package "BLAS")


(defun zrotg (ca cb c s)
  (declare (type (double-float) c)
           (type (f2cl-lib:complex16) s cb ca))
  (prog ((alpha #C(0.0d0 0.0d0)) (norm 0.0d0) (scale 0.0d0))
    (declare (type (double-float) scale norm)
             (type (f2cl-lib:complex16) alpha))
    (if (/= (f2cl-lib:cdabs ca) 0.0d0)
        (go label10))
    (setf c 0.0d0)
    (setf s (f2cl-lib:cmplx 1.0d0 0.0d0))
    (setf ca cb)
    (go label20)
   label10
    (setf scale
            (coerce (+ (f2cl-lib:cdabs ca) (f2cl-lib:cdabs cb)) 'double-float))
    (setf norm
            (* scale
               (f2cl-lib:dsqrt
                (+
                 (expt (f2cl-lib:cdabs (/ ca (f2cl-lib:dcmplx scale 0.0d0))) 2)
                 (expt (f2cl-lib:cdabs (/ cb (f2cl-lib:dcmplx scale 0.0d0)))
                       2)))))
    (setf alpha (/ ca (f2cl-lib:cdabs ca)))
    (setf c (/ (f2cl-lib:cdabs ca) norm))
    (setf s (/ (* alpha (f2cl-lib:dconjg cb)) norm))
    (setf ca (* alpha norm))
   label20
    (go end_label)
   end_label
    (return (values ca nil c s))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::zrotg fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo :arg-types
                                            '((fortran-to-lisp::complex16)
                                              (fortran-to-lisp::complex16)
                                              (double-float)
                                              (fortran-to-lisp::complex16))
                                            :return-values
                                            '(fortran-to-lisp::ca nil
                                              fortran-to-lisp::c
                                              fortran-to-lisp::s)
                                            :calls 'nil)))

