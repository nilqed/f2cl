;;; Compiled by f2cl version:
;;; ("" "" "" "" "" "" "")

;;; Using Lisp SBCL 1.4.2
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing t) (:declare-common nil)
;;;           (:float-format single-float))

(in-package "HOMPACK")


(defun powp (nnnn xxxx yyyy)
  (declare (type (array double-float (*)) yyyy xxxx)
           (type (f2cl-lib:integer4) nnnn))
  (f2cl-lib:with-multi-array-data
      ((xxxx double-float xxxx-%data% xxxx-%offset%)
       (yyyy double-float yyyy-%data% yyyy-%offset%))
    (prog ((r 0.0d0) (rr 0.0d0) (t$ 0.0d0) (tt 0.0d0))
      (declare (type (double-float) tt t$ rr r))
      (cond
       ((= nnnn 0)
        (setf (f2cl-lib:fref yyyy-%data% (1) ((1 2)) yyyy-%offset%)
                (coerce 1.0 'double-float))
        (setf (f2cl-lib:fref yyyy-%data% (2) ((1 2)) yyyy-%offset%)
                (coerce 0.0 'double-float))
        (go end_label)))
      (cond
       ((= nnnn 1)
        (setf (f2cl-lib:fref yyyy-%data% (1) ((1 2)) yyyy-%offset%)
                (f2cl-lib:fref xxxx-%data% (1) ((1 2)) xxxx-%offset%))
        (setf (f2cl-lib:fref yyyy-%data% (2) ((1 2)) yyyy-%offset%)
                (f2cl-lib:fref xxxx-%data% (2) ((1 2)) xxxx-%offset%))
        (go end_label)))
      (setf r (dnrm2 2 xxxx 1))
      (cond
       ((= r 0.0)
        (setf (f2cl-lib:fref yyyy-%data% (1) ((1 2)) yyyy-%offset%)
                (coerce 0.0 'double-float))
        (setf (f2cl-lib:fref yyyy-%data% (2) ((1 2)) yyyy-%offset%)
                (coerce 0.0 'double-float))
        (go end_label)))
      (setf rr (expt r nnnn))
      (setf t$
              (f2cl-lib:atan2
               (f2cl-lib:fref xxxx-%data% (2) ((1 2)) xxxx-%offset%)
               (f2cl-lib:fref xxxx-%data% (1) ((1 2)) xxxx-%offset%)))
      (setf tt (* nnnn t$))
      (setf (f2cl-lib:fref yyyy-%data% (1) ((1 2)) yyyy-%offset%)
              (* rr (cos tt)))
      (setf (f2cl-lib:fref yyyy-%data% (2) ((1 2)) yyyy-%offset%)
              (* rr (sin tt)))
      (go end_label)
     end_label
      (return (values nil nil nil)))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::powp fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo :arg-types
                                            '((fortran-to-lisp::integer4)
                                              (array double-float (*))
                                              (array double-float (*)))
                                            :return-values '(nil nil nil)
                                            :calls '(fortran-to-lisp::dnrm2))))

