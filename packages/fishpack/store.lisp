;;; Compiled by f2cl version:
;;; ("" "" "" "" "" "" "")

;;; Using Lisp SBCL 1.4.2
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing t) (:declare-common t)
;;;           (:float-format single-float))

(in-package "FISHPACK")


(defstruct (value (:predicate is-value-p)) (v 0.0 :type (single-float)))


(defparameter *value-common-block*
  (let* ()
    (declare (ignorable))
    (make-value)))


(defun store (x)
  (declare (type (single-float) x))
  (let ()
    (symbol-macrolet ((v (value-v *value-common-block*)))
      (prog ()
        (declare)
        (setf v x)
        (go end_label)
       end_label
        (return (values nil))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::store fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo :arg-types '((single-float))
                                            :return-values '(nil) :calls 'nil)))

