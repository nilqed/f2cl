;;; Compiled by f2cl version:
;;; ("" "" "" "" "" "" "")

;;; Using Lisp SBCL 1.4.2
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing t) (:declare-common nil)
;;;           (:float-format single-float))

(in-package "LAPACK")


(defun ieeeck (ispec zero one)
  (declare (type (single-float) one zero)
           (type (f2cl-lib:integer4) ispec))
  (prog ((nan1 0.0) (nan2 0.0) (nan3 0.0) (nan4 0.0) (nan5 0.0) (nan6 0.0)
         (neginf 0.0) (negzro 0.0) (newzro 0.0) (posinf 0.0) (ieeeck 0))
    (declare (type (f2cl-lib:integer4) ieeeck)
             (type (single-float) posinf newzro negzro neginf nan6 nan5 nan4
              nan3 nan2 nan1))
    (setf ieeeck 1)
    (setf posinf (/ one zero))
    (cond ((<= posinf one) (setf ieeeck 0) (go end_label)))
    (setf neginf (/ (- one) zero))
    (cond ((>= neginf zero) (setf ieeeck 0) (go end_label)))
    (setf negzro (/ one (+ neginf one)))
    (cond ((/= negzro zero) (setf ieeeck 0) (go end_label)))
    (setf neginf (/ one negzro))
    (cond ((>= neginf zero) (setf ieeeck 0) (go end_label)))
    (setf newzro (+ negzro zero))
    (cond ((/= newzro zero) (setf ieeeck 0) (go end_label)))
    (setf posinf (/ one newzro))
    (cond ((<= posinf one) (setf ieeeck 0) (go end_label)))
    (setf neginf (* neginf posinf))
    (cond ((>= neginf zero) (setf ieeeck 0) (go end_label)))
    (setf posinf (* posinf posinf))
    (cond ((<= posinf one) (setf ieeeck 0) (go end_label)))
    (if (= ispec 0)
        (go end_label))
    (setf nan1 (+ posinf neginf))
    (setf nan2 (/ posinf neginf))
    (setf nan3 (/ posinf posinf))
    (setf nan4 (* posinf zero))
    (setf nan5 (* neginf negzro))
    (setf nan6 (* nan5 0.0))
    (cond ((= nan1 nan1) (setf ieeeck 0) (go end_label)))
    (cond ((= nan2 nan2) (setf ieeeck 0) (go end_label)))
    (cond ((= nan3 nan3) (setf ieeeck 0) (go end_label)))
    (cond ((= nan4 nan4) (setf ieeeck 0) (go end_label)))
    (cond ((= nan5 nan5) (setf ieeeck 0) (go end_label)))
    (cond ((= nan6 nan6) (setf ieeeck 0) (go end_label)))
    (go end_label)
   end_label
    (return (values ieeeck nil nil nil))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::ieeeck
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo :arg-types
                                            '((fortran-to-lisp::integer4)
                                              (single-float) (single-float))
                                            :return-values '(nil nil nil)
                                            :calls 'nil)))

