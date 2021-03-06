;;; Compiled by f2cl version:
;;; ("" "" "" "" "" "" "")

;;; Using Lisp SBCL 1.4.2
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing t) (:declare-common nil)
;;;           (:float-format single-float))

(in-package "LAPACK")


(defun dladiv (a b c d p q)
  (declare (type (double-float) q p d c b a))
  (prog ((e 0.0d0) (f 0.0d0))
    (declare (type (double-float) f e))
    (cond
     ((< (abs d) (abs c)) (setf e (/ d c)) (setf f (+ c (* d e)))
      (setf p (/ (+ a (* b e)) f)) (setf q (/ (- b (* a e)) f)))
     (t (setf e (/ c d)) (setf f (+ d (* c e))) (setf p (/ (+ b (* a e)) f))
      (setf q (/ (- (* b e) a) f))))
    (go end_label)
   end_label
    (return (values nil nil nil nil p q))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dladiv
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo :arg-types
                                            '((double-float) (double-float)
                                              (double-float) (double-float)
                                              (double-float) (double-float))
                                            :return-values
                                            '(nil nil nil nil
                                              fortran-to-lisp::p
                                              fortran-to-lisp::q)
                                            :calls 'nil)))

