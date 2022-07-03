;;; Compiled by f2cl version:
;;; ("" "" "" "" "" "" "")

;;; Using Lisp SBCL 1.4.2
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing t) (:declare-common nil)
;;;           (:float-format single-float))

(in-package "ODEPACK")


(let ((lunit -1) (mesflg 1))
  (declare (type (f2cl-lib:integer4) lunit mesflg))
  (defun ixsav (ipar ivalue iset)
    (declare (type f2cl-lib:logical iset)
             (type (f2cl-lib:integer4) ivalue ipar))
    (prog ((ixsav 0))
      (declare (type (f2cl-lib:integer4) ixsav))
      (cond
       ((= ipar 1)
        (if (= lunit -1)
            (setf lunit (iumach)))
        (setf ixsav lunit)
        (if iset
            (setf lunit ivalue))))
      (cond
       ((= ipar 2) (setf ixsav mesflg)
        (if iset
            (setf mesflg ivalue))))
      (go end_label)
     end_label
      (return (values ixsav nil nil nil)))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::ixsav fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo :arg-types
                                            '((fortran-to-lisp::integer4)
                                              (fortran-to-lisp::integer4)
                                              fortran-to-lisp::logical)
                                            :return-values '(nil nil nil)
                                            :calls 'nil)))

