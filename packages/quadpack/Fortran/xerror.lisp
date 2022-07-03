;;; Compiled by f2cl version:
;;; ("" "" "" "" "" "" "")

;;; Using Lisp SBCL 1.4.2
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing t) (:declare-common nil)
;;;           (:float-format single-float))

(in-package "QUADPACK")


(defun xerror (messg nmessg nerr level)
  (declare (type (f2cl-lib:integer4) level nerr nmessg)
           (type (string *) messg))
  (f2cl-lib:with-multi-array-data
      ((messg character messg-%data% messg-%offset%))
    (prog ()
      (declare)
      (multiple-value-bind
          (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9)
          (xerrwv messg nmessg nerr level 0 0 0 0 0.0 0.0)
        (declare (ignore var-4 var-5 var-6 var-7 var-8 var-9))
        (setf messg var-0)
        (setf nmessg var-1)
        (setf nerr var-2)
        (setf level var-3))
      (go end_label)
     end_label
      (return (values messg nmessg nerr level)))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::xerror
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo :arg-types
                                            '((string)
                                              (fortran-to-lisp::integer4)
                                              (fortran-to-lisp::integer4)
                                              (fortran-to-lisp::integer4))
                                            :return-values
                                            '(fortran-to-lisp::messg
                                              fortran-to-lisp::nmessg
                                              fortran-to-lisp::nerr
                                              fortran-to-lisp::level)
                                            :calls '(fortran-to-lisp::xerrwv))))

