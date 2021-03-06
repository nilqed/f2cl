;;; Compiled by f2cl version:
;;; ("" "" "" "" "" "" "")

;;; Using Lisp SBCL 1.4.2
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing t) (:declare-common nil)
;;;           (:float-format single-float))

(in-package "ODEPACK")


(defun adjlr (n isp ldif)
  (declare (type (array f2cl-lib:integer4 (*)) isp)
           (type (f2cl-lib:integer4) ldif n))
  (f2cl-lib:with-multi-array-data
      ((isp f2cl-lib:integer4 isp-%data% isp-%offset%))
    (prog ((ip 0) (jlmax 0) (jumax 0) (lnfc 0) (lsfc 0) (nzlu 0))
      (declare (type (f2cl-lib:integer4) nzlu lsfc lnfc jumax jlmax ip))
      (setf ip (f2cl-lib:int-add (f2cl-lib:int-mul 2 n) 1))
      (setf jlmax (f2cl-lib:fref isp-%data% (ip) ((1 *)) isp-%offset%))
      (setf jumax
              (f2cl-lib:fref isp-%data% ((f2cl-lib:int-add ip ip)) ((1 *))
                             isp-%offset%))
      (setf nzlu
              (f2cl-lib:int-sub
               (f2cl-lib:int-add
                (f2cl-lib:int-sub
                 (f2cl-lib:fref isp-%data% ((f2cl-lib:int-add n 1)) ((1 *))
                                isp-%offset%)
                 (f2cl-lib:fref isp-%data% (1) ((1 *)) isp-%offset%))
                (f2cl-lib:fref isp-%data% ((f2cl-lib:int-add ip n 1)) ((1 *))
                               isp-%offset%))
               (f2cl-lib:fref isp-%data% ((f2cl-lib:int-add ip 1)) ((1 *))
                              isp-%offset%)))
      (setf lsfc
              (f2cl-lib:int-add (f2cl-lib:int-mul 12 n) 3
                                (f2cl-lib:int-mul 2
                                                  (max
                                                   (the f2cl-lib:integer4
                                                        jlmax)
                                                   (the f2cl-lib:integer4
                                                        jumax)))))
      (setf lnfc (f2cl-lib:int-add (f2cl-lib:int-mul 9 n) 2 jlmax jumax nzlu))
      (setf ldif
              (max (the f2cl-lib:integer4 0)
                   (the f2cl-lib:integer4 (f2cl-lib:int-sub lsfc lnfc))))
      (go end_label)
     end_label
      (return (values nil nil ldif)))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::adjlr fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo :arg-types
                                            '((fortran-to-lisp::integer4)
                                              (array fortran-to-lisp::integer4
                                               (*))
                                              (fortran-to-lisp::integer4))
                                            :return-values
                                            '(nil nil fortran-to-lisp::ldif)
                                            :calls 'nil)))

