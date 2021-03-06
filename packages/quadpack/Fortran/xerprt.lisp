;;; Compiled by f2cl version:
;;; ("" "" "" "" "" "" "")

;;; Using Lisp SBCL 1.4.2
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing t) (:declare-common nil)
;;;           (:float-format single-float))

(in-package "QUADPACK")


(defun xerprt (messg nmessg)
  (declare (type (f2cl-lib:integer4) nmessg)
           (type (string *) messg))
  (f2cl-lib:with-multi-array-data
      ((messg character messg-%data% messg-%offset%))
    (prog ((lun (make-array 5 :element-type 'f2cl-lib:integer4)) (last$ 0)
           (f2cl-lib:ichar 0) (iunit 0) (kunit 0) (lenmes 0) (nunit 0))
      (declare
       (type (f2cl-lib:integer4) nunit lenmes kunit iunit f2cl-lib:ichar last$)
       (type (array f2cl-lib:integer4 (5)) lun))
      (multiple-value-bind (var-0 var-1)
          (xgetua lun nunit)
        (declare (ignore var-0))
        (when var-1 (setf nunit var-1)))
      (setf lenmes (f2cl-lib:len messg))
      (f2cl-lib:fdo (kunit 1 (f2cl-lib:int-add kunit 1))
                    ((> kunit nunit) nil)
        (tagbody
          (setf iunit (f2cl-lib:fref lun (kunit) ((1 5))))
          (if (= iunit 0)
              (setf iunit (f2cl-lib:i1mach 4)))
          (f2cl-lib:fdo (f2cl-lib:ichar 1 (f2cl-lib:int-add f2cl-lib:ichar 72))
                        ((> f2cl-lib:ichar lenmes) nil)
            (tagbody
              (setf last$
                      (f2cl-lib:min0 (f2cl-lib:int-add f2cl-lib:ichar 71)
                                     lenmes))
              (f2cl-lib:fformat iunit (t ("~1@T" ("~A")) "~%")
                                (f2cl-lib:fref-string messg
                                                      (f2cl-lib:ichar last$)))
             label10))
         label20))
      (go end_label)
     end_label
      (return (values nil nil)))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::xerprt
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo :arg-types
                                            '((string)
                                              (fortran-to-lisp::integer4))
                                            :return-values '(nil nil) :calls
                                            '(fortran-to-lisp::i1mach))))

