;;; Compiled by f2cl version:
;;; ("" "" "" "" "" "" "")

;;; Using Lisp SBCL 1.4.2
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing t) (:declare-common nil)
;;;           (:float-format single-float))

(in-package "QUADPACK")


(defun xerrwv (messg nmessg nerr level ni i1 i2 nr r1 r2)
  (declare (type (single-float) r2 r1)
           (type (f2cl-lib:integer4) nr i2 i1 ni level nerr nmessg)
           (type (string *) messg))
  (f2cl-lib:with-multi-array-data
      ((messg character messg-%data% messg-%offset%))
    (prog ((lun (make-array 5 :element-type 'f2cl-lib:integer4))
           (form
            (make-array '(37) :element-type 'character :initial-element #\ ))
           (lfirst
            (make-array '(20) :element-type 'character :initial-element #\ ))
           (ifatal 0) (i 0) (iunit 0) (kunit 0) (isizef 0) (isizei 0) (nunit 0)
           (mkntrl 0) (llevel 0) (lerr 0) (lmessg 0) (kount 0) (junk 0)
           (kdummy 0) (maxmes 0) (lkntrl 0))
      (declare
       (type (f2cl-lib:integer4) lkntrl maxmes kdummy junk kount lmessg lerr
        llevel mkntrl nunit isizei isizef kunit iunit i ifatal)
       (type (string 20) lfirst)
       (type (string 37) form)
       (type (array f2cl-lib:integer4 (5)) lun))
      (setf lkntrl (j4save 2 0 f2cl-lib:%false%))
      (setf maxmes (j4save 4 0 f2cl-lib:%false%))
      (if (and (> nmessg 0) (/= nerr 0) (>= level -1) (<= level 2))
          (go label10))
      (if (> lkntrl 0)
          (xerprt "FATAL ERROR IN..." 17))
      (xerprt "XERROR -- INVALID INPUT" 23)
      (if (> lkntrl 0)
          (fdump))
      (if (> lkntrl 0)
          (xerprt "JOB ABORT DUE TO FATAL ERROR." 29))
      (if (> lkntrl 0)
          (multiple-value-bind (var-0 var-1 var-2 var-3 var-4)
              (xersav " " 0 0 0 kdummy)
            (declare (ignore var-0 var-1 var-2 var-3))
            (when var-4 (setf kdummy var-4))))
      (xerabt "XERROR -- INVALID INPUT" 23)
      (go end_label)
     label10
      (setf junk
              (multiple-value-bind (ret-val var-0 var-1 var-2)
                  (j4save 1 nerr f2cl-lib:%true%)
                (declare (ignore var-0 var-2))
                (when var-1 (setf nerr var-1))
                ret-val))
      (multiple-value-bind (var-0 var-1 var-2 var-3 var-4)
          (xersav messg nmessg nerr level kount)
        (declare (ignore))
        (when var-0 (setf messg var-0))
        (when var-1 (setf nmessg var-1))
        (when var-2 (setf nerr var-2))
        (when var-3 (setf level var-3))
        (when var-4 (setf kount var-4)))
      (f2cl-lib:f2cl-set-string lfirst messg (string 20))
      (setf lmessg nmessg)
      (setf lerr nerr)
      (setf llevel level)
      (multiple-value-bind (var-0 var-1 var-2 var-3 var-4)
          (xerctl lfirst lmessg lerr llevel lkntrl)
        (declare (ignore))
        (when var-0 (setf lfirst var-0))
        (when var-1 (setf lmessg var-1))
        (when var-2 (setf lerr var-2))
        (when var-3 (setf llevel var-3))
        (when var-4 (setf lkntrl var-4)))
      (setf lmessg nmessg)
      (setf lerr nerr)
      (setf llevel level)
      (setf lkntrl (f2cl-lib:max0 -2 (f2cl-lib:min0 2 lkntrl)))
      (setf mkntrl (f2cl-lib:iabs lkntrl))
      (if (and (< llevel 2) (= lkntrl 0))
          (go label100))
      (if (or (and (= llevel -1) (> kount (f2cl-lib:min0 1 maxmes)))
              (and (= llevel 0) (> kount maxmes))
              (and (= llevel 1) (> kount maxmes) (= mkntrl 1))
              (and (= llevel 2) (> kount (f2cl-lib:max0 1 maxmes))))
          (go label100))
      (if (<= lkntrl 0)
          (go label20))
      (xerprt " " 1)
      (if (= llevel -1)
          (xerprt "WARNING MESSAGE...THIS MESSAGE WILL ONLY BE PRINTED ONCE."
           57))
      (if (= llevel 0)
          (xerprt "WARNING IN..." 13))
      (if (= llevel 1)
          (xerprt "RECOVERABLE ERROR IN..." 23))
      (if (= llevel 2)
          (xerprt "FATAL ERROR IN..." 17))
     label20
      (multiple-value-bind (var-0 var-1)
          (xerprt messg lmessg)
        (declare (ignore))
        (when var-0 (setf messg var-0))
        (when var-1 (setf lmessg var-1)))
      (multiple-value-bind (var-0 var-1)
          (xgetua lun nunit)
        (declare (ignore var-0))
        (when var-1 (setf nunit var-1)))
      (setf isizei
              (f2cl-lib:int
               (+ (f2cl-lib:log10 (f2cl-lib:ffloat (f2cl-lib:i1mach 9))) 1.0)))
      (setf isizef
              (f2cl-lib:int
               (+
                (f2cl-lib:log10
                 (expt (f2cl-lib:ffloat (f2cl-lib:i1mach 10))
                       (f2cl-lib:i1mach 11)))
                1.0)))
      (f2cl-lib:fdo (kunit 1 (f2cl-lib:int-add kunit 1))
                    ((> kunit nunit) nil)
        (tagbody
          (setf iunit (f2cl-lib:fref lun (kunit) ((1 5))))
          (if (= iunit 0)
              (setf iunit (f2cl-lib:i1mach 4)))
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i
                            (min (the f2cl-lib:integer4 ni)
                                 (the f2cl-lib:integer4 2)))
                         nil)
            (tagbody
              (f2cl-lib:fformat form
                                ("(11X,21HIN ABOVE MESSAGE, I" 1 (("~1D"))
                                 "=,I" 1 (("~2D")) ")   " "~%")
                                i isizei)
              (if (= i 1)
                  (f2cl-lib:fformat iunit :list-directed i1))
              (if (= i 2)
                  (f2cl-lib:fformat iunit :list-directed i2))
             label22))
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i
                            (min (the f2cl-lib:integer4 nr)
                                 (the f2cl-lib:integer4 2)))
                         nil)
            (tagbody
              (f2cl-lib:fformat form
                                ("(11X,21HIN ABOVE MESSAGE, R" 1 (("~1D"))
                                 "=,E" 1 (("~2D")) "." 1 (("~2D")) ")" "~%")
                                i (f2cl-lib:int-add isizef 10) isizef)
              (if (= i 1)
                  (f2cl-lib:fformat iunit :list-directed r1))
              (if (= i 2)
                  (f2cl-lib:fformat iunit :list-directed r2))
             label24))
          (if (<= lkntrl 0)
              (go label40))
          (f2cl-lib:fformat iunit (" ERROR NUMBER =" 1 (("~10D")) "~%") lerr)
         label40
         label50))
      (if (> lkntrl 0)
          (fdump))
     label100
      (setf ifatal 0)
      (if (or (= llevel 2) (and (= llevel 1) (= mkntrl 2)))
          (setf ifatal 1))
      (if (<= ifatal 0)
          (go end_label))
      (if (or (<= lkntrl 0) (> kount (f2cl-lib:max0 1 maxmes)))
          (go label120))
      (if (= llevel 1)
          (xerprt "JOB ABORT DUE TO UNRECOVERED ERROR." 35))
      (if (= llevel 2)
          (xerprt "JOB ABORT DUE TO FATAL ERROR." 29))
      (multiple-value-bind (var-0 var-1 var-2 var-3 var-4)
          (xersav " " -1 0 0 kdummy)
        (declare (ignore var-0 var-1 var-2 var-3))
        (when var-4 (setf kdummy var-4)))
     label120
      (if (and (= llevel 2) (> kount (f2cl-lib:max0 1 maxmes)))
          (setf lmessg 0))
      (xerabt messg lmessg)
      (go end_label)
     end_label
      (return (values messg nmessg nerr level nil nil nil nil nil nil)))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::xerrwv
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo :arg-types
                                            '((string)
                                              (fortran-to-lisp::integer4)
                                              (fortran-to-lisp::integer4)
                                              (fortran-to-lisp::integer4)
                                              (fortran-to-lisp::integer4)
                                              (fortran-to-lisp::integer4)
                                              (fortran-to-lisp::integer4)
                                              (fortran-to-lisp::integer4)
                                              (single-float) (single-float))
                                            :return-values
                                            '(fortran-to-lisp::messg
                                              fortran-to-lisp::nmessg
                                              fortran-to-lisp::nerr
                                              fortran-to-lisp::level nil nil
                                              nil nil nil nil)
                                            :calls
                                            '(fortran-to-lisp::i1mach
                                              fortran-to-lisp::xerabt))))

