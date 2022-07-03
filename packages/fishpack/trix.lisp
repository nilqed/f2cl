;;; Compiled by f2cl version:
;;; ("" "" "" "" "" "" "")

;;; Using Lisp SBCL 1.4.2
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing t) (:declare-common nil)
;;;           (:float-format single-float))

(in-package "FISHPACK")


(defun trix (idegbr idegcr m a b c y tcos d w)
  (declare (type (array single-float (*)) w d tcos y c b a)
           (type (f2cl-lib:integer4) m idegcr idegbr))
  (f2cl-lib:with-multi-array-data
      ((a single-float a-%data% a-%offset%)
       (b single-float b-%data% b-%offset%)
       (c single-float c-%data% c-%offset%)
       (y single-float y-%data% y-%offset%)
       (tcos single-float tcos-%data% tcos-%offset%)
       (d single-float d-%data% d-%offset%)
       (w single-float w-%data% w-%offset%))
    (prog ((ip 0) (z 0.0) (xx 0.0) (i 0) (x 0.0) (k 0) (lint 0) (l 0) (fc 0.0)
           (fb 0.0) (mm1 0))
      (declare (type (single-float) fb fc x xx z)
               (type (f2cl-lib:integer4) mm1 l lint k i ip))
      (setf mm1 (f2cl-lib:int-sub m 1))
      (setf fb
              (coerce (the f2cl-lib:integer4 (f2cl-lib:int-add idegbr 1))
                      'single-float))
      (setf fc
              (coerce (the f2cl-lib:integer4 (f2cl-lib:int-add idegcr 1))
                      'single-float))
      (setf l (f2cl-lib:int (/ fb fc)))
      (setf lint 1)
      (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                    ((> k idegbr) nil)
        (tagbody
          (setf x (f2cl-lib:fref tcos-%data% (k) ((1 1)) tcos-%offset%))
          (if (/= k l)
              (go label102))
          (setf i (f2cl-lib:int-add idegbr lint))
          (setf xx (- x (f2cl-lib:fref tcos-%data% (i) ((1 1)) tcos-%offset%)))
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i m) nil)
            (tagbody
              (setf (f2cl-lib:fref w-%data% (i) ((1 1)) w-%offset%)
                      (f2cl-lib:fref y-%data% (i) ((1 1)) y-%offset%))
              (setf (f2cl-lib:fref y-%data% (i) ((1 1)) y-%offset%)
                      (* xx (f2cl-lib:fref y-%data% (i) ((1 1)) y-%offset%)))
             label101))
         label102
          (setf z
                  (/ 1.0
                     (- (f2cl-lib:fref b-%data% (1) ((1 1)) b-%offset%) x)))
          (setf (f2cl-lib:fref d-%data% (1) ((1 1)) d-%offset%)
                  (* (f2cl-lib:fref c-%data% (1) ((1 1)) c-%offset%) z))
          (setf (f2cl-lib:fref y-%data% (1) ((1 1)) y-%offset%)
                  (* (f2cl-lib:fref y-%data% (1) ((1 1)) y-%offset%) z))
          (f2cl-lib:fdo (i 2 (f2cl-lib:int-add i 1))
                        ((> i mm1) nil)
            (tagbody
              (setf z
                      (/ 1.0
                         (- (f2cl-lib:fref b-%data% (i) ((1 1)) b-%offset%) x
                            (* (f2cl-lib:fref a-%data% (i) ((1 1)) a-%offset%)
                               (f2cl-lib:fref d-%data% ((f2cl-lib:int-sub i 1))
                                              ((1 1)) d-%offset%)))))
              (setf (f2cl-lib:fref d-%data% (i) ((1 1)) d-%offset%)
                      (* (f2cl-lib:fref c-%data% (i) ((1 1)) c-%offset%) z))
              (setf (f2cl-lib:fref y-%data% (i) ((1 1)) y-%offset%)
                      (*
                       (- (f2cl-lib:fref y-%data% (i) ((1 1)) y-%offset%)
                          (* (f2cl-lib:fref a-%data% (i) ((1 1)) a-%offset%)
                             (f2cl-lib:fref y-%data% ((f2cl-lib:int-sub i 1))
                                            ((1 1)) y-%offset%)))
                       z))
             label103))
          (setf z
                  (- (f2cl-lib:fref b-%data% (m) ((1 1)) b-%offset%) x
                     (* (f2cl-lib:fref a-%data% (m) ((1 1)) a-%offset%)
                        (f2cl-lib:fref d-%data% (mm1) ((1 1)) d-%offset%))))
          (if (/= z 0.0)
              (go label104))
          (setf (f2cl-lib:fref y-%data% (m) ((1 1)) y-%offset%) 0.0)
          (go label105)
         label104
          (setf (f2cl-lib:fref y-%data% (m) ((1 1)) y-%offset%)
                  (/
                   (- (f2cl-lib:fref y-%data% (m) ((1 1)) y-%offset%)
                      (* (f2cl-lib:fref a-%data% (m) ((1 1)) a-%offset%)
                         (f2cl-lib:fref y-%data% (mm1) ((1 1)) y-%offset%)))
                   z))
         label105
          (f2cl-lib:fdo (ip 1 (f2cl-lib:int-add ip 1))
                        ((> ip mm1) nil)
            (tagbody
              (setf i (f2cl-lib:int-sub m ip))
              (setf (f2cl-lib:fref y-%data% (i) ((1 1)) y-%offset%)
                      (- (f2cl-lib:fref y-%data% (i) ((1 1)) y-%offset%)
                         (* (f2cl-lib:fref d-%data% (i) ((1 1)) d-%offset%)
                            (f2cl-lib:fref y-%data% ((f2cl-lib:int-add i 1))
                                           ((1 1)) y-%offset%))))
             label106))
          (if (/= k l)
              (go label108))
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i m) nil)
            (tagbody
              (setf (f2cl-lib:fref y-%data% (i) ((1 1)) y-%offset%)
                      (+ (f2cl-lib:fref y-%data% (i) ((1 1)) y-%offset%)
                         (f2cl-lib:fref w-%data% (i) ((1 1)) w-%offset%)))
             label107))
          (setf lint (f2cl-lib:int-add lint 1))
          (setf l (f2cl-lib:int (/ (* (f2cl-lib:ffloat lint) fb) fc)))
         label108))
      (go end_label)
     end_label
      (return (values nil nil nil nil nil nil nil nil nil nil)))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::trix fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo :arg-types
                                            '((fortran-to-lisp::integer4)
                                              (fortran-to-lisp::integer4)
                                              (fortran-to-lisp::integer4)
                                              (array single-float (*))
                                              (array single-float (*))
                                              (array single-float (*))
                                              (array single-float (*))
                                              (array single-float (*))
                                              (array single-float (*))
                                              (array single-float (*)))
                                            :return-values
                                            '(nil nil nil nil nil nil nil nil
                                              nil nil)
                                            :calls 'nil)))

