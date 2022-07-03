;;; Compiled by f2cl version:
;;; ("" "" "" "" "" "" "")

;;; Using Lisp SBCL 1.4.2
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing t) (:declare-common nil)
;;;           (:float-format single-float))

(in-package "FISHPACK")


(defun tri3 (m a b c k y1 y2 y3 tcos d w1 w2 w3)
  (declare (type (array f2cl-lib:integer4 (*)) k)
           (type (array single-float (*)) w3 w2 w1 d tcos y3 y2 y1 c b a)
           (type (f2cl-lib:integer4) m))
  (f2cl-lib:with-multi-array-data
      ((a single-float a-%data% a-%offset%)
       (b single-float b-%data% b-%offset%)
       (c single-float c-%data% c-%offset%)
       (y1 single-float y1-%data% y1-%offset%)
       (y2 single-float y2-%data% y2-%offset%)
       (y3 single-float y3-%data% y3-%offset%)
       (tcos single-float tcos-%data% tcos-%offset%)
       (d single-float d-%data% d-%offset%)
       (w1 single-float w1-%data% w1-%offset%)
       (w2 single-float w2-%data% w2-%offset%)
       (w3 single-float w3-%data% w3-%offset%)
       (k f2cl-lib:integer4 k-%data% k-%offset%))
    (prog ((xx 0.0) (ip 0) (z 0.0) (i 0) (x 0.0) (n 0) (kint3 0) (kint2 0)
           (kint1 0) (lint3 0) (lint2 0) (lint1 0) (l3 0) (l2 0) (l1 0)
           (k2k3k4 0) (f4 0.0) (f3 0.0) (f2 0.0) (f1 0.0) (k4 0) (k3 0) (k2 0)
           (k1 0) (mm1 0))
      (declare
       (type (f2cl-lib:integer4) mm1 k1 k2 k3 k4 k2k3k4 l1 l2 l3 lint1 lint2
        lint3 kint1 kint2 kint3 n i ip)
       (type (single-float) f1 f2 f3 f4 x z xx))
      (setf mm1 (f2cl-lib:int-sub m 1))
      (setf k1 (f2cl-lib:fref k-%data% (1) ((1 4)) k-%offset%))
      (setf k2 (f2cl-lib:fref k-%data% (2) ((1 4)) k-%offset%))
      (setf k3 (f2cl-lib:fref k-%data% (3) ((1 4)) k-%offset%))
      (setf k4 (f2cl-lib:fref k-%data% (4) ((1 4)) k-%offset%))
      (setf f1
              (coerce (the f2cl-lib:integer4 (f2cl-lib:int-add k1 1))
                      'single-float))
      (setf f2
              (coerce (the f2cl-lib:integer4 (f2cl-lib:int-add k2 1))
                      'single-float))
      (setf f3
              (coerce (the f2cl-lib:integer4 (f2cl-lib:int-add k3 1))
                      'single-float))
      (setf f4
              (coerce (the f2cl-lib:integer4 (f2cl-lib:int-add k4 1))
                      'single-float))
      (setf k2k3k4 (f2cl-lib:int-add k2 k3 k4))
      (if (= k2k3k4 0)
          (go label101))
      (setf l1 (f2cl-lib:int (/ f1 f2)))
      (setf l2 (f2cl-lib:int (/ f1 f3)))
      (setf l3 (f2cl-lib:int (/ f1 f4)))
      (setf lint1 1)
      (setf lint2 1)
      (setf lint3 1)
      (setf kint1 k1)
      (setf kint2 (f2cl-lib:int-add kint1 k2))
      (setf kint3 (f2cl-lib:int-add kint2 k3))
     label101
      (f2cl-lib:fdo (n 1 (f2cl-lib:int-add n 1))
                    ((> n k1) nil)
        (tagbody
          (setf x (f2cl-lib:fref tcos-%data% (n) ((1 1)) tcos-%offset%))
          (if (= k2k3k4 0)
              (go label107))
          (if (/= n l1)
              (go label103))
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i m) nil)
            (tagbody
              (setf (f2cl-lib:fref w1-%data% (i) ((1 1)) w1-%offset%)
                      (f2cl-lib:fref y1-%data% (i) ((1 1)) y1-%offset%))
             label102))
         label103
          (if (/= n l2)
              (go label105))
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i m) nil)
            (tagbody
              (setf (f2cl-lib:fref w2-%data% (i) ((1 1)) w2-%offset%)
                      (f2cl-lib:fref y2-%data% (i) ((1 1)) y2-%offset%))
             label104))
         label105
          (if (/= n l3)
              (go label107))
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i m) nil)
            (tagbody
              (setf (f2cl-lib:fref w3-%data% (i) ((1 1)) w3-%offset%)
                      (f2cl-lib:fref y3-%data% (i) ((1 1)) y3-%offset%))
             label106))
         label107
          (setf z
                  (/ 1.0
                     (- (f2cl-lib:fref b-%data% (1) ((1 1)) b-%offset%) x)))
          (setf (f2cl-lib:fref d-%data% (1) ((1 1)) d-%offset%)
                  (* (f2cl-lib:fref c-%data% (1) ((1 1)) c-%offset%) z))
          (setf (f2cl-lib:fref y1-%data% (1) ((1 1)) y1-%offset%)
                  (* (f2cl-lib:fref y1-%data% (1) ((1 1)) y1-%offset%) z))
          (setf (f2cl-lib:fref y2-%data% (1) ((1 1)) y2-%offset%)
                  (* (f2cl-lib:fref y2-%data% (1) ((1 1)) y2-%offset%) z))
          (setf (f2cl-lib:fref y3-%data% (1) ((1 1)) y3-%offset%)
                  (* (f2cl-lib:fref y3-%data% (1) ((1 1)) y3-%offset%) z))
          (f2cl-lib:fdo (i 2 (f2cl-lib:int-add i 1))
                        ((> i m) nil)
            (tagbody
              (setf z
                      (/ 1.0
                         (- (f2cl-lib:fref b-%data% (i) ((1 1)) b-%offset%) x
                            (* (f2cl-lib:fref a-%data% (i) ((1 1)) a-%offset%)
                               (f2cl-lib:fref d-%data% ((f2cl-lib:int-sub i 1))
                                              ((1 1)) d-%offset%)))))
              (setf (f2cl-lib:fref d-%data% (i) ((1 1)) d-%offset%)
                      (* (f2cl-lib:fref c-%data% (i) ((1 1)) c-%offset%) z))
              (setf (f2cl-lib:fref y1-%data% (i) ((1 1)) y1-%offset%)
                      (*
                       (- (f2cl-lib:fref y1-%data% (i) ((1 1)) y1-%offset%)
                          (* (f2cl-lib:fref a-%data% (i) ((1 1)) a-%offset%)
                             (f2cl-lib:fref y1-%data% ((f2cl-lib:int-sub i 1))
                                            ((1 1)) y1-%offset%)))
                       z))
              (setf (f2cl-lib:fref y2-%data% (i) ((1 1)) y2-%offset%)
                      (*
                       (- (f2cl-lib:fref y2-%data% (i) ((1 1)) y2-%offset%)
                          (* (f2cl-lib:fref a-%data% (i) ((1 1)) a-%offset%)
                             (f2cl-lib:fref y2-%data% ((f2cl-lib:int-sub i 1))
                                            ((1 1)) y2-%offset%)))
                       z))
              (setf (f2cl-lib:fref y3-%data% (i) ((1 1)) y3-%offset%)
                      (*
                       (- (f2cl-lib:fref y3-%data% (i) ((1 1)) y3-%offset%)
                          (* (f2cl-lib:fref a-%data% (i) ((1 1)) a-%offset%)
                             (f2cl-lib:fref y3-%data% ((f2cl-lib:int-sub i 1))
                                            ((1 1)) y3-%offset%)))
                       z))
             label108))
          (f2cl-lib:fdo (ip 1 (f2cl-lib:int-add ip 1))
                        ((> ip mm1) nil)
            (tagbody
              (setf i (f2cl-lib:int-sub m ip))
              (setf (f2cl-lib:fref y1-%data% (i) ((1 1)) y1-%offset%)
                      (- (f2cl-lib:fref y1-%data% (i) ((1 1)) y1-%offset%)
                         (* (f2cl-lib:fref d-%data% (i) ((1 1)) d-%offset%)
                            (f2cl-lib:fref y1-%data% ((f2cl-lib:int-add i 1))
                                           ((1 1)) y1-%offset%))))
              (setf (f2cl-lib:fref y2-%data% (i) ((1 1)) y2-%offset%)
                      (- (f2cl-lib:fref y2-%data% (i) ((1 1)) y2-%offset%)
                         (* (f2cl-lib:fref d-%data% (i) ((1 1)) d-%offset%)
                            (f2cl-lib:fref y2-%data% ((f2cl-lib:int-add i 1))
                                           ((1 1)) y2-%offset%))))
              (setf (f2cl-lib:fref y3-%data% (i) ((1 1)) y3-%offset%)
                      (- (f2cl-lib:fref y3-%data% (i) ((1 1)) y3-%offset%)
                         (* (f2cl-lib:fref d-%data% (i) ((1 1)) d-%offset%)
                            (f2cl-lib:fref y3-%data% ((f2cl-lib:int-add i 1))
                                           ((1 1)) y3-%offset%))))
             label109))
          (if (= k2k3k4 0)
              (go label115))
          (if (/= n l1)
              (go label111))
          (setf i (f2cl-lib:int-add lint1 kint1))
          (setf xx (- x (f2cl-lib:fref tcos-%data% (i) ((1 1)) tcos-%offset%)))
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i m) nil)
            (tagbody
              (setf (f2cl-lib:fref y1-%data% (i) ((1 1)) y1-%offset%)
                      (+
                       (* xx (f2cl-lib:fref y1-%data% (i) ((1 1)) y1-%offset%))
                       (f2cl-lib:fref w1-%data% (i) ((1 1)) w1-%offset%)))
             label110))
          (setf lint1 (f2cl-lib:int-add lint1 1))
          (setf l1 (f2cl-lib:int (/ (* (f2cl-lib:ffloat lint1) f1) f2)))
         label111
          (if (/= n l2)
              (go label113))
          (setf i (f2cl-lib:int-add lint2 kint2))
          (setf xx (- x (f2cl-lib:fref tcos-%data% (i) ((1 1)) tcos-%offset%)))
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i m) nil)
            (tagbody
              (setf (f2cl-lib:fref y2-%data% (i) ((1 1)) y2-%offset%)
                      (+
                       (* xx (f2cl-lib:fref y2-%data% (i) ((1 1)) y2-%offset%))
                       (f2cl-lib:fref w2-%data% (i) ((1 1)) w2-%offset%)))
             label112))
          (setf lint2 (f2cl-lib:int-add lint2 1))
          (setf l2 (f2cl-lib:int (/ (* (f2cl-lib:ffloat lint2) f1) f3)))
         label113
          (if (/= n l3)
              (go label115))
          (setf i (f2cl-lib:int-add lint3 kint3))
          (setf xx (- x (f2cl-lib:fref tcos-%data% (i) ((1 1)) tcos-%offset%)))
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i m) nil)
            (tagbody
              (setf (f2cl-lib:fref y3-%data% (i) ((1 1)) y3-%offset%)
                      (+
                       (* xx (f2cl-lib:fref y3-%data% (i) ((1 1)) y3-%offset%))
                       (f2cl-lib:fref w3-%data% (i) ((1 1)) w3-%offset%)))
             label114))
          (setf lint3 (f2cl-lib:int-add lint3 1))
          (setf l3 (f2cl-lib:int (/ (* (f2cl-lib:ffloat lint3) f1) f4)))
         label115))
      (go end_label)
     end_label
      (return (values nil nil nil nil nil nil nil nil nil nil nil nil nil)))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::tri3 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo :arg-types
                                            '((fortran-to-lisp::integer4)
                                              (array single-float (*))
                                              (array single-float (*))
                                              (array single-float (*))
                                              (array fortran-to-lisp::integer4
                                               (*))
                                              (array single-float (*))
                                              (array single-float (*))
                                              (array single-float (*))
                                              (array single-float (*))
                                              (array single-float (*))
                                              (array single-float (*))
                                              (array single-float (*))
                                              (array single-float (*)))
                                            :return-values
                                            '(nil nil nil nil nil nil nil nil
                                              nil nil nil nil nil)
                                            :calls 'nil)))

