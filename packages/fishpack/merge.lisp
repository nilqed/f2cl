;;; Compiled by f2cl version:
;;; ("" "" "" "" "" "" "")

;;; Using Lisp SBCL 1.4.2
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing t) (:declare-common nil)
;;;           (:float-format single-float))

(in-package "FISHPACK")


(defun merge$ (tcos i1 m1 i2 m2 i3)
  (declare (type (f2cl-lib:integer4) i3 m2 i2 m1 i1)
           (type (array single-float (*)) tcos))
  (f2cl-lib:with-multi-array-data
      ((tcos single-float tcos-%data% tcos-%offset%))
    (prog ((m 0) (k 0) (y 0.0) (x 0.0) (l 0) (j 0) (j2 0) (j1 0))
      (declare (type (single-float) x y)
               (type (f2cl-lib:integer4) j1 j2 j l k m))
      (setf j1 1)
      (setf j2 1)
      (setf j i3)
      (if (= m1 0)
          (go label107))
      (if (= m2 0)
          (go label104))
     label101
      (setf j (f2cl-lib:int-add j 1))
      (setf l (f2cl-lib:int-add j1 i1))
      (setf x (f2cl-lib:fref tcos-%data% (l) ((1 1)) tcos-%offset%))
      (setf l (f2cl-lib:int-add j2 i2))
      (setf y (f2cl-lib:fref tcos-%data% (l) ((1 1)) tcos-%offset%))
      (f2cl-lib:arithmetic-if (- x y) (go label102) (go label102)
                              (go label103))
     label102
      (setf (f2cl-lib:fref tcos-%data% (j) ((1 1)) tcos-%offset%) x)
      (setf j1 (f2cl-lib:int-add j1 1))
      (if (> j1 m1)
          (go label106))
      (go label101)
     label103
      (setf (f2cl-lib:fref tcos-%data% (j) ((1 1)) tcos-%offset%) y)
      (setf j2 (f2cl-lib:int-add j2 1))
      (if (<= j2 m2)
          (go label101))
      (if (> j1 m1)
          (go label109))
     label104
      (setf k (f2cl-lib:int-add (f2cl-lib:int-sub j j1) 1))
      (f2cl-lib:fdo (j j1 (f2cl-lib:int-add j 1))
                    ((> j m1) nil)
        (tagbody
          (setf m (f2cl-lib:int-add k j))
          (setf l (f2cl-lib:int-add j i1))
          (setf (f2cl-lib:fref tcos-%data% (m) ((1 1)) tcos-%offset%)
                  (f2cl-lib:fref tcos-%data% (l) ((1 1)) tcos-%offset%))
         label105))
      (go label109)
     label106
      (if (> j2 m2)
          (go label109))
     label107
      (setf k (f2cl-lib:int-add (f2cl-lib:int-sub j j2) 1))
      (f2cl-lib:fdo (j j2 (f2cl-lib:int-add j 1))
                    ((> j m2) nil)
        (tagbody
          (setf m (f2cl-lib:int-add k j))
          (setf l (f2cl-lib:int-add j i2))
          (setf (f2cl-lib:fref tcos-%data% (m) ((1 1)) tcos-%offset%)
                  (f2cl-lib:fref tcos-%data% (l) ((1 1)) tcos-%offset%))
         label108))
     label109
      (go end_label)
     end_label
      (return (values nil nil nil nil nil nil)))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::merge$
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo :arg-types
                                            '((array single-float (*))
                                              (fortran-to-lisp::integer4)
                                              (fortran-to-lisp::integer4)
                                              (fortran-to-lisp::integer4)
                                              (fortran-to-lisp::integer4)
                                              (fortran-to-lisp::integer4))
                                            :return-values
                                            '(nil nil nil nil nil nil) :calls
                                            'nil)))

