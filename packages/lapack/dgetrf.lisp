;;; Compiled by f2cl version:
;;; ("" "" "" "" "" "" "")

;;; Using Lisp SBCL 1.4.2
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing t) (:declare-common nil)
;;;           (:float-format single-float))

(in-package "LAPACK")


(let* ((one 1.0d0))
  (declare (type (double-float 1.0d0 1.0d0) one)
           (ignorable one))
  (defun dgetrf (m n a lda ipiv info)
    (declare (type (array f2cl-lib:integer4 (*)) ipiv)
             (type (array double-float (*)) a)
             (type (f2cl-lib:integer4) info lda n m))
    (f2cl-lib:with-multi-array-data
        ((a double-float a-%data% a-%offset%)
         (ipiv f2cl-lib:integer4 ipiv-%data% ipiv-%offset%))
      (prog ((i 0) (iinfo 0) (j 0) (jb 0) (nb 0))
        (declare (type (f2cl-lib:integer4) i iinfo j jb nb))
        (setf info 0)
        (cond ((< m 0) (setf info -1)) ((< n 0) (setf info -2))
              ((< lda
                  (max (the f2cl-lib:integer4 1) (the f2cl-lib:integer4 m)))
               (setf info -4)))
        (cond
         ((/= info 0) (xerbla "DGETRF" (f2cl-lib:int-sub info))
          (go end_label)))
        (if (or (= m 0) (= n 0))
            (go end_label))
        (setf nb (ilaenv 1 "DGETRF" " " m n -1 -1))
        (cond
         ((or (<= nb 1)
              (>= nb
                  (min (the f2cl-lib:integer4 m) (the f2cl-lib:integer4 n))))
          (multiple-value-bind (var-0 var-1 var-2 var-3 var-4 var-5)
              (dgetf2 m n a lda ipiv info)
            (declare (ignore var-0 var-1 var-2 var-3 var-4))
            (setf info var-5)))
         (t
          (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j nb))
                        ((> j
                            (min (the f2cl-lib:integer4 m)
                                 (the f2cl-lib:integer4 n)))
                         nil)
            (tagbody
              (setf jb
                      (min
                       (the f2cl-lib:integer4
                            (f2cl-lib:int-add
                             (f2cl-lib:int-sub
                              (min (the f2cl-lib:integer4 m)
                                   (the f2cl-lib:integer4 n))
                              j)
                             1))
                       (the f2cl-lib:integer4 nb)))
              (multiple-value-bind (var-0 var-1 var-2 var-3 var-4 var-5)
                  (dgetf2 (f2cl-lib:int-add (f2cl-lib:int-sub m j) 1) jb
                   (f2cl-lib:array-slice a-%data% double-float (j j)
                                         ((1 lda) (1 *)) a-%offset%)
                   lda
                   (f2cl-lib:array-slice ipiv-%data% f2cl-lib:integer4 (j)
                                         ((1 *)) ipiv-%offset%)
                   iinfo)
                (declare (ignore var-0 var-1 var-2 var-3 var-4))
                (setf iinfo var-5))
              (if (and (= info 0) (> iinfo 0))
                  (setf info (f2cl-lib:int-sub (f2cl-lib:int-add iinfo j) 1)))
              (f2cl-lib:fdo (i j (f2cl-lib:int-add i 1))
                            ((> i
                                (min (the f2cl-lib:integer4 m)
                                     (the f2cl-lib:integer4
                                          (f2cl-lib:int-add j jb
                                                            (f2cl-lib:int-sub
                                                             1)))))
                             nil)
                (tagbody
                  (setf (f2cl-lib:fref ipiv-%data% (i) ((1 *)) ipiv-%offset%)
                          (f2cl-lib:int-add (f2cl-lib:int-sub j 1)
                                            (f2cl-lib:fref ipiv-%data% (i)
                                                           ((1 *))
                                                           ipiv-%offset%)))
                 label10))
              (dlaswp (f2cl-lib:int-sub j 1) a lda j
               (f2cl-lib:int-sub (f2cl-lib:int-add j jb) 1) ipiv 1)
              (cond
               ((<= (f2cl-lib:int-add j jb) n)
                (dlaswp (f2cl-lib:int-add (f2cl-lib:int-sub n j jb) 1)
                 (f2cl-lib:array-slice a-%data% double-float
                                       (1 (f2cl-lib:int-add j jb))
                                       ((1 lda) (1 *)) a-%offset%)
                 lda j (f2cl-lib:int-sub (f2cl-lib:int-add j jb) 1) ipiv 1)
                (dtrsm "Left" "Lower" "No transpose" "Unit" jb
                 (f2cl-lib:int-add (f2cl-lib:int-sub n j jb) 1) one
                 (f2cl-lib:array-slice a-%data% double-float (j j)
                                       ((1 lda) (1 *)) a-%offset%)
                 lda
                 (f2cl-lib:array-slice a-%data% double-float
                                       (j (f2cl-lib:int-add j jb))
                                       ((1 lda) (1 *)) a-%offset%)
                 lda)
                (cond
                 ((<= (f2cl-lib:int-add j jb) m)
                  (dgemm "No transpose" "No transpose"
                   (f2cl-lib:int-add (f2cl-lib:int-sub m j jb) 1)
                   (f2cl-lib:int-add (f2cl-lib:int-sub n j jb) 1) jb (- one)
                   (f2cl-lib:array-slice a-%data% double-float ((+ j jb) j)
                                         ((1 lda) (1 *)) a-%offset%)
                   lda
                   (f2cl-lib:array-slice a-%data% double-float
                                         (j (f2cl-lib:int-add j jb))
                                         ((1 lda) (1 *)) a-%offset%)
                   lda one
                   (f2cl-lib:array-slice a-%data% double-float
                                         ((+ j jb) (f2cl-lib:int-add j jb))
                                         ((1 lda) (1 *)) a-%offset%)
                   lda)))))
             label20))))
        (go end_label)
       end_label
        (return (values nil nil nil nil nil info))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dgetrf
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo :arg-types
                                            '((fortran-to-lisp::integer4)
                                              (fortran-to-lisp::integer4)
                                              (array double-float (*))
                                              (fortran-to-lisp::integer4)
                                              (array fortran-to-lisp::integer4
                                               (*))
                                              (fortran-to-lisp::integer4))
                                            :return-values
                                            '(nil nil nil nil nil
                                              fortran-to-lisp::info)
                                            :calls
                                            '(fortran-to-lisp::dgemm
                                              fortran-to-lisp::dtrsm
                                              fortran-to-lisp::dlaswp
                                              fortran-to-lisp::dgetf2
                                              fortran-to-lisp::ilaenv
                                              fortran-to-lisp::xerbla))))

