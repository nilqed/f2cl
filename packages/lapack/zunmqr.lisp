;;; Compiled by f2cl version:
;;; ("" "" "" "" "" "" "")

;;; Using Lisp SBCL 1.3.18
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing t) (:declare-common nil)
;;;           (:float-format single-float))

(in-package "LAPACK")


(let* ((nbmax 64) (ldt (+ nbmax 1)))
  (declare (type (f2cl-lib:integer4 64 64) nbmax)
           (type (f2cl-lib:integer4) ldt)
           (ignorable nbmax ldt))
  (defun zunmqr (side trans m n k a lda tau c ldc work lwork info)
    (declare (type (array f2cl-lib:complex16 (*)) work c tau a)
             (type (f2cl-lib:integer4) info lwork ldc lda k n m)
             (type (string *) trans side))
    (f2cl-lib:with-multi-array-data
        ((side character side-%data% side-%offset%)
         (trans character trans-%data% trans-%offset%)
         (a f2cl-lib:complex16 a-%data% a-%offset%)
         (tau f2cl-lib:complex16 tau-%data% tau-%offset%)
         (c f2cl-lib:complex16 c-%data% c-%offset%)
         (work f2cl-lib:complex16 work-%data% work-%offset%))
      (prog ((i 0) (i1 0) (i2 0) (i3 0) (ib 0) (ic 0) (iinfo 0) (iws 0) (jc 0)
             (ldwork 0) (lwkopt 0) (mi 0) (nb 0) (nbmin 0) (ni 0) (nq 0) (nw 0)
             (left nil) (lquery nil) (notran nil)
             (t$
              (make-array (the fixnum (reduce #'* (list ldt nbmax)))
                          :element-type 'f2cl-lib:complex16)))
        (declare (type (array f2cl-lib:complex16 (*)) t$)
                 (type (f2cl-lib:integer4) i i1 i2 i3 ib ic iinfo iws jc ldwork
                  lwkopt mi nb nbmin ni nq nw)
                 (type f2cl-lib:logical left lquery notran))
        (setf info 0)
        (setf left (lsame side "L"))
        (setf notran (lsame trans "N"))
        (setf lquery (coerce (= lwork -1) 'f2cl-lib:logical))
        (cond (left (setf nq m) (setf nw n)) (t (setf nq n) (setf nw m)))
        (cond ((and (not left) (not (lsame side "R"))) (setf info -1))
              ((and (not notran) (not (lsame trans "C"))) (setf info -2))
              ((< m 0) (setf info -3)) ((< n 0) (setf info -4))
              ((or (< k 0) (> k nq)) (setf info -5))
              ((< lda
                  (max (the f2cl-lib:integer4 1) (the f2cl-lib:integer4 nq)))
               (setf info -7))
              ((< ldc
                  (max (the f2cl-lib:integer4 1) (the f2cl-lib:integer4 m)))
               (setf info -10))
              ((and
                (< lwork
                   (max (the f2cl-lib:integer4 1) (the f2cl-lib:integer4 nw)))
                (not lquery))
               (setf info -12)))
        (cond
         ((= info 0)
          (setf nb
                  (min (the f2cl-lib:integer4 nbmax)
                       (the f2cl-lib:integer4
                            (ilaenv 1 "ZUNMQR" (f2cl-lib:f2cl-// side trans) m
                             n k -1))))
          (setf lwkopt
                  (f2cl-lib:int-mul
                   (max (the f2cl-lib:integer4 1) (the f2cl-lib:integer4 nw))
                   nb))
          (setf (f2cl-lib:fref work-%data% (1) ((1 *)) work-%offset%)
                  (coerce lwkopt 'f2cl-lib:complex16))))
        (cond
         ((/= info 0) (xerbla "ZUNMQR" (f2cl-lib:int-sub info)) (go end_label))
         (lquery (go end_label)))
        (cond
         ((or (= m 0) (= n 0) (= k 0))
          (setf (f2cl-lib:fref work-%data% (1) ((1 *)) work-%offset%)
                  (coerce 1 'f2cl-lib:complex16))
          (go end_label)))
        (setf nbmin 2)
        (setf ldwork nw)
        (cond
         ((and (> nb 1) (< nb k)) (setf iws (f2cl-lib:int-mul nw nb))
          (cond
           ((< lwork iws)
            (setf nb (the f2cl-lib:integer4 (truncate lwork ldwork)))
            (setf nbmin
                    (max (the f2cl-lib:integer4 2)
                         (the f2cl-lib:integer4
                              (ilaenv 2 "ZUNMQR" (f2cl-lib:f2cl-// side trans)
                               m n k -1)))))))
         (t (setf iws nw)))
        (cond
         ((or (< nb nbmin) (>= nb k))
          (multiple-value-bind
              (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9
               var-10 var-11)
              (zunm2r side trans m n k a lda tau c ldc work iinfo)
            (declare (ignore var-5 var-7 var-8 var-10))
            (when var-0 (setf side var-0))
            (when var-1 (setf trans var-1))
            (when var-2 (setf m var-2))
            (when var-3 (setf n var-3))
            (when var-4 (setf k var-4))
            (when var-6 (setf lda var-6))
            (when var-9 (setf ldc var-9))
            (when var-11 (setf iinfo var-11))))
         (t
          (cond
           ((or (and left (not notran)) (and (not left) notran)) (setf i1 1)
            (setf i2 k) (setf i3 nb))
           (t
            (setf i1
                    (+ (* (the f2cl-lib:integer4 (truncate (- k 1) nb)) nb) 1))
            (setf i2 1) (setf i3 (f2cl-lib:int-sub nb))))
          (cond (left (setf ni n) (setf jc 1)) (t (setf mi m) (setf ic 1)))
          (f2cl-lib:fdo (i i1 (f2cl-lib:int-add i i3))
                        ((> i i2) nil)
            (tagbody
              (setf ib
                      (min (the f2cl-lib:integer4 nb)
                           (the f2cl-lib:integer4
                                (f2cl-lib:int-add (f2cl-lib:int-sub k i) 1))))
              (multiple-value-bind
                  (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8)
                  (zlarft "Forward" "Columnwise"
                   (f2cl-lib:int-add (f2cl-lib:int-sub nq i) 1) ib
                   (f2cl-lib:array-slice a-%data% f2cl-lib:complex16 (i i)
                                         ((1 lda) (1 *)) a-%offset%)
                   lda
                   (f2cl-lib:array-slice tau-%data% f2cl-lib:complex16 (i)
                                         ((1 *)) tau-%offset%)
                   t$ ldt)
                (declare (ignore var-0 var-1 var-2 var-4 var-6 var-7))
                (when var-3 (setf ib var-3))
                (when var-5 (setf lda var-5))
                (when var-8 (setf ldt var-8)))
              (cond
               (left (setf mi (f2cl-lib:int-add (f2cl-lib:int-sub m i) 1))
                (setf ic i))
               (t (setf ni (f2cl-lib:int-add (f2cl-lib:int-sub n i) 1))
                (setf jc i)))
              (multiple-value-bind
                  (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9
                   var-10 var-11 var-12 var-13 var-14)
                  (zlarfb side trans "Forward" "Columnwise" mi ni ib
                   (f2cl-lib:array-slice a-%data% f2cl-lib:complex16 (i i)
                                         ((1 lda) (1 *)) a-%offset%)
                   lda t$ ldt
                   (f2cl-lib:array-slice c-%data% f2cl-lib:complex16 (ic jc)
                                         ((1 ldc) (1 *)) c-%offset%)
                   ldc work ldwork)
                (declare (ignore var-2 var-3 var-7 var-9 var-11 var-13))
                (when var-0 (setf side var-0))
                (when var-1 (setf trans var-1))
                (when var-4 (setf mi var-4))
                (when var-5 (setf ni var-5))
                (when var-6 (setf ib var-6))
                (when var-8 (setf lda var-8))
                (when var-10 (setf ldt var-10))
                (when var-12 (setf ldc var-12))
                (when var-14 (setf ldwork var-14)))
             label10))))
        (setf (f2cl-lib:fref work-%data% (1) ((1 *)) work-%offset%)
                (coerce lwkopt 'f2cl-lib:complex16))
        (go end_label)
       end_label
        (return (values side trans m n k nil lda nil nil ldc nil nil info))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::zunmqr
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo :arg-types
                                            '((string) (string)
                                              (fortran-to-lisp::integer4)
                                              (fortran-to-lisp::integer4)
                                              (fortran-to-lisp::integer4)
                                              (array fortran-to-lisp::complex16
                                               (*))
                                              (fortran-to-lisp::integer4)
                                              (array fortran-to-lisp::complex16
                                               (*))
                                              (array fortran-to-lisp::complex16
                                               (*))
                                              (fortran-to-lisp::integer4)
                                              (array fortran-to-lisp::complex16
                                               (*))
                                              (fortran-to-lisp::integer4)
                                              (fortran-to-lisp::integer4))
                                            :return-values
                                            '(fortran-to-lisp::side
                                              fortran-to-lisp::trans
                                              fortran-to-lisp::m
                                              fortran-to-lisp::n
                                              fortran-to-lisp::k nil
                                              fortran-to-lisp::lda nil nil
                                              fortran-to-lisp::ldc nil nil
                                              fortran-to-lisp::info)
                                            :calls
                                            '(fortran-to-lisp::xerbla
                                              fortran-to-lisp::ilaenv
                                              fortran-to-lisp::lsame))))

