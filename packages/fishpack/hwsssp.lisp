;;; Compiled by f2cl version:
;;; ("" "" "" "" "" "" "")

;;; Using Lisp SBCL 1.4.2
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing t) (:declare-common nil)
;;;           (:float-format single-float))

(in-package "FISHPACK")


(defun hwsssp
       (ts tf m mbdcnd bdts bdtf ps pf n nbdcnd bdps bdpf elmbda f idimf pertrb
        ierror w)
  (declare (type (array single-float (*)) f)
           (type (array single-float (*)) w bdpf bdps bdtf bdts)
           (type (f2cl-lib:integer4) ierror idimf nbdcnd n mbdcnd m)
           (type (single-float) pertrb elmbda pf ps tf ts))
  (f2cl-lib:with-multi-array-data
      ((bdts single-float bdts-%data% bdts-%offset%)
       (bdtf single-float bdtf-%data% bdtf-%offset%)
       (bdps single-float bdps-%data% bdps-%offset%)
       (bdpf single-float bdpf-%data% bdpf-%offset%)
       (w single-float w-%data% w-%offset%)
       (f single-float f-%data% f-%offset%))
    (prog ((tpi 0.0) (dum 0.0) (pi$ 0.0) (nbr 0))
      (declare (type (f2cl-lib:integer4) nbr)
               (type (single-float) pi$ dum tpi))
      (setf nbr (f2cl-lib:int-add nbdcnd 1))
      (setf pi$ (pimach dum))
      (setf tpi (* 2.0 pi$))
      (setf ierror 0)
      (if (or (< ts 0.0) (> tf pi$))
          (setf ierror 1))
      (if (>= ts tf)
          (setf ierror 2))
      (if (or (< mbdcnd 1) (> mbdcnd 9))
          (setf ierror 3))
      (if (or (< ps 0.0) (> pf tpi))
          (setf ierror 4))
      (if (>= ps pf)
          (setf ierror 5))
      (if (< n 5)
          (setf ierror 6))
      (if (< m 5)
          (setf ierror 7))
      (if (or (< nbdcnd 0) (> nbdcnd 4))
          (setf ierror 8))
      (if (> elmbda 0.0)
          (setf ierror 9))
      (if (< idimf (f2cl-lib:int-add m 1))
          (setf ierror 10))
      (if (and (or (= nbdcnd 1) (= nbdcnd 2) (= nbdcnd 4)) (>= mbdcnd 5))
          (setf ierror 11))
      (if (and (= ts 0.0) (or (= mbdcnd 3) (= mbdcnd 4) (= mbdcnd 8)))
          (setf ierror 12))
      (if (and (= tf pi$) (or (= mbdcnd 2) (= mbdcnd 3) (= mbdcnd 6)))
          (setf ierror 13))
      (if (and (or (= mbdcnd 5) (= mbdcnd 6) (= mbdcnd 9)) (/= ts 0.0))
          (setf ierror 14))
      (if (and (>= mbdcnd 7) (/= tf pi$))
          (setf ierror 15))
      (if (and (/= ierror 0) (/= ierror 9))
          (go end_label))
      (multiple-value-bind
          (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9 var-10
           var-11 var-12 var-13 var-14 var-15 var-16 var-17 var-18 var-19
           var-20 var-21 var-22)
          (hwsss1 ts tf m mbdcnd bdts bdtf ps pf n nbdcnd bdps bdpf elmbda f
           idimf pertrb w
           (f2cl-lib:array-slice w-%data% single-float ((+ m 2)) ((1 1))
                                 w-%offset%)
           (f2cl-lib:array-slice w-%data% single-float
                                 ((+ (f2cl-lib:int-mul 2 m) 3)) ((1 1))
                                 w-%offset%)
           (f2cl-lib:array-slice w-%data% single-float
                                 ((+ (f2cl-lib:int-mul 3 m) 4)) ((1 1))
                                 w-%offset%)
           (f2cl-lib:array-slice w-%data% single-float
                                 ((+ (f2cl-lib:int-mul 4 m) 5)) ((1 1))
                                 w-%offset%)
           (f2cl-lib:array-slice w-%data% single-float
                                 ((+ (f2cl-lib:int-mul 5 m) 6)) ((1 1))
                                 w-%offset%)
           (f2cl-lib:array-slice w-%data% single-float
                                 ((+ (f2cl-lib:int-mul 6 m) 7)) ((1 1))
                                 w-%offset%))
        (declare
         (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9
          var-10 var-11 var-12 var-13 var-16 var-17 var-18 var-19 var-20 var-21
          var-22))
        (setf idimf var-14)
        (setf pertrb var-15))
      (setf (f2cl-lib:fref w-%data% (1) ((1 1)) w-%offset%)
              (+
               (f2cl-lib:fref w-%data%
                              ((f2cl-lib:int-add (f2cl-lib:int-mul 6 m) 7))
                              ((1 1)) w-%offset%)
               (f2cl-lib:ffloat (f2cl-lib:int-mul 6 (f2cl-lib:int-add m 1)))))
      (go end_label)
     end_label
      (return
       (values nil nil nil nil nil nil nil nil nil nil nil nil nil nil idimf
               pertrb ierror nil)))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::hwsssp
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo :arg-types
                                            '((single-float) (single-float)
                                              (fortran-to-lisp::integer4)
                                              (fortran-to-lisp::integer4)
                                              (array single-float (*))
                                              (array single-float (*))
                                              (single-float) (single-float)
                                              (fortran-to-lisp::integer4)
                                              (fortran-to-lisp::integer4)
                                              (array single-float (*))
                                              (array single-float (*))
                                              (single-float)
                                              (array single-float (*))
                                              (fortran-to-lisp::integer4)
                                              (single-float)
                                              (fortran-to-lisp::integer4)
                                              (array single-float (*)))
                                            :return-values
                                            '(nil nil nil nil nil nil nil nil
                                              nil nil nil nil nil nil
                                              fortran-to-lisp::idimf
                                              fortran-to-lisp::pertrb
                                              fortran-to-lisp::ierror nil)
                                            :calls
                                            '(fortran-to-lisp::hwsss1
                                              fortran-to-lisp::pimach))))

