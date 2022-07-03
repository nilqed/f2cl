;;; Compiled by f2cl version:
;;; ("" "" "" "" "" "" "")

;;; Using Lisp SBCL 1.4.2
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing t) (:declare-common nil)
;;;           (:float-format single-float))

(in-package "FISHPACK")


(defun genbun (nperod n mperod m a b c idimy y ierror w)
  (declare (type (array single-float (*)) y)
           (type (array single-float (*)) w c b a)
           (type (f2cl-lib:integer4) ierror idimy m mperod n nperod))
  (f2cl-lib:with-multi-array-data
      ((a single-float a-%data% a-%offset%)
       (b single-float b-%data% b-%offset%)
       (c single-float c-%data% c-%offset%)
       (w single-float w-%data% w-%offset%)
       (y single-float y-%data% y-%offset%))
    (prog ((a1 0.0) (mskip 0) (nby2 0) (mhmi 0) (mhpi 0) (modd 0) (mhm1 0)
           (mh 0) (irev 0) (ipstor 0) (np 0) (mp 0) (j 0) (k 0) (iwp 0)
           (iwtcos 0) (iwd 0) (iww3 0) (iww2 0) (iww1 0) (iwb3 0) (iwb2 0)
           (iwbc 0) (iwbb 0) (iwba 0) (mp1 0) (i 0))
      (declare
       (type (f2cl-lib:integer4) i mp1 iwba iwbb iwbc iwb2 iwb3 iww1 iww2 iww3
        iwd iwtcos iwp k j mp np ipstor irev mh mhm1 modd mhpi mhmi nby2 mskip)
       (type (single-float) a1))
      (setf ierror 0)
      (if (<= m 2)
          (setf ierror 1))
      (if (<= n 2)
          (setf ierror 2))
      (if (< idimy m)
          (setf ierror 3))
      (if (or (< nperod 0) (> nperod 4))
          (setf ierror 4))
      (if (or (< mperod 0) (> mperod 1))
          (setf ierror 5))
      (if (= mperod 1)
          (go label102))
      (f2cl-lib:fdo (i 2 (f2cl-lib:int-add i 1))
                    ((> i m) nil)
        (tagbody
          (if (/= (f2cl-lib:fref a-%data% (i) ((1 1)) a-%offset%)
                  (f2cl-lib:fref c-%data% (1) ((1 1)) c-%offset%))
              (go label103))
          (if (/= (f2cl-lib:fref c-%data% (i) ((1 1)) c-%offset%)
                  (f2cl-lib:fref c-%data% (1) ((1 1)) c-%offset%))
              (go label103))
          (if (/= (f2cl-lib:fref b-%data% (i) ((1 1)) b-%offset%)
                  (f2cl-lib:fref b-%data% (1) ((1 1)) b-%offset%))
              (go label103))
         label101))
      (go label104)
     label102
      (if (or (/= (f2cl-lib:fref a-%data% (1) ((1 1)) a-%offset%) 0.0)
              (/= (f2cl-lib:fref c-%data% (m) ((1 1)) c-%offset%) 0.0))
          (setf ierror 7))
      (go label104)
     label103
      (setf ierror 6)
     label104
      (if (/= ierror 0)
          (go end_label))
      (setf mp1 (f2cl-lib:int-add m 1))
      (setf iwba mp1)
      (setf iwbb (f2cl-lib:int-add iwba m))
      (setf iwbc (f2cl-lib:int-add iwbb m))
      (setf iwb2 (f2cl-lib:int-add iwbc m))
      (setf iwb3 (f2cl-lib:int-add iwb2 m))
      (setf iww1 (f2cl-lib:int-add iwb3 m))
      (setf iww2 (f2cl-lib:int-add iww1 m))
      (setf iww3 (f2cl-lib:int-add iww2 m))
      (setf iwd (f2cl-lib:int-add iww3 m))
      (setf iwtcos (f2cl-lib:int-add iwd m))
      (setf iwp (f2cl-lib:int-add iwtcos (f2cl-lib:int-mul 4 n)))
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i m) nil)
        (tagbody
          (setf k (f2cl-lib:int-sub (f2cl-lib:int-add iwba i) 1))
          (setf (f2cl-lib:fref w-%data% (k) ((1 1)) w-%offset%)
                  (- (f2cl-lib:fref a-%data% (i) ((1 1)) a-%offset%)))
          (setf k (f2cl-lib:int-sub (f2cl-lib:int-add iwbc i) 1))
          (setf (f2cl-lib:fref w-%data% (k) ((1 1)) w-%offset%)
                  (- (f2cl-lib:fref c-%data% (i) ((1 1)) c-%offset%)))
          (setf k (f2cl-lib:int-sub (f2cl-lib:int-add iwbb i) 1))
          (setf (f2cl-lib:fref w-%data% (k) ((1 1)) w-%offset%)
                  (- 2.0 (f2cl-lib:fref b-%data% (i) ((1 1)) b-%offset%)))
          (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                        ((> j n) nil)
            (tagbody
              (setf (f2cl-lib:fref y-%data% (i j) ((1 idimy) (1 1)) y-%offset%)
                      (-
                       (f2cl-lib:fref y-%data% (i j) ((1 idimy) (1 1))
                                      y-%offset%)))
             label105))
         label106))
      (setf mp (f2cl-lib:int-add mperod 1))
      (setf np (f2cl-lib:int-add nperod 1))
      (f2cl-lib:computed-goto (label114 label107) mp)
     label107
      (f2cl-lib:computed-goto (label108 label109 label110 label111 label123)
                              np)
     label108
      (multiple-value-bind
          (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9 var-10
           var-11 var-12 var-13 var-14 var-15)
          (poisp2 m n
           (f2cl-lib:array-slice w-%data% single-float (iwba) ((1 1))
                                 w-%offset%)
           (f2cl-lib:array-slice w-%data% single-float (iwbb) ((1 1))
                                 w-%offset%)
           (f2cl-lib:array-slice w-%data% single-float (iwbc) ((1 1))
                                 w-%offset%)
           y idimy w
           (f2cl-lib:array-slice w-%data% single-float (iwb2) ((1 1))
                                 w-%offset%)
           (f2cl-lib:array-slice w-%data% single-float (iwb3) ((1 1))
                                 w-%offset%)
           (f2cl-lib:array-slice w-%data% single-float (iww1) ((1 1))
                                 w-%offset%)
           (f2cl-lib:array-slice w-%data% single-float (iww2) ((1 1))
                                 w-%offset%)
           (f2cl-lib:array-slice w-%data% single-float (iww3) ((1 1))
                                 w-%offset%)
           (f2cl-lib:array-slice w-%data% single-float (iwd) ((1 1))
                                 w-%offset%)
           (f2cl-lib:array-slice w-%data% single-float (iwtcos) ((1 1))
                                 w-%offset%)
           (f2cl-lib:array-slice w-%data% single-float (iwp) ((1 1))
                                 w-%offset%))
        (declare
         (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-7 var-8 var-9 var-10
          var-11 var-12 var-13 var-14 var-15))
        (setf idimy var-6))
      (go label112)
     label109
      (poisd2 m n 1
       (f2cl-lib:array-slice w-%data% single-float (iwba) ((1 1)) w-%offset%)
       (f2cl-lib:array-slice w-%data% single-float (iwbb) ((1 1)) w-%offset%)
       (f2cl-lib:array-slice w-%data% single-float (iwbc) ((1 1)) w-%offset%) y
       idimy w
       (f2cl-lib:array-slice w-%data% single-float (iww1) ((1 1)) w-%offset%)
       (f2cl-lib:array-slice w-%data% single-float (iwd) ((1 1)) w-%offset%)
       (f2cl-lib:array-slice w-%data% single-float (iwtcos) ((1 1)) w-%offset%)
       (f2cl-lib:array-slice w-%data% single-float (iwp) ((1 1)) w-%offset%))
      (go label112)
     label110
      (poisn2 m n 1 2
       (f2cl-lib:array-slice w-%data% single-float (iwba) ((1 1)) w-%offset%)
       (f2cl-lib:array-slice w-%data% single-float (iwbb) ((1 1)) w-%offset%)
       (f2cl-lib:array-slice w-%data% single-float (iwbc) ((1 1)) w-%offset%) y
       idimy w
       (f2cl-lib:array-slice w-%data% single-float (iwb2) ((1 1)) w-%offset%)
       (f2cl-lib:array-slice w-%data% single-float (iwb3) ((1 1)) w-%offset%)
       (f2cl-lib:array-slice w-%data% single-float (iww1) ((1 1)) w-%offset%)
       (f2cl-lib:array-slice w-%data% single-float (iww2) ((1 1)) w-%offset%)
       (f2cl-lib:array-slice w-%data% single-float (iww3) ((1 1)) w-%offset%)
       (f2cl-lib:array-slice w-%data% single-float (iwd) ((1 1)) w-%offset%)
       (f2cl-lib:array-slice w-%data% single-float (iwtcos) ((1 1)) w-%offset%)
       (f2cl-lib:array-slice w-%data% single-float (iwp) ((1 1)) w-%offset%))
      (go label112)
     label111
      (poisn2 m n 1 1
       (f2cl-lib:array-slice w-%data% single-float (iwba) ((1 1)) w-%offset%)
       (f2cl-lib:array-slice w-%data% single-float (iwbb) ((1 1)) w-%offset%)
       (f2cl-lib:array-slice w-%data% single-float (iwbc) ((1 1)) w-%offset%) y
       idimy w
       (f2cl-lib:array-slice w-%data% single-float (iwb2) ((1 1)) w-%offset%)
       (f2cl-lib:array-slice w-%data% single-float (iwb3) ((1 1)) w-%offset%)
       (f2cl-lib:array-slice w-%data% single-float (iww1) ((1 1)) w-%offset%)
       (f2cl-lib:array-slice w-%data% single-float (iww2) ((1 1)) w-%offset%)
       (f2cl-lib:array-slice w-%data% single-float (iww3) ((1 1)) w-%offset%)
       (f2cl-lib:array-slice w-%data% single-float (iwd) ((1 1)) w-%offset%)
       (f2cl-lib:array-slice w-%data% single-float (iwtcos) ((1 1)) w-%offset%)
       (f2cl-lib:array-slice w-%data% single-float (iwp) ((1 1)) w-%offset%))
     label112
      (setf ipstor
              (f2cl-lib:int
               (f2cl-lib:fref w-%data% (iww1) ((1 1)) w-%offset%)))
      (setf irev 2)
      (if (= nperod 4)
          (go label124))
     label113
      (f2cl-lib:computed-goto (label127 label133) mp)
     label114
      (setf mh (the f2cl-lib:integer4 (truncate (+ m 1) 2)))
      (setf mhm1 (f2cl-lib:int-sub mh 1))
      (setf modd 1)
      (if (= (f2cl-lib:int-mul mh 2) m)
          (setf modd 2))
      (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                    ((> j n) nil)
        (tagbody
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i mhm1) nil)
            (tagbody
              (setf mhpi (f2cl-lib:int-add mh i))
              (setf mhmi (f2cl-lib:int-sub mh i))
              (setf (f2cl-lib:fref w-%data% (i) ((1 1)) w-%offset%)
                      (-
                       (f2cl-lib:fref y-%data% (mhmi j) ((1 idimy) (1 1))
                                      y-%offset%)
                       (f2cl-lib:fref y-%data% (mhpi j) ((1 idimy) (1 1))
                                      y-%offset%)))
              (setf (f2cl-lib:fref w-%data% (mhpi) ((1 1)) w-%offset%)
                      (+
                       (f2cl-lib:fref y-%data% (mhmi j) ((1 idimy) (1 1))
                                      y-%offset%)
                       (f2cl-lib:fref y-%data% (mhpi j) ((1 idimy) (1 1))
                                      y-%offset%)))
             label115))
          (setf (f2cl-lib:fref w-%data% (mh) ((1 1)) w-%offset%)
                  (* 2.0
                     (f2cl-lib:fref y-%data% (mh j) ((1 idimy) (1 1))
                                    y-%offset%)))
          (f2cl-lib:computed-goto (label117 label116) modd)
         label116
          (setf (f2cl-lib:fref w-%data% (m) ((1 1)) w-%offset%)
                  (* 2.0
                     (f2cl-lib:fref y-%data% (m j) ((1 idimy) (1 1))
                                    y-%offset%)))
         label117
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i m) nil)
            (tagbody
              (setf (f2cl-lib:fref y-%data% (i j) ((1 idimy) (1 1)) y-%offset%)
                      (f2cl-lib:fref w-%data% (i) ((1 1)) w-%offset%))
             label118))
         label119))
      (setf k (f2cl-lib:int-sub (f2cl-lib:int-add iwbc mhm1) 1))
      (setf i (f2cl-lib:int-add iwba mhm1))
      (setf (f2cl-lib:fref w-%data% (k) ((1 1)) w-%offset%) 0.0)
      (setf (f2cl-lib:fref w-%data% (i) ((1 1)) w-%offset%) 0.0)
      (setf (f2cl-lib:fref w-%data% ((f2cl-lib:int-add k 1)) ((1 1))
                           w-%offset%)
              (* 2.0
                 (f2cl-lib:fref w-%data% ((f2cl-lib:int-add k 1)) ((1 1))
                                w-%offset%)))
      (f2cl-lib:computed-goto (label120 label121) modd)
     label120
      (setf k (f2cl-lib:int-sub (f2cl-lib:int-add iwbb mhm1) 1))
      (setf (f2cl-lib:fref w-%data% (k) ((1 1)) w-%offset%)
              (- (f2cl-lib:fref w-%data% (k) ((1 1)) w-%offset%)
                 (f2cl-lib:fref w-%data% ((f2cl-lib:int-sub i 1)) ((1 1))
                                w-%offset%)))
      (setf (f2cl-lib:fref w-%data% ((f2cl-lib:int-sub iwbc 1)) ((1 1))
                           w-%offset%)
              (+
               (f2cl-lib:fref w-%data% ((f2cl-lib:int-sub iwbc 1)) ((1 1))
                              w-%offset%)
               (f2cl-lib:fref w-%data% ((f2cl-lib:int-sub iwbb 1)) ((1 1))
                              w-%offset%)))
      (go label122)
     label121
      (setf (f2cl-lib:fref w-%data% ((f2cl-lib:int-sub iwbb 1)) ((1 1))
                           w-%offset%)
              (f2cl-lib:fref w-%data% ((f2cl-lib:int-add k 1)) ((1 1))
                             w-%offset%))
     label122
      (go label107)
     label123
      (setf irev 1)
      (setf nby2 (the f2cl-lib:integer4 (truncate n 2)))
     label124
      (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                    ((> j nby2) nil)
        (tagbody
          (setf mskip (f2cl-lib:int-sub (f2cl-lib:int-add n 1) j))
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i m) nil)
            (tagbody
              (setf a1
                      (f2cl-lib:fref y-%data% (i j) ((1 idimy) (1 1))
                                     y-%offset%))
              (setf (f2cl-lib:fref y-%data% (i j) ((1 idimy) (1 1)) y-%offset%)
                      (f2cl-lib:fref y-%data% (i mskip) ((1 idimy) (1 1))
                                     y-%offset%))
              (setf (f2cl-lib:fref y-%data% (i mskip) ((1 idimy) (1 1))
                                   y-%offset%)
                      a1)
             label125))
         label126))
      (f2cl-lib:computed-goto (label110 label113) irev)
     label127
      (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                    ((> j n) nil)
        (tagbody
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i mhm1) nil)
            (tagbody
              (setf mhmi (f2cl-lib:int-sub mh i))
              (setf mhpi (f2cl-lib:int-add mh i))
              (setf (f2cl-lib:fref w-%data% (mhmi) ((1 1)) w-%offset%)
                      (* 0.5
                         (+
                          (f2cl-lib:fref y-%data% (mhpi j) ((1 idimy) (1 1))
                                         y-%offset%)
                          (f2cl-lib:fref y-%data% (i j) ((1 idimy) (1 1))
                                         y-%offset%))))
              (setf (f2cl-lib:fref w-%data% (mhpi) ((1 1)) w-%offset%)
                      (* 0.5
                         (-
                          (f2cl-lib:fref y-%data% (mhpi j) ((1 idimy) (1 1))
                                         y-%offset%)
                          (f2cl-lib:fref y-%data% (i j) ((1 idimy) (1 1))
                                         y-%offset%))))
             label128))
          (setf (f2cl-lib:fref w-%data% (mh) ((1 1)) w-%offset%)
                  (* 0.5
                     (f2cl-lib:fref y-%data% (mh j) ((1 idimy) (1 1))
                                    y-%offset%)))
          (f2cl-lib:computed-goto (label130 label129) modd)
         label129
          (setf (f2cl-lib:fref w-%data% (m) ((1 1)) w-%offset%)
                  (* 0.5
                     (f2cl-lib:fref y-%data% (m j) ((1 idimy) (1 1))
                                    y-%offset%)))
         label130
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i m) nil)
            (tagbody
              (setf (f2cl-lib:fref y-%data% (i j) ((1 idimy) (1 1)) y-%offset%)
                      (f2cl-lib:fref w-%data% (i) ((1 1)) w-%offset%))
             label131))
         label132))
     label133
      (setf (f2cl-lib:fref w-%data% (1) ((1 1)) w-%offset%)
              (coerce
               (the f2cl-lib:integer4
                    (f2cl-lib:int-sub (f2cl-lib:int-add ipstor iwp) 1))
               'single-float))
      (go end_label)
     end_label
      (return (values nil nil nil nil nil nil nil idimy nil ierror nil)))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::genbun
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo :arg-types
                                            '((fortran-to-lisp::integer4)
                                              (fortran-to-lisp::integer4)
                                              (fortran-to-lisp::integer4)
                                              (fortran-to-lisp::integer4)
                                              (array single-float (*))
                                              (array single-float (*))
                                              (array single-float (*))
                                              (fortran-to-lisp::integer4)
                                              (array single-float (*))
                                              (fortran-to-lisp::integer4)
                                              (array single-float (*)))
                                            :return-values
                                            '(nil nil nil nil nil nil nil
                                              fortran-to-lisp::idimy nil
                                              fortran-to-lisp::ierror nil)
                                            :calls
                                            '(fortran-to-lisp::poisn2
                                              fortran-to-lisp::poisd2
                                              fortran-to-lisp::poisp2))))

