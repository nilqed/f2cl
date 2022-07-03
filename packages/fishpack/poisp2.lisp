;;; Compiled by f2cl version:
;;; ("" "" "" "" "" "" "")

;;; Using Lisp SBCL 1.4.2
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing t) (:declare-common nil)
;;;           (:float-format single-float))

(in-package "FISHPACK")


(defun poisp2 (m n a bb c q idimq b b2 b3 w w2 w3 d tcos p)
  (declare (type (array single-float (*)) q)
           (type (array single-float (*)) p tcos d w3 w2 w b3 b2 b c bb a)
           (type (f2cl-lib:integer4) idimq n m))
  (f2cl-lib:with-multi-array-data
      ((a single-float a-%data% a-%offset%)
       (bb single-float bb-%data% bb-%offset%)
       (c single-float c-%data% c-%offset%)
       (b single-float b-%data% b-%offset%)
       (b2 single-float b2-%data% b2-%offset%)
       (b3 single-float b3-%data% b3-%offset%)
       (w single-float w-%data% w-%offset%)
       (w2 single-float w2-%data% w2-%offset%)
       (w3 single-float w3-%data% w3-%offset%)
       (d single-float d-%data% d-%offset%)
       (tcos single-float tcos-%data% tcos-%offset%)
       (p single-float p-%data% p-%offset%)
       (q single-float q-%data% q-%offset%))
    (prog ((lh 0) (ipstor 0) (t$ 0.0) (s 0.0) (i 0) (nrpj 0) (nrmj 0) (j 0)
           (nrm1 0) (nr 0) (mr 0))
      (declare (type (single-float) s t$)
               (type (f2cl-lib:integer4) mr nr nrm1 j nrmj nrpj i ipstor lh))
      (setf mr m)
      (setf nr (the f2cl-lib:integer4 (truncate (+ n 1) 2)))
      (setf nrm1 (f2cl-lib:int-sub nr 1))
      (if (/= (f2cl-lib:int-mul 2 nr) n)
          (go label107))
      (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                    ((> j nrm1) nil)
        (tagbody
          (setf nrmj (f2cl-lib:int-sub nr j))
          (setf nrpj (f2cl-lib:int-add nr j))
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i mr) nil)
            (tagbody
              (setf s
                      (-
                       (f2cl-lib:fref q-%data% (i nrmj) ((1 idimq) (1 1))
                                      q-%offset%)
                       (f2cl-lib:fref q-%data% (i nrpj) ((1 idimq) (1 1))
                                      q-%offset%)))
              (setf t$
                      (+
                       (f2cl-lib:fref q-%data% (i nrmj) ((1 idimq) (1 1))
                                      q-%offset%)
                       (f2cl-lib:fref q-%data% (i nrpj) ((1 idimq) (1 1))
                                      q-%offset%)))
              (setf (f2cl-lib:fref q-%data% (i nrmj) ((1 idimq) (1 1))
                                   q-%offset%)
                      s)
              (setf (f2cl-lib:fref q-%data% (i nrpj) ((1 idimq) (1 1))
                                   q-%offset%)
                      t$)
             label101))
         label102))
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i mr) nil)
        (tagbody
          (setf (f2cl-lib:fref q-%data% (i nr) ((1 idimq) (1 1)) q-%offset%)
                  (* 2.0
                     (f2cl-lib:fref q-%data% (i nr) ((1 idimq) (1 1))
                                    q-%offset%)))
          (setf (f2cl-lib:fref q-%data% (i n) ((1 idimq) (1 1)) q-%offset%)
                  (* 2.0
                     (f2cl-lib:fref q-%data% (i n) ((1 idimq) (1 1))
                                    q-%offset%)))
         label103))
      (poisd2 mr nrm1 1 a bb c q idimq b w d tcos p)
      (setf ipstor
              (f2cl-lib:int (f2cl-lib:fref w-%data% (1) ((1 1)) w-%offset%)))
      (multiple-value-bind
          (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9 var-10
           var-11 var-12 var-13 var-14 var-15 var-16 var-17)
          (poisn2 mr (f2cl-lib:int-add nr 1) 1 1 a bb c
           (f2cl-lib:array-slice q-%data% single-float (1 nr) ((1 idimq) (1 1))
                                 q-%offset%)
           idimq b b2 b3 w w2 w3 d tcos p)
        (declare
         (ignore var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-9 var-10 var-11
          var-12 var-13 var-14 var-15 var-16 var-17))
        (when var-0 (setf mr var-0))
        (when var-8 (setf idimq var-8)))
      (setf ipstor
              (f2cl-lib:max0 ipstor
                             (f2cl-lib:int
                              (f2cl-lib:fref w-%data% (1) ((1 1))
                                             w-%offset%))))
      (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                    ((> j nrm1) nil)
        (tagbody
          (setf nrmj (f2cl-lib:int-sub nr j))
          (setf nrpj (f2cl-lib:int-add nr j))
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i mr) nil)
            (tagbody
              (setf s
                      (* 0.5
                         (+
                          (f2cl-lib:fref q-%data% (i nrpj) ((1 idimq) (1 1))
                                         q-%offset%)
                          (f2cl-lib:fref q-%data% (i nrmj) ((1 idimq) (1 1))
                                         q-%offset%))))
              (setf t$
                      (* 0.5
                         (-
                          (f2cl-lib:fref q-%data% (i nrpj) ((1 idimq) (1 1))
                                         q-%offset%)
                          (f2cl-lib:fref q-%data% (i nrmj) ((1 idimq) (1 1))
                                         q-%offset%))))
              (setf (f2cl-lib:fref q-%data% (i nrmj) ((1 idimq) (1 1))
                                   q-%offset%)
                      s)
              (setf (f2cl-lib:fref q-%data% (i nrpj) ((1 idimq) (1 1))
                                   q-%offset%)
                      t$)
             label104))
         label105))
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i mr) nil)
        (tagbody
          (setf (f2cl-lib:fref q-%data% (i nr) ((1 idimq) (1 1)) q-%offset%)
                  (* 0.5
                     (f2cl-lib:fref q-%data% (i nr) ((1 idimq) (1 1))
                                    q-%offset%)))
          (setf (f2cl-lib:fref q-%data% (i n) ((1 idimq) (1 1)) q-%offset%)
                  (* 0.5
                     (f2cl-lib:fref q-%data% (i n) ((1 idimq) (1 1))
                                    q-%offset%)))
         label106))
      (go label118)
     label107
      (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                    ((> j nrm1) nil)
        (tagbody
          (setf nrpj (f2cl-lib:int-sub (f2cl-lib:int-add n 1) j))
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i mr) nil)
            (tagbody
              (setf s
                      (-
                       (f2cl-lib:fref q-%data% (i j) ((1 idimq) (1 1))
                                      q-%offset%)
                       (f2cl-lib:fref q-%data% (i nrpj) ((1 idimq) (1 1))
                                      q-%offset%)))
              (setf t$
                      (+
                       (f2cl-lib:fref q-%data% (i j) ((1 idimq) (1 1))
                                      q-%offset%)
                       (f2cl-lib:fref q-%data% (i nrpj) ((1 idimq) (1 1))
                                      q-%offset%)))
              (setf (f2cl-lib:fref q-%data% (i j) ((1 idimq) (1 1)) q-%offset%)
                      s)
              (setf (f2cl-lib:fref q-%data% (i nrpj) ((1 idimq) (1 1))
                                   q-%offset%)
                      t$)
             label108))
         label109))
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i mr) nil)
        (tagbody
          (setf (f2cl-lib:fref q-%data% (i nr) ((1 idimq) (1 1)) q-%offset%)
                  (* 2.0
                     (f2cl-lib:fref q-%data% (i nr) ((1 idimq) (1 1))
                                    q-%offset%)))
         label110))
      (setf lh (the f2cl-lib:integer4 (truncate nrm1 2)))
      (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                    ((> j lh) nil)
        (tagbody
          (setf nrmj (f2cl-lib:int-sub nr j))
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i mr) nil)
            (tagbody
              (setf s
                      (f2cl-lib:fref q-%data% (i j) ((1 idimq) (1 1))
                                     q-%offset%))
              (setf (f2cl-lib:fref q-%data% (i j) ((1 idimq) (1 1)) q-%offset%)
                      (f2cl-lib:fref q-%data% (i nrmj) ((1 idimq) (1 1))
                                     q-%offset%))
              (setf (f2cl-lib:fref q-%data% (i nrmj) ((1 idimq) (1 1))
                                   q-%offset%)
                      s)
             label111))
         label112))
      (poisd2 mr nrm1 2 a bb c q idimq b w d tcos p)
      (setf ipstor
              (f2cl-lib:int (f2cl-lib:fref w-%data% (1) ((1 1)) w-%offset%)))
      (multiple-value-bind
          (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9 var-10
           var-11 var-12 var-13 var-14 var-15 var-16 var-17)
          (poisn2 mr nr 2 1 a bb c
           (f2cl-lib:array-slice q-%data% single-float (1 nr) ((1 idimq) (1 1))
                                 q-%offset%)
           idimq b b2 b3 w w2 w3 d tcos p)
        (declare
         (ignore var-2 var-3 var-4 var-5 var-6 var-7 var-9 var-10 var-11 var-12
          var-13 var-14 var-15 var-16 var-17))
        (when var-0 (setf mr var-0))
        (when var-1 (setf nr var-1))
        (when var-8 (setf idimq var-8)))
      (setf ipstor
              (f2cl-lib:max0 ipstor
                             (f2cl-lib:int
                              (f2cl-lib:fref w-%data% (1) ((1 1))
                                             w-%offset%))))
      (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                    ((> j nrm1) nil)
        (tagbody
          (setf nrpj (f2cl-lib:int-add nr j))
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i mr) nil)
            (tagbody
              (setf s
                      (* 0.5
                         (+
                          (f2cl-lib:fref q-%data% (i nrpj) ((1 idimq) (1 1))
                                         q-%offset%)
                          (f2cl-lib:fref q-%data% (i j) ((1 idimq) (1 1))
                                         q-%offset%))))
              (setf t$
                      (* 0.5
                         (-
                          (f2cl-lib:fref q-%data% (i nrpj) ((1 idimq) (1 1))
                                         q-%offset%)
                          (f2cl-lib:fref q-%data% (i j) ((1 idimq) (1 1))
                                         q-%offset%))))
              (setf (f2cl-lib:fref q-%data% (i nrpj) ((1 idimq) (1 1))
                                   q-%offset%)
                      t$)
              (setf (f2cl-lib:fref q-%data% (i j) ((1 idimq) (1 1)) q-%offset%)
                      s)
             label113))
         label114))
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i mr) nil)
        (tagbody
          (setf (f2cl-lib:fref q-%data% (i nr) ((1 idimq) (1 1)) q-%offset%)
                  (* 0.5
                     (f2cl-lib:fref q-%data% (i nr) ((1 idimq) (1 1))
                                    q-%offset%)))
         label115))
      (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                    ((> j lh) nil)
        (tagbody
          (setf nrmj (f2cl-lib:int-sub nr j))
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i mr) nil)
            (tagbody
              (setf s
                      (f2cl-lib:fref q-%data% (i j) ((1 idimq) (1 1))
                                     q-%offset%))
              (setf (f2cl-lib:fref q-%data% (i j) ((1 idimq) (1 1)) q-%offset%)
                      (f2cl-lib:fref q-%data% (i nrmj) ((1 idimq) (1 1))
                                     q-%offset%))
              (setf (f2cl-lib:fref q-%data% (i nrmj) ((1 idimq) (1 1))
                                   q-%offset%)
                      s)
             label116))
         label117))
     label118
      (setf (f2cl-lib:fref w-%data% (1) ((1 1)) w-%offset%)
              (coerce (the f2cl-lib:integer4 ipstor) 'single-float))
      (go end_label)
     end_label
      (return
       (values nil nil nil nil nil nil idimq nil nil nil nil nil nil nil nil
               nil)))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::poisp2
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo :arg-types
                                            '((fortran-to-lisp::integer4)
                                              (fortran-to-lisp::integer4)
                                              (array single-float (*))
                                              (array single-float (*))
                                              (array single-float (*))
                                              (array single-float (*))
                                              (fortran-to-lisp::integer4)
                                              (array single-float (*))
                                              (array single-float (*))
                                              (array single-float (*))
                                              (array single-float (*))
                                              (array single-float (*))
                                              (array single-float (*))
                                              (array single-float (*))
                                              (array single-float (*))
                                              (array single-float (*)))
                                            :return-values
                                            '(nil nil nil nil nil nil
                                              fortran-to-lisp::idimq nil nil
                                              nil nil nil nil nil nil nil)
                                            :calls '(fortran-to-lisp::poisd2))))

