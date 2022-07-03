;;; Compiled by f2cl version:
;;; ("" "" "" "" "" "" "")

;;; Using Lisp SBCL 1.4.2
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing t) (:declare-common nil)
;;;           (:float-format single-float))

(in-package "FISHPACK")


(defun hwscyl
       (a b m mbdcnd bda bdb c d n nbdcnd bdc bdd elmbda f idimf pertrb ierror
        w)
  (declare (type (array single-float (*)) f)
           (type (array single-float (*)) w bdd bdc bdb bda)
           (type (f2cl-lib:integer4) ierror idimf nbdcnd n mbdcnd m)
           (type (single-float) pertrb elmbda d c b a))
  (f2cl-lib:with-multi-array-data
      ((bda single-float bda-%data% bda-%offset%)
       (bdb single-float bdb-%data% bdb-%offset%)
       (bdc single-float bdc-%data% bdc-%offset%)
       (bdd single-float bdd-%data% bdd-%offset%)
       (w single-float w-%data% w-%offset%)
       (f single-float f-%data% f-%offset%))
    (prog ((ierr1 0) (s2 0.0) (nstm1 0) (nsp1 0) (s1 0.0) (s 0.0) (a2 0.0)
           (l 0) (k 0) (j 0) (r 0.0) (i 0) (ij 0) (a1 0.0) (istart 0) (id6 0)
           (id5 0) (id4 0) (id3 0) (id2 0) (nunk 0) (nstop 0) (nstart 0)
           (munk 0) (mstop 0) (mstart 0) (np 0) (dlthsq 0.0) (deltht 0.0)
           (np1 0) (dlrsq 0.0) (dlrby2 0.0) (deltar 0.0) (mp1 0))
      (declare
       (type (single-float) deltar dlrby2 dlrsq deltht dlthsq a1 r a2 s s1 s2)
       (type (f2cl-lib:integer4) mp1 np1 np mstart mstop munk nstart nstop nunk
        id2 id3 id4 id5 id6 istart ij i j k l nsp1 nstm1 ierr1))
      (setf ierror 0)
      (if (< a 0.0)
          (setf ierror 1))
      (if (>= a b)
          (setf ierror 2))
      (if (or (<= mbdcnd 0) (>= mbdcnd 7))
          (setf ierror 3))
      (if (>= c d)
          (setf ierror 4))
      (if (<= n 3)
          (setf ierror 5))
      (if (or (<= nbdcnd -1) (>= nbdcnd 5))
          (setf ierror 6))
      (if (and (= a 0.0) (or (= mbdcnd 3) (= mbdcnd 4)))
          (setf ierror 7))
      (if (and (> a 0.0) (>= mbdcnd 5))
          (setf ierror 8))
      (if (and (= a 0.0) (/= elmbda 0.0) (>= mbdcnd 5))
          (setf ierror 9))
      (if (< idimf (f2cl-lib:int-add m 1))
          (setf ierror 10))
      (if (<= m 3)
          (setf ierror 12))
      (if (/= ierror 0)
          (go end_label))
      (setf mp1 (f2cl-lib:int-add m 1))
      (setf deltar (/ (- b a) (f2cl-lib:ffloat m)))
      (setf dlrby2 (/ deltar 2.0))
      (setf dlrsq (expt deltar 2))
      (setf np1 (f2cl-lib:int-add n 1))
      (setf deltht (/ (- d c) (f2cl-lib:ffloat n)))
      (setf dlthsq (expt deltht 2))
      (setf np (f2cl-lib:int-add nbdcnd 1))
      (setf mstart 2)
      (setf mstop m)
      (f2cl-lib:computed-goto
       (label104 label103 label102 label101 label101 label102) mbdcnd)
     label101
      (setf mstart 1)
      (go label104)
     label102
      (setf mstart 1)
     label103
      (setf mstop mp1)
     label104
      (setf munk (f2cl-lib:int-add (f2cl-lib:int-sub mstop mstart) 1))
      (setf nstart 1)
      (setf nstop n)
      (f2cl-lib:computed-goto (label108 label105 label106 label107 label108)
                              np)
     label105
      (setf nstart 2)
      (go label108)
     label106
      (setf nstart 2)
     label107
      (setf nstop np1)
     label108
      (setf nunk (f2cl-lib:int-add (f2cl-lib:int-sub nstop nstart) 1))
      (setf id2 munk)
      (setf id3 (f2cl-lib:int-add id2 munk))
      (setf id4 (f2cl-lib:int-add id3 munk))
      (setf id5 (f2cl-lib:int-add id4 munk))
      (setf id6 (f2cl-lib:int-add id5 munk))
      (setf istart 1)
      (setf a1 (/ 2.0 dlrsq))
      (setf ij 0)
      (if (or (= mbdcnd 3) (= mbdcnd 4))
          (setf ij 1))
      (if (<= mbdcnd 4)
          (go label109))
      (setf (f2cl-lib:fref w-%data% (1) ((1 1)) w-%offset%) 0.0)
      (setf (f2cl-lib:fref w-%data% ((f2cl-lib:int-add id2 1)) ((1 1))
                           w-%offset%)
              (* -2.0 a1))
      (setf (f2cl-lib:fref w-%data% ((f2cl-lib:int-add id3 1)) ((1 1))
                           w-%offset%)
              (* 2.0 a1))
      (setf istart 2)
      (setf ij 1)
     label109
      (f2cl-lib:fdo (i istart (f2cl-lib:int-add i 1))
                    ((> i munk) nil)
        (tagbody
          (setf r (+ a (* (f2cl-lib:ffloat (f2cl-lib:int-sub i ij)) deltar)))
          (setf j (f2cl-lib:int-add id5 i))
          (setf (f2cl-lib:fref w-%data% (j) ((1 1)) w-%offset%) r)
          (setf j (f2cl-lib:int-add id6 i))
          (setf (f2cl-lib:fref w-%data% (j) ((1 1)) w-%offset%)
                  (/ 1.0 (expt r 2)))
          (setf (f2cl-lib:fref w-%data% (i) ((1 1)) w-%offset%)
                  (/ (- r dlrby2) (* r dlrsq)))
          (setf j (f2cl-lib:int-add id3 i))
          (setf (f2cl-lib:fref w-%data% (j) ((1 1)) w-%offset%)
                  (/ (+ r dlrby2) (* r dlrsq)))
          (setf k (f2cl-lib:int-add id6 i))
          (setf j (f2cl-lib:int-add id2 i))
          (setf (f2cl-lib:fref w-%data% (j) ((1 1)) w-%offset%)
                  (- (* elmbda (f2cl-lib:fref w-%data% (k) ((1 1)) w-%offset%))
                     a1))
         label110))
      (f2cl-lib:computed-goto
       (label114 label111 label112 label113 label114 label112) mbdcnd)
     label111
      (setf (f2cl-lib:fref w-%data% (id2) ((1 1)) w-%offset%) a1)
      (go label114)
     label112
      (setf (f2cl-lib:fref w-%data% (id2) ((1 1)) w-%offset%) a1)
     label113
      (setf (f2cl-lib:fref w-%data% ((f2cl-lib:int-add id3 1)) ((1 1))
                           w-%offset%)
              (* a1 (f2cl-lib:ffloat istart)))
     label114
      (f2cl-lib:computed-goto
       (label115 label115 label117 label117 label119 label119) mbdcnd)
     label115
      (setf a1 (f2cl-lib:fref w-%data% (1) ((1 1)) w-%offset%))
      (f2cl-lib:fdo (j nstart (f2cl-lib:int-add j 1))
                    ((> j nstop) nil)
        (tagbody
          (setf (f2cl-lib:fref f-%data% (2 j) ((1 idimf) (1 1)) f-%offset%)
                  (-
                   (f2cl-lib:fref f-%data% (2 j) ((1 idimf) (1 1)) f-%offset%)
                   (* a1
                      (f2cl-lib:fref f-%data% (1 j) ((1 idimf) (1 1))
                                     f-%offset%))))
         label116))
      (go label119)
     label117
      (setf a1 (* 2.0 deltar (f2cl-lib:fref w-%data% (1) ((1 1)) w-%offset%)))
      (f2cl-lib:fdo (j nstart (f2cl-lib:int-add j 1))
                    ((> j nstop) nil)
        (tagbody
          (setf (f2cl-lib:fref f-%data% (1 j) ((1 idimf) (1 1)) f-%offset%)
                  (+
                   (f2cl-lib:fref f-%data% (1 j) ((1 idimf) (1 1)) f-%offset%)
                   (* a1 (f2cl-lib:fref bda-%data% (j) ((1 1)) bda-%offset%))))
         label118))
     label119
      (f2cl-lib:computed-goto
       (label120 label122 label122 label120 label120 label122) mbdcnd)
     label120
      (setf a1 (f2cl-lib:fref w-%data% (id4) ((1 1)) w-%offset%))
      (f2cl-lib:fdo (j nstart (f2cl-lib:int-add j 1))
                    ((> j nstop) nil)
        (tagbody
          (setf (f2cl-lib:fref f-%data% (m j) ((1 idimf) (1 1)) f-%offset%)
                  (-
                   (f2cl-lib:fref f-%data% (m j) ((1 idimf) (1 1)) f-%offset%)
                   (* a1
                      (f2cl-lib:fref f-%data% (mp1 j) ((1 idimf) (1 1))
                                     f-%offset%))))
         label121))
      (go label124)
     label122
      (setf a1
              (* 2.0 deltar (f2cl-lib:fref w-%data% (id4) ((1 1)) w-%offset%)))
      (f2cl-lib:fdo (j nstart (f2cl-lib:int-add j 1))
                    ((> j nstop) nil)
        (tagbody
          (setf (f2cl-lib:fref f-%data% (mp1 j) ((1 idimf) (1 1)) f-%offset%)
                  (-
                   (f2cl-lib:fref f-%data% (mp1 j) ((1 idimf) (1 1))
                                  f-%offset%)
                   (* a1 (f2cl-lib:fref bdb-%data% (j) ((1 1)) bdb-%offset%))))
         label123))
     label124
      (setf a1 (/ 1.0 dlthsq))
      (setf l (f2cl-lib:int-add (f2cl-lib:int-sub id5 mstart) 1))
      (f2cl-lib:computed-goto (label134 label125 label125 label127 label127)
                              np)
     label125
      (f2cl-lib:fdo (i mstart (f2cl-lib:int-add i 1))
                    ((> i mstop) nil)
        (tagbody
          (setf (f2cl-lib:fref f-%data% (i 2) ((1 idimf) (1 1)) f-%offset%)
                  (-
                   (f2cl-lib:fref f-%data% (i 2) ((1 idimf) (1 1)) f-%offset%)
                   (* a1
                      (f2cl-lib:fref f-%data% (i 1) ((1 idimf) (1 1))
                                     f-%offset%))))
         label126))
      (go label129)
     label127
      (setf a1 (/ 2.0 deltht))
      (f2cl-lib:fdo (i mstart (f2cl-lib:int-add i 1))
                    ((> i mstop) nil)
        (tagbody
          (setf (f2cl-lib:fref f-%data% (i 1) ((1 idimf) (1 1)) f-%offset%)
                  (+
                   (f2cl-lib:fref f-%data% (i 1) ((1 idimf) (1 1)) f-%offset%)
                   (* a1 (f2cl-lib:fref bdc-%data% (i) ((1 1)) bdc-%offset%))))
         label128))
     label129
      (setf a1 (/ 1.0 dlthsq))
      (f2cl-lib:computed-goto (label134 label130 label132 label132 label130)
                              np)
     label130
      (f2cl-lib:fdo (i mstart (f2cl-lib:int-add i 1))
                    ((> i mstop) nil)
        (tagbody
          (setf (f2cl-lib:fref f-%data% (i n) ((1 idimf) (1 1)) f-%offset%)
                  (-
                   (f2cl-lib:fref f-%data% (i n) ((1 idimf) (1 1)) f-%offset%)
                   (* a1
                      (f2cl-lib:fref f-%data% (i np1) ((1 idimf) (1 1))
                                     f-%offset%))))
         label131))
      (go label134)
     label132
      (setf a1 (/ 2.0 deltht))
      (f2cl-lib:fdo (i mstart (f2cl-lib:int-add i 1))
                    ((> i mstop) nil)
        (tagbody
          (setf (f2cl-lib:fref f-%data% (i np1) ((1 idimf) (1 1)) f-%offset%)
                  (-
                   (f2cl-lib:fref f-%data% (i np1) ((1 idimf) (1 1))
                                  f-%offset%)
                   (* a1 (f2cl-lib:fref bdd-%data% (i) ((1 1)) bdd-%offset%))))
         label133))
     label134
      (setf pertrb 0.0)
      (f2cl-lib:arithmetic-if elmbda (go label146) (go label136) (go label135))
     label135
      (setf ierror 11)
      (go label146)
     label136
      (setf (f2cl-lib:fref w-%data% ((f2cl-lib:int-add id5 1)) ((1 1))
                           w-%offset%)
              (* 0.5
                 (-
                  (f2cl-lib:fref w-%data% ((f2cl-lib:int-add id5 2)) ((1 1))
                                 w-%offset%)
                  dlrby2)))
      (f2cl-lib:computed-goto
       (label146 label146 label138 label146 label146 label137) mbdcnd)
     label137
      (setf (f2cl-lib:fref w-%data% ((f2cl-lib:int-add id5 1)) ((1 1))
                           w-%offset%)
              (* 0.5
                 (f2cl-lib:fref w-%data% ((f2cl-lib:int-add id5 1)) ((1 1))
                                w-%offset%)))
     label138
      (f2cl-lib:computed-goto (label140 label146 label146 label139 label146)
                              np)
     label139
      (setf a2 2.0)
      (go label141)
     label140
      (setf a2 1.0)
     label141
      (setf k (f2cl-lib:int-add id5 munk))
      (setf (f2cl-lib:fref w-%data% (k) ((1 1)) w-%offset%)
              (* 0.5
                 (+
                  (f2cl-lib:fref w-%data% ((f2cl-lib:int-sub k 1)) ((1 1))
                                 w-%offset%)
                  dlrby2)))
      (setf s 0.0)
      (f2cl-lib:fdo (i mstart (f2cl-lib:int-add i 1))
                    ((> i mstop) nil)
        (tagbody
          (setf s1 0.0)
          (setf nsp1 (f2cl-lib:int-add nstart 1))
          (setf nstm1 (f2cl-lib:int-sub nstop 1))
          (f2cl-lib:fdo (j nsp1 (f2cl-lib:int-add j 1))
                        ((> j nstm1) nil)
            (tagbody
              (setf s1
                      (+ s1
                         (f2cl-lib:fref f-%data% (i j) ((1 idimf) (1 1))
                                        f-%offset%)))
             label142))
          (setf k (f2cl-lib:int-add i l))
          (setf s
                  (+ s
                     (*
                      (+ (* a2 s1)
                         (f2cl-lib:fref f-%data% (i nstart) ((1 idimf) (1 1))
                                        f-%offset%)
                         (f2cl-lib:fref f-%data% (i nstop) ((1 idimf) (1 1))
                                        f-%offset%))
                      (f2cl-lib:fref w-%data% (k) ((1 1)) w-%offset%))))
         label143))
      (setf s2
              (+ (* (f2cl-lib:ffloat m) a)
                 (*
                  (+ 0.75
                     (f2cl-lib:ffloat
                      (f2cl-lib:int-mul (f2cl-lib:int-sub m 1)
                                        (f2cl-lib:int-add m 1))))
                  dlrby2)))
      (if (= mbdcnd 3)
          (setf s2 (+ s2 (* 0.25 dlrby2))))
      (setf s1
              (* (+ 2.0 (* a2 (f2cl-lib:ffloat (f2cl-lib:int-sub nunk 2))))
                 s2))
      (setf pertrb (/ s s1))
      (f2cl-lib:fdo (i mstart (f2cl-lib:int-add i 1))
                    ((> i mstop) nil)
        (tagbody
          (f2cl-lib:fdo (j nstart (f2cl-lib:int-add j 1))
                        ((> j nstop) nil)
            (tagbody
              (setf (f2cl-lib:fref f-%data% (i j) ((1 idimf) (1 1)) f-%offset%)
                      (-
                       (f2cl-lib:fref f-%data% (i j) ((1 idimf) (1 1))
                                      f-%offset%)
                       pertrb))
             label144))
         label145))
     label146
      (f2cl-lib:fdo (i mstart (f2cl-lib:int-add i 1))
                    ((> i mstop) nil)
        (tagbody
          (setf k (f2cl-lib:int-add (f2cl-lib:int-sub i mstart) 1))
          (setf (f2cl-lib:fref w-%data% (k) ((1 1)) w-%offset%)
                  (* (f2cl-lib:fref w-%data% (k) ((1 1)) w-%offset%) dlthsq))
          (setf j (f2cl-lib:int-add id2 k))
          (setf (f2cl-lib:fref w-%data% (j) ((1 1)) w-%offset%)
                  (* (f2cl-lib:fref w-%data% (j) ((1 1)) w-%offset%) dlthsq))
          (setf j (f2cl-lib:int-add id3 k))
          (setf (f2cl-lib:fref w-%data% (j) ((1 1)) w-%offset%)
                  (* (f2cl-lib:fref w-%data% (j) ((1 1)) w-%offset%) dlthsq))
          (f2cl-lib:fdo (j nstart (f2cl-lib:int-add j 1))
                        ((> j nstop) nil)
            (tagbody
              (setf (f2cl-lib:fref f-%data% (i j) ((1 idimf) (1 1)) f-%offset%)
                      (*
                       (f2cl-lib:fref f-%data% (i j) ((1 idimf) (1 1))
                                      f-%offset%)
                       dlthsq))
             label147))
         label148))
      (setf (f2cl-lib:fref w-%data% (1) ((1 1)) w-%offset%) 0.0)
      (setf (f2cl-lib:fref w-%data% (id4) ((1 1)) w-%offset%) 0.0)
      (multiple-value-bind
          (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9 var-10)
          (genbun nbdcnd nunk 1 munk
           (f2cl-lib:array-slice w-%data% single-float (1) ((1 1)) w-%offset%)
           (f2cl-lib:array-slice w-%data% single-float ((+ id2 1)) ((1 1))
                                 w-%offset%)
           (f2cl-lib:array-slice w-%data% single-float ((+ id3 1)) ((1 1))
                                 w-%offset%)
           idimf
           (f2cl-lib:array-slice f-%data% single-float (mstart nstart)
                                 ((1 idimf) (1 1)) f-%offset%)
           ierr1
           (f2cl-lib:array-slice w-%data% single-float ((+ id4 1)) ((1 1))
                                 w-%offset%))
        (declare
         (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-8 var-10))
        (setf idimf var-7)
        (setf ierr1 var-9))
      (setf (f2cl-lib:fref w-%data% (1) ((1 1)) w-%offset%)
              (+
               (f2cl-lib:fref w-%data% ((f2cl-lib:int-add id4 1)) ((1 1))
                              w-%offset%)
               (* 3.0 (f2cl-lib:ffloat munk))))
      (if (/= nbdcnd 0)
          (go label150))
      (f2cl-lib:fdo (i mstart (f2cl-lib:int-add i 1))
                    ((> i mstop) nil)
        (tagbody
          (setf (f2cl-lib:fref f-%data% (i np1) ((1 idimf) (1 1)) f-%offset%)
                  (f2cl-lib:fref f-%data% (i 1) ((1 idimf) (1 1)) f-%offset%))
         label149))
     label150
      (go end_label)
     end_label
      (return
       (values nil nil nil nil nil nil nil nil nil nil nil nil nil nil idimf
               pertrb ierror nil)))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::hwscyl
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
                                            :calls '(fortran-to-lisp::genbun))))

