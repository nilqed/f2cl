;;; Compiled by f2cl version:
;;; ("" "" "" "" "" "" "")

;;; Using Lisp SBCL 1.4.2
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing t) (:declare-common nil)
;;;           (:float-format single-float))

(in-package "FISHPACK")


(defun hwscrt
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
    (prog ((ierr1 0) (nstm1 0) (nsp1 0) (mstm1 0) (msp1 0) (s1 0.0) (a2 0.0)
           (a1 0.0) (st2 0.0) (s 0.0) (id4 0) (id3 0) (id2 0) (i 0) (munk 0)
           (j 0) (mskip 0) (mstop 0) (mstart 0) (nunk 0) (nskip 0) (nstop 0)
           (nstart 0) (mp1 0) (mp 0) (np1 0) (np 0) (delysq 0.0) (twdely 0.0)
           (deltay 0.0) (delxsq 0.0) (twdelx 0.0) (deltax 0.0) (mperod 0)
           (nperod 0))
      (declare
       (type (single-float) deltax twdelx delxsq deltay twdely delysq s st2 a1
        a2 s1)
       (type (f2cl-lib:integer4) nperod mperod np np1 mp mp1 nstart nstop nskip
        nunk mstart mstop mskip j munk i id2 id3 id4 msp1 mstm1 nsp1 nstm1
        ierr1))
      (setf ierror 0)
      (if (>= a b)
          (setf ierror 1))
      (if (or (< mbdcnd 0) (> mbdcnd 4))
          (setf ierror 2))
      (if (>= c d)
          (setf ierror 3))
      (if (<= n 3)
          (setf ierror 4))
      (if (or (< nbdcnd 0) (> nbdcnd 4))
          (setf ierror 5))
      (if (< idimf (f2cl-lib:int-add m 1))
          (setf ierror 7))
      (if (<= m 3)
          (setf ierror 8))
      (if (/= ierror 0)
          (go end_label))
      (setf nperod nbdcnd)
      (setf mperod 0)
      (if (> mbdcnd 0)
          (setf mperod 1))
      (setf deltax (/ (- b a) (f2cl-lib:ffloat m)))
      (setf twdelx (/ 2.0 deltax))
      (setf delxsq (/ 1.0 (expt deltax 2)))
      (setf deltay (/ (- d c) (f2cl-lib:ffloat n)))
      (setf twdely (/ 2.0 deltay))
      (setf delysq (/ 1.0 (expt deltay 2)))
      (setf np (f2cl-lib:int-add nbdcnd 1))
      (setf np1 (f2cl-lib:int-add n 1))
      (setf mp (f2cl-lib:int-add mbdcnd 1))
      (setf mp1 (f2cl-lib:int-add m 1))
      (setf nstart 1)
      (setf nstop n)
      (setf nskip 1)
      (f2cl-lib:computed-goto (label104 label101 label102 label103 label104)
                              np)
     label101
      (setf nstart 2)
      (go label104)
     label102
      (setf nstart 2)
     label103
      (setf nstop np1)
      (setf nskip 2)
     label104
      (setf nunk (f2cl-lib:int-add (f2cl-lib:int-sub nstop nstart) 1))
      (setf mstart 1)
      (setf mstop m)
      (setf mskip 1)
      (f2cl-lib:computed-goto (label117 label105 label106 label109 label110)
                              mp)
     label105
      (setf mstart 2)
      (go label107)
     label106
      (setf mstart 2)
      (setf mstop mp1)
      (setf mskip 2)
     label107
      (f2cl-lib:fdo (j nstart (f2cl-lib:int-add j 1))
                    ((> j nstop) nil)
        (tagbody
          (setf (f2cl-lib:fref f-%data% (2 j) ((1 idimf) (1 1)) f-%offset%)
                  (-
                   (f2cl-lib:fref f-%data% (2 j) ((1 idimf) (1 1)) f-%offset%)
                   (*
                    (f2cl-lib:fref f-%data% (1 j) ((1 idimf) (1 1)) f-%offset%)
                    delxsq)))
         label108))
      (go label112)
     label109
      (setf mstop mp1)
      (setf mskip 2)
     label110
      (f2cl-lib:fdo (j nstart (f2cl-lib:int-add j 1))
                    ((> j nstop) nil)
        (tagbody
          (setf (f2cl-lib:fref f-%data% (1 j) ((1 idimf) (1 1)) f-%offset%)
                  (+
                   (f2cl-lib:fref f-%data% (1 j) ((1 idimf) (1 1)) f-%offset%)
                   (* (f2cl-lib:fref bda-%data% (j) ((1 1)) bda-%offset%)
                      twdelx)))
         label111))
     label112
      (f2cl-lib:computed-goto (label113 label115) mskip)
     label113
      (f2cl-lib:fdo (j nstart (f2cl-lib:int-add j 1))
                    ((> j nstop) nil)
        (tagbody
          (setf (f2cl-lib:fref f-%data% (m j) ((1 idimf) (1 1)) f-%offset%)
                  (-
                   (f2cl-lib:fref f-%data% (m j) ((1 idimf) (1 1)) f-%offset%)
                   (*
                    (f2cl-lib:fref f-%data% (mp1 j) ((1 idimf) (1 1))
                                   f-%offset%)
                    delxsq)))
         label114))
      (go label117)
     label115
      (f2cl-lib:fdo (j nstart (f2cl-lib:int-add j 1))
                    ((> j nstop) nil)
        (tagbody
          (setf (f2cl-lib:fref f-%data% (mp1 j) ((1 idimf) (1 1)) f-%offset%)
                  (-
                   (f2cl-lib:fref f-%data% (mp1 j) ((1 idimf) (1 1))
                                  f-%offset%)
                   (* (f2cl-lib:fref bdb-%data% (j) ((1 1)) bdb-%offset%)
                      twdelx)))
         label116))
     label117
      (setf munk (f2cl-lib:int-add (f2cl-lib:int-sub mstop mstart) 1))
      (f2cl-lib:computed-goto (label127 label118 label118 label120 label120)
                              np)
     label118
      (f2cl-lib:fdo (i mstart (f2cl-lib:int-add i 1))
                    ((> i mstop) nil)
        (tagbody
          (setf (f2cl-lib:fref f-%data% (i 2) ((1 idimf) (1 1)) f-%offset%)
                  (-
                   (f2cl-lib:fref f-%data% (i 2) ((1 idimf) (1 1)) f-%offset%)
                   (*
                    (f2cl-lib:fref f-%data% (i 1) ((1 idimf) (1 1)) f-%offset%)
                    delysq)))
         label119))
      (go label122)
     label120
      (f2cl-lib:fdo (i mstart (f2cl-lib:int-add i 1))
                    ((> i mstop) nil)
        (tagbody
          (setf (f2cl-lib:fref f-%data% (i 1) ((1 idimf) (1 1)) f-%offset%)
                  (+
                   (f2cl-lib:fref f-%data% (i 1) ((1 idimf) (1 1)) f-%offset%)
                   (* (f2cl-lib:fref bdc-%data% (i) ((1 1)) bdc-%offset%)
                      twdely)))
         label121))
     label122
      (f2cl-lib:computed-goto (label123 label125) nskip)
     label123
      (f2cl-lib:fdo (i mstart (f2cl-lib:int-add i 1))
                    ((> i mstop) nil)
        (tagbody
          (setf (f2cl-lib:fref f-%data% (i n) ((1 idimf) (1 1)) f-%offset%)
                  (-
                   (f2cl-lib:fref f-%data% (i n) ((1 idimf) (1 1)) f-%offset%)
                   (*
                    (f2cl-lib:fref f-%data% (i np1) ((1 idimf) (1 1))
                                   f-%offset%)
                    delysq)))
         label124))
      (go label127)
     label125
      (f2cl-lib:fdo (i mstart (f2cl-lib:int-add i 1))
                    ((> i mstop) nil)
        (tagbody
          (setf (f2cl-lib:fref f-%data% (i np1) ((1 idimf) (1 1)) f-%offset%)
                  (-
                   (f2cl-lib:fref f-%data% (i np1) ((1 idimf) (1 1))
                                  f-%offset%)
                   (* (f2cl-lib:fref bdd-%data% (i) ((1 1)) bdd-%offset%)
                      twdely)))
         label126))
     label127
      (setf delysq (* deltay deltay))
      (f2cl-lib:fdo (i mstart (f2cl-lib:int-add i 1))
                    ((> i mstop) nil)
        (tagbody
          (f2cl-lib:fdo (j nstart (f2cl-lib:int-add j 1))
                        ((> j nstop) nil)
            (tagbody
              (setf (f2cl-lib:fref f-%data% (i j) ((1 idimf) (1 1)) f-%offset%)
                      (*
                       (f2cl-lib:fref f-%data% (i j) ((1 idimf) (1 1))
                                      f-%offset%)
                       delysq))
             label128))
         label129))
      (setf id2 munk)
      (setf id3 (f2cl-lib:int-add id2 munk))
      (setf id4 (f2cl-lib:int-add id3 munk))
      (setf s (* delysq delxsq))
      (setf st2 (* 2.0 s))
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i munk) nil)
        (tagbody
          (setf (f2cl-lib:fref w-%data% (i) ((1 1)) w-%offset%) s)
          (setf j (f2cl-lib:int-add id2 i))
          (setf (f2cl-lib:fref w-%data% (j) ((1 1)) w-%offset%)
                  (- (* elmbda delysq) st2))
          (setf j (f2cl-lib:int-add id3 i))
          (setf (f2cl-lib:fref w-%data% (j) ((1 1)) w-%offset%) s)
         label130))
      (if (= mp 1)
          (go label131))
      (setf (f2cl-lib:fref w-%data% (1) ((1 1)) w-%offset%) 0.0)
      (setf (f2cl-lib:fref w-%data% (id4) ((1 1)) w-%offset%) 0.0)
     label131
      (f2cl-lib:computed-goto (label135 label135 label132 label133 label134)
                              mp)
     label132
      (setf (f2cl-lib:fref w-%data% (id2) ((1 1)) w-%offset%) st2)
      (go label135)
     label133
      (setf (f2cl-lib:fref w-%data% (id2) ((1 1)) w-%offset%) st2)
     label134
      (setf (f2cl-lib:fref w-%data% ((f2cl-lib:int-add id3 1)) ((1 1))
                           w-%offset%)
              st2)
     label135
      (setf pertrb 0.0)
      (f2cl-lib:arithmetic-if elmbda (go label144) (go label137) (go label136))
     label136
      (setf ierror 6)
      (go label144)
     label137
      (if (and (or (= nbdcnd 0) (= nbdcnd 3)) (or (= mbdcnd 0) (= mbdcnd 3)))
          (go label138))
      (go label144)
     label138
      (setf a1 1.0)
      (setf a2 1.0)
      (if (= nbdcnd 3)
          (setf a2 2.0))
      (if (= mbdcnd 3)
          (setf a1 2.0))
      (setf s1 0.0)
      (setf msp1 (f2cl-lib:int-add mstart 1))
      (setf mstm1 (f2cl-lib:int-sub mstop 1))
      (setf nsp1 (f2cl-lib:int-add nstart 1))
      (setf nstm1 (f2cl-lib:int-sub nstop 1))
      (f2cl-lib:fdo (j nsp1 (f2cl-lib:int-add j 1))
                    ((> j nstm1) nil)
        (tagbody
          (setf s 0.0)
          (f2cl-lib:fdo (i msp1 (f2cl-lib:int-add i 1))
                        ((> i mstm1) nil)
            (tagbody
              (setf s
                      (+ s
                         (f2cl-lib:fref f-%data% (i j) ((1 idimf) (1 1))
                                        f-%offset%)))
             label139))
          (setf s1
                  (+ s1 (* s a1)
                     (f2cl-lib:fref f-%data% (mstart j) ((1 idimf) (1 1))
                                    f-%offset%)
                     (f2cl-lib:fref f-%data% (mstop j) ((1 idimf) (1 1))
                                    f-%offset%)))
         label140))
      (setf s1 (* a2 s1))
      (setf s 0.0)
      (f2cl-lib:fdo (i msp1 (f2cl-lib:int-add i 1))
                    ((> i mstm1) nil)
        (tagbody
          (setf s
                  (+ s
                     (f2cl-lib:fref f-%data% (i nstart) ((1 idimf) (1 1))
                                    f-%offset%)
                     (f2cl-lib:fref f-%data% (i nstop) ((1 idimf) (1 1))
                                    f-%offset%)))
         label141))
      (setf s1
              (+ s1 (* s a1)
                 (f2cl-lib:fref f-%data% (mstart nstart) ((1 idimf) (1 1))
                                f-%offset%)
                 (f2cl-lib:fref f-%data% (mstart nstop) ((1 idimf) (1 1))
                                f-%offset%)
                 (f2cl-lib:fref f-%data% (mstop nstart) ((1 idimf) (1 1))
                                f-%offset%)
                 (f2cl-lib:fref f-%data% (mstop nstop) ((1 idimf) (1 1))
                                f-%offset%)))
      (setf s
              (* (+ 2.0 (* (f2cl-lib:ffloat (f2cl-lib:int-sub nunk 2)) a2))
                 (+ 2.0 (* (f2cl-lib:ffloat (f2cl-lib:int-sub munk 2)) a1))))
      (setf pertrb (/ s1 s))
      (f2cl-lib:fdo (j nstart (f2cl-lib:int-add j 1))
                    ((> j nstop) nil)
        (tagbody
          (f2cl-lib:fdo (i mstart (f2cl-lib:int-add i 1))
                        ((> i mstop) nil)
            (tagbody
              (setf (f2cl-lib:fref f-%data% (i j) ((1 idimf) (1 1)) f-%offset%)
                      (-
                       (f2cl-lib:fref f-%data% (i j) ((1 idimf) (1 1))
                                      f-%offset%)
                       pertrb))
             label142))
         label143))
      (setf pertrb (/ pertrb delysq))
     label144
      (multiple-value-bind
          (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9 var-10)
          (genbun nperod nunk mperod munk
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
          (go label146))
      (f2cl-lib:fdo (i mstart (f2cl-lib:int-add i 1))
                    ((> i mstop) nil)
        (tagbody
          (setf (f2cl-lib:fref f-%data% (i np1) ((1 idimf) (1 1)) f-%offset%)
                  (f2cl-lib:fref f-%data% (i 1) ((1 idimf) (1 1)) f-%offset%))
         label145))
     label146
      (if (/= mbdcnd 0)
          (go label148))
      (f2cl-lib:fdo (j nstart (f2cl-lib:int-add j 1))
                    ((> j nstop) nil)
        (tagbody
          (setf (f2cl-lib:fref f-%data% (mp1 j) ((1 idimf) (1 1)) f-%offset%)
                  (f2cl-lib:fref f-%data% (1 j) ((1 idimf) (1 1)) f-%offset%))
         label147))
      (if (= nbdcnd 0)
          (setf (f2cl-lib:fref f-%data% (mp1 np1) ((1 idimf) (1 1)) f-%offset%)
                  (f2cl-lib:fref f-%data% (1 np1) ((1 idimf) (1 1))
                                 f-%offset%)))
     label148
      (go end_label)
     end_label
      (return
       (values nil nil nil nil nil nil nil nil nil nil nil nil nil nil idimf
               pertrb ierror nil)))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::hwscrt
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

