;;; Compiled by f2cl version:
;;; ("" "" "" "" "" "" "")

;;; Using Lisp SBCL 1.4.2
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing t) (:declare-common nil)
;;;           (:float-format single-float))

(in-package "FISHPACK")


(defun hwsplr
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
    (prog ((ip 0) (ypole 0.0) (iwstor 0) (ierr1 0) (k 0) (s1 0.0) (s 0.0)
           (a2 0.0) (s2 0.0) (lp 0) (l 0) (j 0) (r 0.0) (i 0) (ij 0) (a1 0.0)
           (id6 0) (id5 0) (id4 0) (id3 0) (id2 0) (nunk 0) (nstop 0)
           (nstart 0) (munk 0) (mstop 0) (mstart 0) (np 0) (dlthsq 0.0)
           (deltht 0.0) (np1 0) (dlrsq 0.0) (dlrby2 0.0) (deltar 0.0) (mp1 0))
      (declare
       (type (single-float) deltar dlrby2 dlrsq deltht dlthsq a1 r s2 a2 s s1
        ypole)
       (type (f2cl-lib:integer4) mp1 np1 np mstart mstop munk nstart nstop nunk
        id2 id3 id4 id5 id6 ij i j l lp k ierr1 iwstor ip))
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
      (if (and (>= mbdcnd 5) (/= nbdcnd 0) (/= nbdcnd 3))
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
      (setf mstop mp1)
      (f2cl-lib:computed-goto
       (label101 label105 label102 label103 label104 label105) mbdcnd)
     label101
      (setf mstop m)
      (go label105)
     label102
      (setf mstart 1)
      (go label105)
     label103
      (setf mstart 1)
     label104
      (setf mstop m)
     label105
      (setf munk (f2cl-lib:int-add (f2cl-lib:int-sub mstop mstart) 1))
      (setf nstart 1)
      (setf nstop n)
      (f2cl-lib:computed-goto (label109 label106 label107 label108 label109)
                              np)
     label106
      (setf nstart 2)
      (go label109)
     label107
      (setf nstart 2)
     label108
      (setf nstop np1)
     label109
      (setf nunk (f2cl-lib:int-add (f2cl-lib:int-sub nstop nstart) 1))
      (setf id2 munk)
      (setf id3 (f2cl-lib:int-add id2 munk))
      (setf id4 (f2cl-lib:int-add id3 munk))
      (setf id5 (f2cl-lib:int-add id4 munk))
      (setf id6 (f2cl-lib:int-add id5 munk))
      (setf a1 (/ 2.0 dlrsq))
      (setf ij 0)
      (if (or (= mbdcnd 3) (= mbdcnd 4))
          (setf ij 1))
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
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
          (setf j (f2cl-lib:int-add id2 i))
          (setf (f2cl-lib:fref w-%data% (j) ((1 1)) w-%offset%) (- elmbda a1))
         label110))
      (f2cl-lib:computed-goto
       (label114 label111 label112 label113 label114 label111) mbdcnd)
     label111
      (setf (f2cl-lib:fref w-%data% (id2) ((1 1)) w-%offset%) a1)
      (go label114)
     label112
      (setf (f2cl-lib:fref w-%data% (id2) ((1 1)) w-%offset%) a1)
     label113
      (setf (f2cl-lib:fref w-%data% ((f2cl-lib:int-add id3 1)) ((1 1))
                           w-%offset%)
              a1)
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
      (setf lp (f2cl-lib:int-add (f2cl-lib:int-sub id6 mstart) 1))
      (f2cl-lib:computed-goto (label134 label125 label125 label127 label127)
                              np)
     label125
      (f2cl-lib:fdo (i mstart (f2cl-lib:int-add i 1))
                    ((> i mstop) nil)
        (tagbody
          (setf j (f2cl-lib:int-add i lp))
          (setf (f2cl-lib:fref f-%data% (i 2) ((1 idimf) (1 1)) f-%offset%)
                  (+
                   (f2cl-lib:fref f-%data% (i 2) ((1 idimf) (1 1)) f-%offset%)
                   (* (- a1) (f2cl-lib:fref w-%data% (j) ((1 1)) w-%offset%)
                      (f2cl-lib:fref f-%data% (i 1) ((1 idimf) (1 1))
                                     f-%offset%))))
         label126))
      (go label129)
     label127
      (setf a1 (/ 2.0 deltht))
      (f2cl-lib:fdo (i mstart (f2cl-lib:int-add i 1))
                    ((> i mstop) nil)
        (tagbody
          (setf j (f2cl-lib:int-add i lp))
          (setf (f2cl-lib:fref f-%data% (i 1) ((1 idimf) (1 1)) f-%offset%)
                  (+
                   (f2cl-lib:fref f-%data% (i 1) ((1 idimf) (1 1)) f-%offset%)
                   (* a1 (f2cl-lib:fref w-%data% (j) ((1 1)) w-%offset%)
                      (f2cl-lib:fref bdc-%data% (i) ((1 1)) bdc-%offset%))))
         label128))
     label129
      (setf a1 (/ 1.0 dlthsq))
      (f2cl-lib:computed-goto (label134 label130 label132 label132 label130)
                              np)
     label130
      (f2cl-lib:fdo (i mstart (f2cl-lib:int-add i 1))
                    ((> i mstop) nil)
        (tagbody
          (setf j (f2cl-lib:int-add i lp))
          (setf (f2cl-lib:fref f-%data% (i n) ((1 idimf) (1 1)) f-%offset%)
                  (+
                   (f2cl-lib:fref f-%data% (i n) ((1 idimf) (1 1)) f-%offset%)
                   (* (- a1) (f2cl-lib:fref w-%data% (j) ((1 1)) w-%offset%)
                      (f2cl-lib:fref f-%data% (i np1) ((1 idimf) (1 1))
                                     f-%offset%))))
         label131))
      (go label134)
     label132
      (setf a1 (/ 2.0 deltht))
      (f2cl-lib:fdo (i mstart (f2cl-lib:int-add i 1))
                    ((> i mstop) nil)
        (tagbody
          (setf j (f2cl-lib:int-add i lp))
          (setf (f2cl-lib:fref f-%data% (i np1) ((1 idimf) (1 1)) f-%offset%)
                  (+
                   (f2cl-lib:fref f-%data% (i np1) ((1 idimf) (1 1))
                                  f-%offset%)
                   (* (- a1) (f2cl-lib:fref w-%data% (j) ((1 1)) w-%offset%)
                      (f2cl-lib:fref bdd-%data% (i) ((1 1)) bdd-%offset%))))
         label133))
     label134
      (if (and (>= mbdcnd 5) (= nbdcnd 3))
          (setf (f2cl-lib:fref f-%data% (1 1) ((1 idimf) (1 1)) f-%offset%)
                  (+
                   (f2cl-lib:fref f-%data% (1 1) ((1 idimf) (1 1)) f-%offset%)
                   (/
                    (*
                     (-
                      (- (f2cl-lib:fref bdd-%data% (2) ((1 1)) bdd-%offset%)
                         (f2cl-lib:fref bdc-%data% (2) ((1 1)) bdc-%offset%)))
                     4.0)
                    (* (f2cl-lib:ffloat n) deltht dlrsq)))))
      (setf pertrb 0.0)
      (f2cl-lib:arithmetic-if elmbda (go label144) (go label136) (go label135))
     label135
      (setf ierror 11)
      (go label144)
     label136
      (if (and (/= nbdcnd 0) (/= nbdcnd 3))
          (go label144))
      (setf s2 0.0)
      (f2cl-lib:computed-goto
       (label144 label144 label137 label144 label144 label138) mbdcnd)
     label137
      (setf (f2cl-lib:fref w-%data% ((f2cl-lib:int-add id5 1)) ((1 1))
                           w-%offset%)
              (* 0.5
                 (-
                  (f2cl-lib:fref w-%data% ((f2cl-lib:int-add id5 2)) ((1 1))
                                 w-%offset%)
                  dlrby2)))
      (setf s2 (* 0.25 deltar))
     label138
      (setf a2 2.0)
      (if (= nbdcnd 0)
          (setf a2 1.0))
      (setf j (f2cl-lib:int-add id5 munk))
      (setf (f2cl-lib:fref w-%data% (j) ((1 1)) w-%offset%)
              (* 0.5
                 (+
                  (f2cl-lib:fref w-%data% ((f2cl-lib:int-sub j 1)) ((1 1))
                                 w-%offset%)
                  dlrby2)))
      (setf s 0.0)
      (f2cl-lib:fdo (i mstart (f2cl-lib:int-add i 1))
                    ((> i mstop) nil)
        (tagbody
          (setf s1 0.0)
          (setf ij (f2cl-lib:int-add nstart 1))
          (setf k (f2cl-lib:int-sub nstop 1))
          (f2cl-lib:fdo (j ij (f2cl-lib:int-add j 1))
                        ((> j k) nil)
            (tagbody
              (setf s1
                      (+ s1
                         (f2cl-lib:fref f-%data% (i j) ((1 idimf) (1 1))
                                        f-%offset%)))
             label139))
          (setf j (f2cl-lib:int-add i l))
          (setf s
                  (+ s
                     (*
                      (+ (* a2 s1)
                         (f2cl-lib:fref f-%data% (i nstart) ((1 idimf) (1 1))
                                        f-%offset%)
                         (f2cl-lib:fref f-%data% (i nstop) ((1 idimf) (1 1))
                                        f-%offset%))
                      (f2cl-lib:fref w-%data% (j) ((1 1)) w-%offset%))))
         label140))
      (setf s2
              (+ (* (f2cl-lib:ffloat m) a)
                 (* deltar
                    (+
                     (*
                      (f2cl-lib:ffloat
                       (f2cl-lib:int-mul (f2cl-lib:int-sub m 1)
                                         (f2cl-lib:int-add m 1)))
                      0.5)
                     0.25))
                 s2))
      (setf s1
              (* (+ 2.0 (* a2 (f2cl-lib:ffloat (f2cl-lib:int-sub nunk 2))))
                 s2))
      (if (= mbdcnd 3)
          (go label141))
      (setf s2 (/ (* (f2cl-lib:ffloat n) a2 deltar) 8.0))
      (setf s
              (+ s
                 (* (f2cl-lib:fref f-%data% (1 1) ((1 idimf) (1 1)) f-%offset%)
                    s2)))
      (setf s1 (+ s1 s2))
     label141
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
             label142))
         label143))
     label144
      (f2cl-lib:fdo (i mstart (f2cl-lib:int-add i 1))
                    ((> i mstop) nil)
        (tagbody
          (setf k (f2cl-lib:int-add (f2cl-lib:int-sub i mstart) 1))
          (setf j (f2cl-lib:int-add i lp))
          (setf a1 (/ dlthsq (f2cl-lib:fref w-%data% (j) ((1 1)) w-%offset%)))
          (setf (f2cl-lib:fref w-%data% (k) ((1 1)) w-%offset%)
                  (* a1 (f2cl-lib:fref w-%data% (k) ((1 1)) w-%offset%)))
          (setf j (f2cl-lib:int-add id2 k))
          (setf (f2cl-lib:fref w-%data% (j) ((1 1)) w-%offset%)
                  (* a1 (f2cl-lib:fref w-%data% (j) ((1 1)) w-%offset%)))
          (setf j (f2cl-lib:int-add id3 k))
          (setf (f2cl-lib:fref w-%data% (j) ((1 1)) w-%offset%)
                  (* a1 (f2cl-lib:fref w-%data% (j) ((1 1)) w-%offset%)))
          (f2cl-lib:fdo (j nstart (f2cl-lib:int-add j 1))
                        ((> j nstop) nil)
            (tagbody
              (setf (f2cl-lib:fref f-%data% (i j) ((1 idimf) (1 1)) f-%offset%)
                      (* a1
                         (f2cl-lib:fref f-%data% (i j) ((1 idimf) (1 1))
                                        f-%offset%)))
             label145))
         label146))
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
      (setf iwstor
              (f2cl-lib:int
               (+
                (f2cl-lib:fref w-%data% ((f2cl-lib:int-add id4 1)) ((1 1))
                               w-%offset%)
                (* 3.0 (f2cl-lib:ffloat munk)))))
      (f2cl-lib:computed-goto
       (label157 label157 label157 label157 label148 label147) mbdcnd)
     label147
      (if (/= elmbda 0.0)
          (go label148))
      (setf ypole 0.0)
      (go label155)
     label148
      (setf j (f2cl-lib:int-add id5 munk))
      (setf (f2cl-lib:fref w-%data% (j) ((1 1)) w-%offset%)
              (/ (f2cl-lib:fref w-%data% (id2) ((1 1)) w-%offset%)
                 (f2cl-lib:fref w-%data% (id3) ((1 1)) w-%offset%)))
      (f2cl-lib:fdo (ip 3 (f2cl-lib:int-add ip 1))
                    ((> ip munk) nil)
        (tagbody
          (setf i (f2cl-lib:int-add (f2cl-lib:int-sub munk ip) 2))
          (setf j (f2cl-lib:int-add id5 i))
          (setf lp (f2cl-lib:int-add id2 i))
          (setf k (f2cl-lib:int-add id3 i))
          (setf (f2cl-lib:fref w-%data% (j) ((1 1)) w-%offset%)
                  (/ (f2cl-lib:fref w-%data% (i) ((1 1)) w-%offset%)
                     (- (f2cl-lib:fref w-%data% (lp) ((1 1)) w-%offset%)
                        (* (f2cl-lib:fref w-%data% (k) ((1 1)) w-%offset%)
                           (f2cl-lib:fref w-%data% ((f2cl-lib:int-add j 1))
                                          ((1 1)) w-%offset%)))))
         label149))
      (setf (f2cl-lib:fref w-%data% ((f2cl-lib:int-add id5 1)) ((1 1))
                           w-%offset%)
              (/ (* -0.5 dlthsq)
                 (-
                  (f2cl-lib:fref w-%data% ((f2cl-lib:int-add id2 1)) ((1 1))
                                 w-%offset%)
                  (*
                   (f2cl-lib:fref w-%data% ((f2cl-lib:int-add id3 1)) ((1 1))
                                  w-%offset%)
                   (f2cl-lib:fref w-%data% ((f2cl-lib:int-add id5 2)) ((1 1))
                                  w-%offset%)))))
      (f2cl-lib:fdo (i 2 (f2cl-lib:int-add i 1))
                    ((> i munk) nil)
        (tagbody
          (setf j (f2cl-lib:int-add id5 i))
          (setf (f2cl-lib:fref w-%data% (j) ((1 1)) w-%offset%)
                  (* (- (f2cl-lib:fref w-%data% (j) ((1 1)) w-%offset%))
                     (f2cl-lib:fref w-%data% ((f2cl-lib:int-sub j 1)) ((1 1))
                                    w-%offset%)))
         label150))
      (setf s 0.0)
      (f2cl-lib:fdo (j nstart (f2cl-lib:int-add j 1))
                    ((> j nstop) nil)
        (tagbody
          (setf s
                  (+ s
                     (f2cl-lib:fref f-%data% (2 j) ((1 idimf) (1 1))
                                    f-%offset%)))
         label151))
      (setf a2 (coerce (the f2cl-lib:integer4 nunk) 'single-float))
      (if (= nbdcnd 0)
          (go label152))
      (setf s
              (- s
                 (* 0.5
                    (+
                     (f2cl-lib:fref f-%data% (2 nstart) ((1 idimf) (1 1))
                                    f-%offset%)
                     (f2cl-lib:fref f-%data% (2 nstop) ((1 idimf) (1 1))
                                    f-%offset%)))))
      (setf a2 (- a2 1.0))
     label152
      (setf ypole
              (/
               (+
                (* 0.25 dlrsq
                   (f2cl-lib:fref f-%data% (1 1) ((1 idimf) (1 1)) f-%offset%))
                (/ (- s) a2))
               (+
                (-
                 (f2cl-lib:fref w-%data% ((f2cl-lib:int-add id5 1)) ((1 1))
                                w-%offset%)
                 1.0)
                (* elmbda dlrsq 0.25))))
      (f2cl-lib:fdo (i mstart (f2cl-lib:int-add i 1))
                    ((> i mstop) nil)
        (tagbody
          (setf k (f2cl-lib:int-add l i))
          (f2cl-lib:fdo (j nstart (f2cl-lib:int-add j 1))
                        ((> j nstop) nil)
            (tagbody
              (setf (f2cl-lib:fref f-%data% (i j) ((1 idimf) (1 1)) f-%offset%)
                      (+
                       (f2cl-lib:fref f-%data% (i j) ((1 idimf) (1 1))
                                      f-%offset%)
                       (* ypole
                          (f2cl-lib:fref w-%data% (k) ((1 1)) w-%offset%))))
             label153))
         label154))
     label155
      (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                    ((> j np1) nil)
        (tagbody
          (setf (f2cl-lib:fref f-%data% (1 j) ((1 idimf) (1 1)) f-%offset%)
                  ypole)
         label156))
     label157
      (if (/= nbdcnd 0)
          (go label159))
      (f2cl-lib:fdo (i mstart (f2cl-lib:int-add i 1))
                    ((> i mstop) nil)
        (tagbody
          (setf (f2cl-lib:fref f-%data% (i np1) ((1 idimf) (1 1)) f-%offset%)
                  (f2cl-lib:fref f-%data% (i 1) ((1 idimf) (1 1)) f-%offset%))
         label158))
     label159
      (setf (f2cl-lib:fref w-%data% (1) ((1 1)) w-%offset%)
              (coerce (the f2cl-lib:integer4 iwstor) 'single-float))
      (go end_label)
     end_label
      (return
       (values nil nil nil nil nil nil nil nil nil nil nil nil nil nil idimf
               pertrb ierror nil)))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::hwsplr
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

