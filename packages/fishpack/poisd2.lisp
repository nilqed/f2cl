;;; Compiled by f2cl version:
;;; ("" "" "" "" "" "" "")

;;; Using Lisp SBCL 1.4.2
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing t) (:declare-common nil)
;;;           (:float-format single-float))

(in-package "FISHPACK")


(defun poisd2 (mr nr istag ba bb bc q idimq b w d tcos p)
  (declare (type (array single-float (*)) q)
           (type (array single-float (*)) p tcos d w b bc bb ba)
           (type (f2cl-lib:integer4) idimq istag nr mr))
  (f2cl-lib:with-multi-array-data
      ((ba single-float ba-%data% ba-%offset%)
       (bb single-float bb-%data% bb-%offset%)
       (bc single-float bc-%data% bc-%offset%)
       (b single-float b-%data% b-%offset%)
       (w single-float w-%data% w-%offset%)
       (d single-float d-%data% d-%offset%)
       (tcos single-float tcos-%data% tcos-%offset%)
       (p single-float p-%data% p-%offset%)
       (q single-float q-%data% q-%offset%))
    (prog ((jdeg 0) (noddpr 0) (ideg 0) (krpi 0) (ip1 0) (t$ 0.0) (jp3 0)
           (jm3 0) (jp2 0) (jm2 0) (jp1 0) (jm1 0) (j 0) (nodd 0) (l 0) (jsp 0)
           (jst 0) (nun 0) (lr 0) (i 0) (jstsav 0) (irreg 0) (kr 0) (ipstor 0)
           (ip 0) (fi 0.0) (jsh 0) (n 0) (m 0))
      (declare (type (single-float) fi t$)
               (type (f2cl-lib:integer4) m n jsh ip ipstor kr irreg jstsav i lr
                nun jst jsp l nodd j jm1 jp1 jm2 jp2 jm3 jp3 ip1 krpi ideg
                noddpr jdeg))
      (setf m mr)
      (setf n nr)
      (setf jsh 0)
      (setf fi (/ 1.0 (f2cl-lib:ffloat istag)))
      (setf ip (f2cl-lib:int-sub m))
      (setf ipstor 0)
      (f2cl-lib:computed-goto (label101 label102) istag)
     label101
      (setf kr 0)
      (setf irreg 1)
      (if (> n 1)
          (go label106))
      (setf (f2cl-lib:fref tcos-%data% (1) ((1 1)) tcos-%offset%) 0.0)
      (go label103)
     label102
      (setf kr 1)
      (setf jstsav 1)
      (setf irreg 2)
      (if (> n 1)
          (go label106))
      (setf (f2cl-lib:fref tcos-%data% (1) ((1 1)) tcos-%offset%) -1.0)
     label103
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i m) nil)
        (tagbody
          (setf (f2cl-lib:fref b-%data% (i) ((1 1)) b-%offset%)
                  (f2cl-lib:fref q-%data% (i 1) ((1 idimq) (1 1)) q-%offset%))
         label104))
      (trix 1 0 m ba bb bc b tcos d w)
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i m) nil)
        (tagbody
          (setf (f2cl-lib:fref q-%data% (i 1) ((1 idimq) (1 1)) q-%offset%)
                  (f2cl-lib:fref b-%data% (i) ((1 1)) b-%offset%))
         label105))
      (go label183)
     label106
      (setf lr 0)
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i m) nil)
        (tagbody
          (setf (f2cl-lib:fref p-%data% (i) ((1 1)) p-%offset%) 0.0)
         label107))
      (setf nun n)
      (setf jst 1)
      (setf jsp n)
     label108
      (setf l (f2cl-lib:int-mul 2 jst))
      (setf nodd
              (+ (- 2 (* 2 (the f2cl-lib:integer4 (truncate (+ nun 1) 2))))
                 nun))
      (f2cl-lib:computed-goto (label110 label109) nodd)
     label109
      (setf jsp (f2cl-lib:int-sub jsp l))
      (go label111)
     label110
      (setf jsp (f2cl-lib:int-sub jsp jst))
      (if (/= irreg 1)
          (setf jsp (f2cl-lib:int-sub jsp l)))
     label111
      (cosgen jst 1 0.5 0.0 tcos)
      (if (> l jsp)
          (go label118))
      (f2cl-lib:fdo (j l (f2cl-lib:int-add j l))
                    ((> j jsp) nil)
        (tagbody
          (setf jm1 (f2cl-lib:int-sub j jsh))
          (setf jp1 (f2cl-lib:int-add j jsh))
          (setf jm2 (f2cl-lib:int-sub j jst))
          (setf jp2 (f2cl-lib:int-add j jst))
          (setf jm3 (f2cl-lib:int-sub jm2 jsh))
          (setf jp3 (f2cl-lib:int-add jp2 jsh))
          (if (/= jst 1)
              (go label113))
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i m) nil)
            (tagbody
              (setf (f2cl-lib:fref b-%data% (i) ((1 1)) b-%offset%)
                      (* 2.0
                         (f2cl-lib:fref q-%data% (i j) ((1 idimq) (1 1))
                                        q-%offset%)))
              (setf (f2cl-lib:fref q-%data% (i j) ((1 idimq) (1 1)) q-%offset%)
                      (+
                       (f2cl-lib:fref q-%data% (i jm2) ((1 idimq) (1 1))
                                      q-%offset%)
                       (f2cl-lib:fref q-%data% (i jp2) ((1 idimq) (1 1))
                                      q-%offset%)))
             label112))
          (go label115)
         label113
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i m) nil)
            (tagbody
              (setf t$
                      (+
                       (-
                        (f2cl-lib:fref q-%data% (i j) ((1 idimq) (1 1))
                                       q-%offset%)
                        (f2cl-lib:fref q-%data% (i jm1) ((1 idimq) (1 1))
                                       q-%offset%)
                        (f2cl-lib:fref q-%data% (i jp1) ((1 idimq) (1 1))
                                       q-%offset%))
                       (f2cl-lib:fref q-%data% (i jm2) ((1 idimq) (1 1))
                                      q-%offset%)
                       (f2cl-lib:fref q-%data% (i jp2) ((1 idimq) (1 1))
                                      q-%offset%)))
              (setf (f2cl-lib:fref b-%data% (i) ((1 1)) b-%offset%)
                      (-
                       (+ t$
                          (f2cl-lib:fref q-%data% (i j) ((1 idimq) (1 1))
                                         q-%offset%))
                       (f2cl-lib:fref q-%data% (i jm3) ((1 idimq) (1 1))
                                      q-%offset%)
                       (f2cl-lib:fref q-%data% (i jp3) ((1 idimq) (1 1))
                                      q-%offset%)))
              (setf (f2cl-lib:fref q-%data% (i j) ((1 idimq) (1 1)) q-%offset%)
                      t$)
             label114))
         label115
          (trix jst 0 m ba bb bc b tcos d w)
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i m) nil)
            (tagbody
              (setf (f2cl-lib:fref q-%data% (i j) ((1 idimq) (1 1)) q-%offset%)
                      (+
                       (f2cl-lib:fref q-%data% (i j) ((1 idimq) (1 1))
                                      q-%offset%)
                       (f2cl-lib:fref b-%data% (i) ((1 1)) b-%offset%)))
             label116))
         label117))
     label118
      (f2cl-lib:computed-goto (label119 label136) nodd)
     label119
      (f2cl-lib:computed-goto (label152 label120) irreg)
     label120
      (setf jsp (f2cl-lib:int-add jsp l))
      (setf j jsp)
      (setf jm1 (f2cl-lib:int-sub j jsh))
      (setf jp1 (f2cl-lib:int-add j jsh))
      (setf jm2 (f2cl-lib:int-sub j jst))
      (setf jp2 (f2cl-lib:int-add j jst))
      (setf jm3 (f2cl-lib:int-sub jm2 jsh))
      (f2cl-lib:computed-goto (label123 label121) istag)
     label121
      (if (/= jst 1)
          (go label123))
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i m) nil)
        (tagbody
          (setf (f2cl-lib:fref b-%data% (i) ((1 1)) b-%offset%)
                  (f2cl-lib:fref q-%data% (i j) ((1 idimq) (1 1)) q-%offset%))
          (setf (f2cl-lib:fref q-%data% (i j) ((1 idimq) (1 1)) q-%offset%)
                  0.0)
         label122))
      (go label130)
     label123
      (f2cl-lib:computed-goto (label124 label126) noddpr)
     label124
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i m) nil)
        (tagbody
          (setf ip1 (f2cl-lib:int-add ip i))
          (setf (f2cl-lib:fref b-%data% (i) ((1 1)) b-%offset%)
                  (+
                   (* 0.5
                      (-
                       (f2cl-lib:fref q-%data% (i jm2) ((1 idimq) (1 1))
                                      q-%offset%)
                       (f2cl-lib:fref q-%data% (i jm1) ((1 idimq) (1 1))
                                      q-%offset%)
                       (f2cl-lib:fref q-%data% (i jm3) ((1 idimq) (1 1))
                                      q-%offset%)))
                   (f2cl-lib:fref p-%data% (ip1) ((1 1)) p-%offset%)
                   (f2cl-lib:fref q-%data% (i j) ((1 idimq) (1 1))
                                  q-%offset%)))
         label125))
      (go label128)
     label126
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i m) nil)
        (tagbody
          (setf (f2cl-lib:fref b-%data% (i) ((1 1)) b-%offset%)
                  (+
                   (-
                    (+
                     (* 0.5
                        (-
                         (f2cl-lib:fref q-%data% (i jm2) ((1 idimq) (1 1))
                                        q-%offset%)
                         (f2cl-lib:fref q-%data% (i jm1) ((1 idimq) (1 1))
                                        q-%offset%)
                         (f2cl-lib:fref q-%data% (i jm3) ((1 idimq) (1 1))
                                        q-%offset%)))
                     (f2cl-lib:fref q-%data% (i jp2) ((1 idimq) (1 1))
                                    q-%offset%))
                    (f2cl-lib:fref q-%data% (i jp1) ((1 idimq) (1 1))
                                   q-%offset%))
                   (f2cl-lib:fref q-%data% (i j) ((1 idimq) (1 1))
                                  q-%offset%)))
         label127))
     label128
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i m) nil)
        (tagbody
          (setf (f2cl-lib:fref q-%data% (i j) ((1 idimq) (1 1)) q-%offset%)
                  (* 0.5
                     (-
                      (f2cl-lib:fref q-%data% (i j) ((1 idimq) (1 1))
                                     q-%offset%)
                      (f2cl-lib:fref q-%data% (i jm1) ((1 idimq) (1 1))
                                     q-%offset%)
                      (f2cl-lib:fref q-%data% (i jp1) ((1 idimq) (1 1))
                                     q-%offset%))))
         label129))
     label130
      (trix jst 0 m ba bb bc b tcos d w)
      (setf ip (f2cl-lib:int-add ip m))
      (setf ipstor (f2cl-lib:max0 ipstor (f2cl-lib:int-add ip m)))
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i m) nil)
        (tagbody
          (setf ip1 (f2cl-lib:int-add ip i))
          (setf (f2cl-lib:fref p-%data% (ip1) ((1 1)) p-%offset%)
                  (+
                   (f2cl-lib:fref q-%data% (i j) ((1 idimq) (1 1)) q-%offset%)
                   (f2cl-lib:fref b-%data% (i) ((1 1)) b-%offset%)))
          (setf (f2cl-lib:fref b-%data% (i) ((1 1)) b-%offset%)
                  (+
                   (f2cl-lib:fref q-%data% (i jp2) ((1 idimq) (1 1))
                                  q-%offset%)
                   (f2cl-lib:fref p-%data% (ip1) ((1 1)) p-%offset%)))
         label131))
      (if (/= lr 0)
          (go label133))
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i jst) nil)
        (tagbody
          (setf krpi (f2cl-lib:int-add kr i))
          (setf (f2cl-lib:fref tcos-%data% (krpi) ((1 1)) tcos-%offset%)
                  (f2cl-lib:fref tcos-%data% (i) ((1 1)) tcos-%offset%))
         label132))
      (go label134)
     label133
      (cosgen lr jstsav 0.0 fi
       (f2cl-lib:array-slice tcos-%data% single-float ((+ jst 1)) ((1 1))
                             tcos-%offset%))
      (merge$ tcos 0 jst jst lr kr)
     label134
      (cosgen kr jstsav 0.0 fi tcos)
      (trix kr kr m ba bb bc b tcos d w)
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i m) nil)
        (tagbody
          (setf ip1 (f2cl-lib:int-add ip i))
          (setf (f2cl-lib:fref q-%data% (i j) ((1 idimq) (1 1)) q-%offset%)
                  (+
                   (f2cl-lib:fref q-%data% (i jm2) ((1 idimq) (1 1))
                                  q-%offset%)
                   (f2cl-lib:fref b-%data% (i) ((1 1)) b-%offset%)
                   (f2cl-lib:fref p-%data% (ip1) ((1 1)) p-%offset%)))
         label135))
      (setf lr kr)
      (setf kr (f2cl-lib:int-add kr l))
      (go label152)
     label136
      (setf jsp (f2cl-lib:int-add jsp l))
      (setf j jsp)
      (setf jm1 (f2cl-lib:int-sub j jsh))
      (setf jp1 (f2cl-lib:int-add j jsh))
      (setf jm2 (f2cl-lib:int-sub j jst))
      (setf jp2 (f2cl-lib:int-add j jst))
      (setf jm3 (f2cl-lib:int-sub jm2 jsh))
      (f2cl-lib:computed-goto (label137 label138) irreg)
     label137
      (setf jstsav jst)
      (setf ideg jst)
      (setf kr l)
      (go label139)
     label138
      (cosgen kr jstsav 0.0 fi tcos)
      (cosgen lr jstsav 0.0 fi
       (f2cl-lib:array-slice tcos-%data% single-float ((+ kr 1)) ((1 1))
                             tcos-%offset%))
      (setf ideg kr)
      (setf kr (f2cl-lib:int-add kr jst))
     label139
      (if (/= jst 1)
          (go label141))
      (setf irreg 2)
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i m) nil)
        (tagbody
          (setf (f2cl-lib:fref b-%data% (i) ((1 1)) b-%offset%)
                  (f2cl-lib:fref q-%data% (i j) ((1 idimq) (1 1)) q-%offset%))
          (setf (f2cl-lib:fref q-%data% (i j) ((1 idimq) (1 1)) q-%offset%)
                  (f2cl-lib:fref q-%data% (i jm2) ((1 idimq) (1 1))
                                 q-%offset%))
         label140))
      (go label150)
     label141
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i m) nil)
        (tagbody
          (setf (f2cl-lib:fref b-%data% (i) ((1 1)) b-%offset%)
                  (+
                   (f2cl-lib:fref q-%data% (i j) ((1 idimq) (1 1)) q-%offset%)
                   (* 0.5
                      (-
                       (f2cl-lib:fref q-%data% (i jm2) ((1 idimq) (1 1))
                                      q-%offset%)
                       (f2cl-lib:fref q-%data% (i jm1) ((1 idimq) (1 1))
                                      q-%offset%)
                       (f2cl-lib:fref q-%data% (i jm3) ((1 idimq) (1 1))
                                      q-%offset%)))))
         label142))
      (f2cl-lib:computed-goto (label143 label145) irreg)
     label143
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i m) nil)
        (tagbody
          (setf (f2cl-lib:fref q-%data% (i j) ((1 idimq) (1 1)) q-%offset%)
                  (+
                   (f2cl-lib:fref q-%data% (i jm2) ((1 idimq) (1 1))
                                  q-%offset%)
                   (* 0.5
                      (-
                       (f2cl-lib:fref q-%data% (i j) ((1 idimq) (1 1))
                                      q-%offset%)
                       (f2cl-lib:fref q-%data% (i jm1) ((1 idimq) (1 1))
                                      q-%offset%)
                       (f2cl-lib:fref q-%data% (i jp1) ((1 idimq) (1 1))
                                      q-%offset%)))))
         label144))
      (setf irreg 2)
      (go label150)
     label145
      (f2cl-lib:computed-goto (label146 label148) noddpr)
     label146
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i m) nil)
        (tagbody
          (setf ip1 (f2cl-lib:int-add ip i))
          (setf (f2cl-lib:fref q-%data% (i j) ((1 idimq) (1 1)) q-%offset%)
                  (+
                   (f2cl-lib:fref q-%data% (i jm2) ((1 idimq) (1 1))
                                  q-%offset%)
                   (f2cl-lib:fref p-%data% (ip1) ((1 1)) p-%offset%)))
         label147))
      (setf ip (f2cl-lib:int-sub ip m))
      (go label150)
     label148
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i m) nil)
        (tagbody
          (setf (f2cl-lib:fref q-%data% (i j) ((1 idimq) (1 1)) q-%offset%)
                  (-
                   (+
                    (f2cl-lib:fref q-%data% (i jm2) ((1 idimq) (1 1))
                                   q-%offset%)
                    (f2cl-lib:fref q-%data% (i j) ((1 idimq) (1 1))
                                   q-%offset%))
                   (f2cl-lib:fref q-%data% (i jm1) ((1 idimq) (1 1))
                                  q-%offset%)))
         label149))
     label150
      (trix ideg lr m ba bb bc b tcos d w)
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i m) nil)
        (tagbody
          (setf (f2cl-lib:fref q-%data% (i j) ((1 idimq) (1 1)) q-%offset%)
                  (+
                   (f2cl-lib:fref q-%data% (i j) ((1 idimq) (1 1)) q-%offset%)
                   (f2cl-lib:fref b-%data% (i) ((1 1)) b-%offset%)))
         label151))
     label152
      (setf nun (the f2cl-lib:integer4 (truncate nun 2)))
      (setf noddpr nodd)
      (setf jsh jst)
      (setf jst (f2cl-lib:int-mul 2 jst))
      (if (>= nun 2)
          (go label108))
      (setf j jsp)
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i m) nil)
        (tagbody
          (setf (f2cl-lib:fref b-%data% (i) ((1 1)) b-%offset%)
                  (f2cl-lib:fref q-%data% (i j) ((1 idimq) (1 1)) q-%offset%))
         label153))
      (f2cl-lib:computed-goto (label154 label155) irreg)
     label154
      (cosgen jst 1 0.5 0.0 tcos)
      (setf ideg jst)
      (go label156)
     label155
      (setf kr (f2cl-lib:int-add lr jst))
      (cosgen kr jstsav 0.0 fi tcos)
      (cosgen lr jstsav 0.0 fi
       (f2cl-lib:array-slice tcos-%data% single-float ((+ kr 1)) ((1 1))
                             tcos-%offset%))
      (setf ideg kr)
     label156
      (trix ideg lr m ba bb bc b tcos d w)
      (setf jm1 (f2cl-lib:int-sub j jsh))
      (setf jp1 (f2cl-lib:int-add j jsh))
      (f2cl-lib:computed-goto (label157 label159) irreg)
     label157
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i m) nil)
        (tagbody
          (setf (f2cl-lib:fref q-%data% (i j) ((1 idimq) (1 1)) q-%offset%)
                  (+
                   (* 0.5
                      (-
                       (f2cl-lib:fref q-%data% (i j) ((1 idimq) (1 1))
                                      q-%offset%)
                       (f2cl-lib:fref q-%data% (i jm1) ((1 idimq) (1 1))
                                      q-%offset%)
                       (f2cl-lib:fref q-%data% (i jp1) ((1 idimq) (1 1))
                                      q-%offset%)))
                   (f2cl-lib:fref b-%data% (i) ((1 1)) b-%offset%)))
         label158))
      (go label164)
     label159
      (f2cl-lib:computed-goto (label160 label162) noddpr)
     label160
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i m) nil)
        (tagbody
          (setf ip1 (f2cl-lib:int-add ip i))
          (setf (f2cl-lib:fref q-%data% (i j) ((1 idimq) (1 1)) q-%offset%)
                  (+ (f2cl-lib:fref p-%data% (ip1) ((1 1)) p-%offset%)
                     (f2cl-lib:fref b-%data% (i) ((1 1)) b-%offset%)))
         label161))
      (setf ip (f2cl-lib:int-sub ip m))
      (go label164)
     label162
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i m) nil)
        (tagbody
          (setf (f2cl-lib:fref q-%data% (i j) ((1 idimq) (1 1)) q-%offset%)
                  (+
                   (-
                    (f2cl-lib:fref q-%data% (i j) ((1 idimq) (1 1)) q-%offset%)
                    (f2cl-lib:fref q-%data% (i jm1) ((1 idimq) (1 1))
                                   q-%offset%))
                   (f2cl-lib:fref b-%data% (i) ((1 1)) b-%offset%)))
         label163))
     label164
      (setf jst (the f2cl-lib:integer4 (truncate jst 2)))
      (setf jsh (the f2cl-lib:integer4 (truncate jst 2)))
      (setf nun (f2cl-lib:int-mul 2 nun))
      (if (> nun n)
          (go label183))
      (f2cl-lib:fdo (j jst (f2cl-lib:int-add j l))
                    ((> j n) nil)
        (tagbody
          (setf jm1 (f2cl-lib:int-sub j jsh))
          (setf jp1 (f2cl-lib:int-add j jsh))
          (setf jm2 (f2cl-lib:int-sub j jst))
          (setf jp2 (f2cl-lib:int-add j jst))
          (if (> j jst)
              (go label166))
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i m) nil)
            (tagbody
              (setf (f2cl-lib:fref b-%data% (i) ((1 1)) b-%offset%)
                      (+
                       (f2cl-lib:fref q-%data% (i j) ((1 idimq) (1 1))
                                      q-%offset%)
                       (f2cl-lib:fref q-%data% (i jp2) ((1 idimq) (1 1))
                                      q-%offset%)))
             label165))
          (go label170)
         label166
          (if (<= jp2 n)
              (go label168))
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i m) nil)
            (tagbody
              (setf (f2cl-lib:fref b-%data% (i) ((1 1)) b-%offset%)
                      (+
                       (f2cl-lib:fref q-%data% (i j) ((1 idimq) (1 1))
                                      q-%offset%)
                       (f2cl-lib:fref q-%data% (i jm2) ((1 idimq) (1 1))
                                      q-%offset%)))
             label167))
          (if (< jst jstsav)
              (setf irreg 1))
          (f2cl-lib:computed-goto (label170 label171) irreg)
         label168
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i m) nil)
            (tagbody
              (setf (f2cl-lib:fref b-%data% (i) ((1 1)) b-%offset%)
                      (+
                       (f2cl-lib:fref q-%data% (i j) ((1 idimq) (1 1))
                                      q-%offset%)
                       (f2cl-lib:fref q-%data% (i jm2) ((1 idimq) (1 1))
                                      q-%offset%)
                       (f2cl-lib:fref q-%data% (i jp2) ((1 idimq) (1 1))
                                      q-%offset%)))
             label169))
         label170
          (cosgen jst 1 0.5 0.0 tcos)
          (setf ideg jst)
          (setf jdeg 0)
          (go label172)
         label171
          (if (> (f2cl-lib:int-add j l) n)
              (setf lr (f2cl-lib:int-sub lr jst)))
          (setf kr (f2cl-lib:int-add jst lr))
          (cosgen kr jstsav 0.0 fi tcos)
          (cosgen lr jstsav 0.0 fi
           (f2cl-lib:array-slice tcos-%data% single-float ((+ kr 1)) ((1 1))
                                 tcos-%offset%))
          (setf ideg kr)
          (setf jdeg lr)
         label172
          (trix ideg jdeg m ba bb bc b tcos d w)
          (if (> jst 1)
              (go label174))
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i m) nil)
            (tagbody
              (setf (f2cl-lib:fref q-%data% (i j) ((1 idimq) (1 1)) q-%offset%)
                      (f2cl-lib:fref b-%data% (i) ((1 1)) b-%offset%))
             label173))
          (go label182)
         label174
          (if (> jp2 n)
              (go label177))
         label175
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i m) nil)
            (tagbody
              (setf (f2cl-lib:fref q-%data% (i j) ((1 idimq) (1 1)) q-%offset%)
                      (+
                       (* 0.5
                          (-
                           (f2cl-lib:fref q-%data% (i j) ((1 idimq) (1 1))
                                          q-%offset%)
                           (f2cl-lib:fref q-%data% (i jm1) ((1 idimq) (1 1))
                                          q-%offset%)
                           (f2cl-lib:fref q-%data% (i jp1) ((1 idimq) (1 1))
                                          q-%offset%)))
                       (f2cl-lib:fref b-%data% (i) ((1 1)) b-%offset%)))
             label176))
          (go label182)
         label177
          (f2cl-lib:computed-goto (label175 label178) irreg)
         label178
          (if (> (f2cl-lib:int-add j jsh) n)
              (go label180))
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i m) nil)
            (tagbody
              (setf ip1 (f2cl-lib:int-add ip i))
              (setf (f2cl-lib:fref q-%data% (i j) ((1 idimq) (1 1)) q-%offset%)
                      (+ (f2cl-lib:fref b-%data% (i) ((1 1)) b-%offset%)
                         (f2cl-lib:fref p-%data% (ip1) ((1 1)) p-%offset%)))
             label179))
          (setf ip (f2cl-lib:int-sub ip m))
          (go label182)
         label180
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i m) nil)
            (tagbody
              (setf (f2cl-lib:fref q-%data% (i j) ((1 idimq) (1 1)) q-%offset%)
                      (-
                       (+ (f2cl-lib:fref b-%data% (i) ((1 1)) b-%offset%)
                          (f2cl-lib:fref q-%data% (i j) ((1 idimq) (1 1))
                                         q-%offset%))
                       (f2cl-lib:fref q-%data% (i jm1) ((1 idimq) (1 1))
                                      q-%offset%)))
             label181))
         label182))
      (setf l (the f2cl-lib:integer4 (truncate l 2)))
      (go label164)
     label183
      (setf (f2cl-lib:fref w-%data% (1) ((1 1)) w-%offset%)
              (coerce (the f2cl-lib:integer4 ipstor) 'single-float))
      (go end_label)
     end_label
      (return (values nil nil nil nil nil nil nil nil nil nil nil nil nil)))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::poisd2
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo :arg-types
                                            '((fortran-to-lisp::integer4)
                                              (fortran-to-lisp::integer4)
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
                                              (array single-float (*)))
                                            :return-values
                                            '(nil nil nil nil nil nil nil nil
                                              nil nil nil nil nil)
                                            :calls
                                            '(fortran-to-lisp::merge$
                                              fortran-to-lisp::cosgen
                                              fortran-to-lisp::trix))))

