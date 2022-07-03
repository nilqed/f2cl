;;; Compiled by f2cl version:
;;; ("" "" "" "" "" "" "")

;;; Using Lisp SBCL 1.4.2
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing t) (:declare-common nil)
;;;           (:float-format single-float))

(in-package "FISHPACK")


(defun poisn2 (m n istag mixbnd a bb c q idimq b b2 b3 w w2 w3 d tcos p)
  (declare (type (array single-float (*)) q)
           (type (array single-float (*)) p tcos d w3 w2 w b3 b2 b c bb a)
           (type (f2cl-lib:integer4) idimq mixbnd istag n m))
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
    (symbol-macrolet ((k1 (f2cl-lib:fref k (1) ((1 4))))
                      (k2 (f2cl-lib:fref k (2) ((1 4))))
                      (k3 (f2cl-lib:fref k (3) ((1 4))))
                      (k4 (f2cl-lib:fref k (4) ((1 4)))))
      (prog ((k (make-array 4 :element-type 'f2cl-lib:integer4)) (jstep 0)
             (nlastp 0) (t$ 0.0) (jr2 0) (i2 0) (i1 0) (nrodpr 0) (ii 0)
             (fi 0.0) (jm3 0) (jm2 0) (jm1 0) (jp3 0) (jp2 0) (jp1 0) (j 0)
             (i2rby2 0) (jstop 0) (jstart 0) (nrod 0) (i 0) (lr 0) (kr 0)
             (nlast 0) (nr 0) (jr 0) (i2r 0) (ipstor 0) (ip 0) (mr 0)
             (fden 0.0) (fnum 0.0) (fistag 0.0))
        (declare (type (single-float) fistag fnum fden fi t$)
                 (type (f2cl-lib:integer4) mr ip ipstor i2r jr nr nlast kr lr i
                  nrod jstart jstop i2rby2 j jp1 jp2 jp3 jm1 jm2 jm3 ii nrodpr
                  i1 i2 jr2 k1 k2 k4 k3 nlastp jstep)
                 (type (array f2cl-lib:integer4 (4)) k))
        (setf fistag
                (coerce (the f2cl-lib:integer4 (f2cl-lib:int-sub 3 istag))
                        'single-float))
        (setf fnum (/ 1.0 (f2cl-lib:ffloat istag)))
        (setf fden (* 0.5 (f2cl-lib:ffloat (f2cl-lib:int-sub istag 1))))
        (setf mr m)
        (setf ip (f2cl-lib:int-sub mr))
        (setf ipstor 0)
        (setf i2r 1)
        (setf jr 2)
        (setf nr n)
        (setf nlast n)
        (setf kr 1)
        (setf lr 0)
        (f2cl-lib:computed-goto (label101 label103) istag)
       label101
        (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                      ((> i mr) nil)
          (tagbody
            (setf (f2cl-lib:fref q-%data% (i n) ((1 idimq) (1 1)) q-%offset%)
                    (* 0.5
                       (f2cl-lib:fref q-%data% (i n) ((1 idimq) (1 1))
                                      q-%offset%)))
           label102))
        (f2cl-lib:computed-goto (label103 label104) mixbnd)
       label103
        (if (<= n 3)
            (go label155))
       label104
        (setf jr (f2cl-lib:int-mul 2 i2r))
        (setf nrod 1)
        (if (= (* (the f2cl-lib:integer4 (truncate nr 2)) 2) nr)
            (setf nrod 0))
        (f2cl-lib:computed-goto (label105 label106) mixbnd)
       label105
        (setf jstart 1)
        (go label107)
       label106
        (setf jstart jr)
        (setf nrod (f2cl-lib:int-sub 1 nrod))
       label107
        (setf jstop (f2cl-lib:int-sub nlast jr))
        (if (= nrod 0)
            (setf jstop (f2cl-lib:int-sub jstop i2r)))
        (cosgen i2r 1 0.5 0.0 tcos)
        (setf i2rby2 (the f2cl-lib:integer4 (truncate i2r 2)))
        (if (>= jstop jstart)
            (go label108))
        (setf j jr)
        (go label116)
       label108
        (f2cl-lib:fdo (j jstart (f2cl-lib:int-add j jr))
                      ((> j jstop) nil)
          (tagbody
            (setf jp1 (f2cl-lib:int-add j i2rby2))
            (setf jp2 (f2cl-lib:int-add j i2r))
            (setf jp3 (f2cl-lib:int-add jp2 i2rby2))
            (setf jm1 (f2cl-lib:int-sub j i2rby2))
            (setf jm2 (f2cl-lib:int-sub j i2r))
            (setf jm3 (f2cl-lib:int-sub jm2 i2rby2))
            (if (/= j 1)
                (go label109))
            (setf jm1 jp1)
            (setf jm2 jp2)
            (setf jm3 jp3)
           label109
            (if (/= i2r 1)
                (go label111))
            (if (= j 1)
                (setf jm2 jp2))
            (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                          ((> i mr) nil)
              (tagbody
                (setf (f2cl-lib:fref b-%data% (i) ((1 1)) b-%offset%)
                        (* 2.0
                           (f2cl-lib:fref q-%data% (i j) ((1 idimq) (1 1))
                                          q-%offset%)))
                (setf (f2cl-lib:fref q-%data% (i j) ((1 idimq) (1 1))
                                     q-%offset%)
                        (+
                         (f2cl-lib:fref q-%data% (i jm2) ((1 idimq) (1 1))
                                        q-%offset%)
                         (f2cl-lib:fref q-%data% (i jp2) ((1 idimq) (1 1))
                                        q-%offset%)))
               label110))
            (go label113)
           label111
            (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                          ((> i mr) nil)
              (tagbody
                (setf fi
                        (f2cl-lib:fref q-%data% (i j) ((1 idimq) (1 1))
                                       q-%offset%))
                (setf (f2cl-lib:fref q-%data% (i j) ((1 idimq) (1 1))
                                     q-%offset%)
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
                         (+ fi
                            (f2cl-lib:fref q-%data% (i j) ((1 idimq) (1 1))
                                           q-%offset%))
                         (f2cl-lib:fref q-%data% (i jm3) ((1 idimq) (1 1))
                                        q-%offset%)
                         (f2cl-lib:fref q-%data% (i jp3) ((1 idimq) (1 1))
                                        q-%offset%)))
               label112))
           label113
            (trix i2r 0 mr a bb c b tcos d w)
            (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                          ((> i mr) nil)
              (tagbody
                (setf (f2cl-lib:fref q-%data% (i j) ((1 idimq) (1 1))
                                     q-%offset%)
                        (+
                         (f2cl-lib:fref q-%data% (i j) ((1 idimq) (1 1))
                                        q-%offset%)
                         (f2cl-lib:fref b-%data% (i) ((1 1)) b-%offset%)))
               label114))
           label115))
        (setf j (f2cl-lib:int-add jstop jr))
       label116
        (setf nlast j)
        (setf jm1 (f2cl-lib:int-sub j i2rby2))
        (setf jm2 (f2cl-lib:int-sub j i2r))
        (setf jm3 (f2cl-lib:int-sub jm2 i2rby2))
        (if (= nrod 0)
            (go label128))
        (if (/= i2r 1)
            (go label118))
        (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                      ((> i mr) nil)
          (tagbody
            (setf (f2cl-lib:fref b-%data% (i) ((1 1)) b-%offset%)
                    (* fistag
                       (f2cl-lib:fref q-%data% (i j) ((1 idimq) (1 1))
                                      q-%offset%)))
            (setf (f2cl-lib:fref q-%data% (i j) ((1 idimq) (1 1)) q-%offset%)
                    (f2cl-lib:fref q-%data% (i jm2) ((1 idimq) (1 1))
                                   q-%offset%))
           label117))
        (go label126)
       label118
        (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                      ((> i mr) nil)
          (tagbody
            (setf (f2cl-lib:fref b-%data% (i) ((1 1)) b-%offset%)
                    (+
                     (f2cl-lib:fref q-%data% (i j) ((1 idimq) (1 1))
                                    q-%offset%)
                     (* 0.5
                        (-
                         (f2cl-lib:fref q-%data% (i jm2) ((1 idimq) (1 1))
                                        q-%offset%)
                         (f2cl-lib:fref q-%data% (i jm1) ((1 idimq) (1 1))
                                        q-%offset%)
                         (f2cl-lib:fref q-%data% (i jm3) ((1 idimq) (1 1))
                                        q-%offset%)))))
           label119))
        (if (/= nrodpr 0)
            (go label121))
        (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                      ((> i mr) nil)
          (tagbody
            (setf ii (f2cl-lib:int-add ip i))
            (setf (f2cl-lib:fref q-%data% (i j) ((1 idimq) (1 1)) q-%offset%)
                    (+
                     (f2cl-lib:fref q-%data% (i jm2) ((1 idimq) (1 1))
                                    q-%offset%)
                     (f2cl-lib:fref p-%data% (ii) ((1 1)) p-%offset%)))
           label120))
        (setf ip (f2cl-lib:int-sub ip mr))
        (go label123)
       label121
        (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                      ((> i mr) nil)
          (tagbody
            (setf (f2cl-lib:fref q-%data% (i j) ((1 idimq) (1 1)) q-%offset%)
                    (+
                     (-
                      (f2cl-lib:fref q-%data% (i j) ((1 idimq) (1 1))
                                     q-%offset%)
                      (f2cl-lib:fref q-%data% (i jm1) ((1 idimq) (1 1))
                                     q-%offset%))
                     (f2cl-lib:fref q-%data% (i jm2) ((1 idimq) (1 1))
                                    q-%offset%)))
           label122))
       label123
        (if (= lr 0)
            (go label124))
        (cosgen lr 1 0.5 fden
         (f2cl-lib:array-slice tcos-%data% single-float ((+ kr 1)) ((1 1))
                               tcos-%offset%))
        (go label126)
       label124
        (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                      ((> i mr) nil)
          (tagbody
            (setf (f2cl-lib:fref b-%data% (i) ((1 1)) b-%offset%)
                    (* fistag (f2cl-lib:fref b-%data% (i) ((1 1)) b-%offset%)))
           label125))
       label126
        (cosgen kr 1 0.5 fden tcos)
        (trix kr lr mr a bb c b tcos d w)
        (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                      ((> i mr) nil)
          (tagbody
            (setf (f2cl-lib:fref q-%data% (i j) ((1 idimq) (1 1)) q-%offset%)
                    (+
                     (f2cl-lib:fref q-%data% (i j) ((1 idimq) (1 1))
                                    q-%offset%)
                     (f2cl-lib:fref b-%data% (i) ((1 1)) b-%offset%)))
           label127))
        (setf kr (f2cl-lib:int-add kr i2r))
        (go label151)
       label128
        (setf jp1 (f2cl-lib:int-add j i2rby2))
        (setf jp2 (f2cl-lib:int-add j i2r))
        (if (/= i2r 1)
            (go label135))
        (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                      ((> i mr) nil)
          (tagbody
            (setf (f2cl-lib:fref b-%data% (i) ((1 1)) b-%offset%)
                    (f2cl-lib:fref q-%data% (i j) ((1 idimq) (1 1))
                                   q-%offset%))
           label129))
        (trix 1 0 mr a bb c b tcos d w)
        (setf ip 0)
        (setf ipstor mr)
        (f2cl-lib:computed-goto (label133 label130) istag)
       label130
        (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                      ((> i mr) nil)
          (tagbody
            (setf (f2cl-lib:fref p-%data% (i) ((1 1)) p-%offset%)
                    (f2cl-lib:fref b-%data% (i) ((1 1)) b-%offset%))
            (setf (f2cl-lib:fref b-%data% (i) ((1 1)) b-%offset%)
                    (+ (f2cl-lib:fref b-%data% (i) ((1 1)) b-%offset%)
                       (f2cl-lib:fref q-%data% (i n) ((1 idimq) (1 1))
                                      q-%offset%)))
           label131))
        (setf (f2cl-lib:fref tcos-%data% (1) ((1 1)) tcos-%offset%) 1.0)
        (setf (f2cl-lib:fref tcos-%data% (2) ((1 1)) tcos-%offset%) 0.0)
        (trix 1 1 mr a bb c b tcos d w)
        (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                      ((> i mr) nil)
          (tagbody
            (setf (f2cl-lib:fref q-%data% (i j) ((1 idimq) (1 1)) q-%offset%)
                    (+
                     (f2cl-lib:fref q-%data% (i jm2) ((1 idimq) (1 1))
                                    q-%offset%)
                     (f2cl-lib:fref p-%data% (i) ((1 1)) p-%offset%)
                     (f2cl-lib:fref b-%data% (i) ((1 1)) b-%offset%)))
           label132))
        (go label150)
       label133
        (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                      ((> i mr) nil)
          (tagbody
            (setf (f2cl-lib:fref p-%data% (i) ((1 1)) p-%offset%)
                    (f2cl-lib:fref b-%data% (i) ((1 1)) b-%offset%))
            (setf (f2cl-lib:fref q-%data% (i j) ((1 idimq) (1 1)) q-%offset%)
                    (+
                     (f2cl-lib:fref q-%data% (i jm2) ((1 idimq) (1 1))
                                    q-%offset%)
                     (* 2.0
                        (f2cl-lib:fref q-%data% (i jp2) ((1 idimq) (1 1))
                                       q-%offset%))
                     (* 3.0 (f2cl-lib:fref b-%data% (i) ((1 1)) b-%offset%))))
           label134))
        (go label150)
       label135
        (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                      ((> i mr) nil)
          (tagbody
            (setf (f2cl-lib:fref b-%data% (i) ((1 1)) b-%offset%)
                    (+
                     (f2cl-lib:fref q-%data% (i j) ((1 idimq) (1 1))
                                    q-%offset%)
                     (* 0.5
                        (-
                         (f2cl-lib:fref q-%data% (i jm2) ((1 idimq) (1 1))
                                        q-%offset%)
                         (f2cl-lib:fref q-%data% (i jm1) ((1 idimq) (1 1))
                                        q-%offset%)
                         (f2cl-lib:fref q-%data% (i jm3) ((1 idimq) (1 1))
                                        q-%offset%)))))
           label136))
        (if (/= nrodpr 0)
            (go label138))
        (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                      ((> i mr) nil)
          (tagbody
            (setf ii (f2cl-lib:int-add ip i))
            (setf (f2cl-lib:fref b-%data% (i) ((1 1)) b-%offset%)
                    (+ (f2cl-lib:fref b-%data% (i) ((1 1)) b-%offset%)
                       (f2cl-lib:fref p-%data% (ii) ((1 1)) p-%offset%)))
           label137))
        (go label140)
       label138
        (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                      ((> i mr) nil)
          (tagbody
            (setf (f2cl-lib:fref b-%data% (i) ((1 1)) b-%offset%)
                    (-
                     (+ (f2cl-lib:fref b-%data% (i) ((1 1)) b-%offset%)
                        (f2cl-lib:fref q-%data% (i jp2) ((1 idimq) (1 1))
                                       q-%offset%))
                     (f2cl-lib:fref q-%data% (i jp1) ((1 idimq) (1 1))
                                    q-%offset%)))
           label139))
       label140
        (trix i2r 0 mr a bb c b tcos d w)
        (setf ip (f2cl-lib:int-add ip mr))
        (setf ipstor (f2cl-lib:max0 ipstor (f2cl-lib:int-add ip mr)))
        (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                      ((> i mr) nil)
          (tagbody
            (setf ii (f2cl-lib:int-add ip i))
            (setf (f2cl-lib:fref p-%data% (ii) ((1 1)) p-%offset%)
                    (+ (f2cl-lib:fref b-%data% (i) ((1 1)) b-%offset%)
                       (* 0.5
                          (-
                           (f2cl-lib:fref q-%data% (i j) ((1 idimq) (1 1))
                                          q-%offset%)
                           (f2cl-lib:fref q-%data% (i jm1) ((1 idimq) (1 1))
                                          q-%offset%)
                           (f2cl-lib:fref q-%data% (i jp1) ((1 idimq) (1 1))
                                          q-%offset%)))))
            (setf (f2cl-lib:fref b-%data% (i) ((1 1)) b-%offset%)
                    (+ (f2cl-lib:fref p-%data% (ii) ((1 1)) p-%offset%)
                       (f2cl-lib:fref q-%data% (i jp2) ((1 idimq) (1 1))
                                      q-%offset%)))
           label141))
        (if (= lr 0)
            (go label142))
        (cosgen lr 1 0.5 fden
         (f2cl-lib:array-slice tcos-%data% single-float ((+ i2r 1)) ((1 1))
                               tcos-%offset%))
        (merge$ tcos 0 i2r i2r lr kr)
        (go label144)
       label142
        (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                      ((> i i2r) nil)
          (tagbody
            (setf ii (f2cl-lib:int-add kr i))
            (setf (f2cl-lib:fref tcos-%data% (ii) ((1 1)) tcos-%offset%)
                    (f2cl-lib:fref tcos-%data% (i) ((1 1)) tcos-%offset%))
           label143))
       label144
        (cosgen kr 1 0.5 fden tcos)
        (if (/= lr 0)
            (go label145))
        (f2cl-lib:computed-goto (label146 label145) istag)
       label145
        (trix kr kr mr a bb c b tcos d w)
        (go label148)
       label146
        (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                      ((> i mr) nil)
          (tagbody
            (setf (f2cl-lib:fref b-%data% (i) ((1 1)) b-%offset%)
                    (* fistag (f2cl-lib:fref b-%data% (i) ((1 1)) b-%offset%)))
           label147))
       label148
        (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                      ((> i mr) nil)
          (tagbody
            (setf ii (f2cl-lib:int-add ip i))
            (setf (f2cl-lib:fref q-%data% (i j) ((1 idimq) (1 1)) q-%offset%)
                    (+
                     (f2cl-lib:fref q-%data% (i jm2) ((1 idimq) (1 1))
                                    q-%offset%)
                     (f2cl-lib:fref p-%data% (ii) ((1 1)) p-%offset%)
                     (f2cl-lib:fref b-%data% (i) ((1 1)) b-%offset%)))
           label149))
       label150
        (setf lr kr)
        (setf kr (f2cl-lib:int-add kr jr))
       label151
        (f2cl-lib:computed-goto (label152 label153) mixbnd)
       label152
        (setf nr (+ (the f2cl-lib:integer4 (truncate (- nlast 1) jr)) 1))
        (if (<= nr 3)
            (go label155))
        (go label154)
       label153
        (setf nr (the f2cl-lib:integer4 (truncate nlast jr)))
        (if (<= nr 1)
            (go label192))
       label154
        (setf i2r jr)
        (setf nrodpr nrod)
        (go label104)
       label155
        (setf j (f2cl-lib:int-add 1 jr))
        (setf jm1 (f2cl-lib:int-sub j i2r))
        (setf jp1 (f2cl-lib:int-add j i2r))
        (setf jm2 (f2cl-lib:int-sub nlast i2r))
        (if (= nr 2)
            (go label184))
        (if (/= lr 0)
            (go label170))
        (if (/= n 3)
            (go label161))
        (f2cl-lib:computed-goto (label156 label168) istag)
       label156
        (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                      ((> i mr) nil)
          (tagbody
            (setf (f2cl-lib:fref b-%data% (i) ((1 1)) b-%offset%)
                    (f2cl-lib:fref q-%data% (i 2) ((1 idimq) (1 1))
                                   q-%offset%))
           label157))
        (setf (f2cl-lib:fref tcos-%data% (1) ((1 1)) tcos-%offset%) 0.0)
        (trix 1 0 mr a bb c b tcos d w)
        (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                      ((> i mr) nil)
          (tagbody
            (setf (f2cl-lib:fref q-%data% (i 2) ((1 idimq) (1 1)) q-%offset%)
                    (f2cl-lib:fref b-%data% (i) ((1 1)) b-%offset%))
            (setf (f2cl-lib:fref b-%data% (i) ((1 1)) b-%offset%)
                    (+ (* 4.0 (f2cl-lib:fref b-%data% (i) ((1 1)) b-%offset%))
                       (f2cl-lib:fref q-%data% (i 1) ((1 idimq) (1 1))
                                      q-%offset%)
                       (* 2.0
                          (f2cl-lib:fref q-%data% (i 3) ((1 idimq) (1 1))
                                         q-%offset%))))
           label158))
        (setf (f2cl-lib:fref tcos-%data% (1) ((1 1)) tcos-%offset%) -2.0)
        (setf (f2cl-lib:fref tcos-%data% (2) ((1 1)) tcos-%offset%) 2.0)
        (setf i1 2)
        (setf i2 0)
        (trix i1 i2 mr a bb c b tcos d w)
        (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                      ((> i mr) nil)
          (tagbody
            (setf (f2cl-lib:fref q-%data% (i 2) ((1 idimq) (1 1)) q-%offset%)
                    (+
                     (f2cl-lib:fref q-%data% (i 2) ((1 idimq) (1 1))
                                    q-%offset%)
                     (f2cl-lib:fref b-%data% (i) ((1 1)) b-%offset%)))
            (setf (f2cl-lib:fref b-%data% (i) ((1 1)) b-%offset%)
                    (+
                     (f2cl-lib:fref q-%data% (i 1) ((1 idimq) (1 1))
                                    q-%offset%)
                     (* 2.0
                        (f2cl-lib:fref q-%data% (i 2) ((1 idimq) (1 1))
                                       q-%offset%))))
           label159))
        (setf (f2cl-lib:fref tcos-%data% (1) ((1 1)) tcos-%offset%) 0.0)
        (trix 1 0 mr a bb c b tcos d w)
        (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                      ((> i mr) nil)
          (tagbody
            (setf (f2cl-lib:fref q-%data% (i 1) ((1 idimq) (1 1)) q-%offset%)
                    (f2cl-lib:fref b-%data% (i) ((1 1)) b-%offset%))
           label160))
        (setf jr 1)
        (setf i2r 0)
        (go label194)
       label161
        (f2cl-lib:computed-goto (label162 label170) istag)
       label162
        (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                      ((> i mr) nil)
          (tagbody
            (setf (f2cl-lib:fref b-%data% (i) ((1 1)) b-%offset%)
                    (-
                     (+
                      (-
                       (+
                        (f2cl-lib:fref q-%data% (i j) ((1 idimq) (1 1))
                                       q-%offset%)
                        (* 0.5
                           (f2cl-lib:fref q-%data% (i 1) ((1 idimq) (1 1))
                                          q-%offset%)))
                       (f2cl-lib:fref q-%data% (i jm1) ((1 idimq) (1 1))
                                      q-%offset%))
                      (f2cl-lib:fref q-%data% (i nlast) ((1 idimq) (1 1))
                                     q-%offset%))
                     (f2cl-lib:fref q-%data% (i jm2) ((1 idimq) (1 1))
                                    q-%offset%)))
           label163))
        (cosgen jr 1 0.5 0.0 tcos)
        (trix jr 0 mr a bb c b tcos d w)
        (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                      ((> i mr) nil)
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
            (setf (f2cl-lib:fref b-%data% (i) ((1 1)) b-%offset%)
                    (+
                     (f2cl-lib:fref q-%data% (i 1) ((1 idimq) (1 1))
                                    q-%offset%)
                     (* 2.0
                        (f2cl-lib:fref q-%data% (i nlast) ((1 idimq) (1 1))
                                       q-%offset%))
                     (* 4.0
                        (f2cl-lib:fref q-%data% (i j) ((1 idimq) (1 1))
                                       q-%offset%))))
           label164))
        (setf jr2 (f2cl-lib:int-mul 2 jr))
        (cosgen jr 1 0.0 0.0 tcos)
        (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                      ((> i jr) nil)
          (tagbody
            (setf i1 (f2cl-lib:int-add jr i))
            (setf i2 (f2cl-lib:int-sub (f2cl-lib:int-add jr 1) i))
            (setf (f2cl-lib:fref tcos-%data% (i1) ((1 1)) tcos-%offset%)
                    (- (f2cl-lib:fref tcos-%data% (i2) ((1 1)) tcos-%offset%)))
           label165))
        (trix jr2 0 mr a bb c b tcos d w)
        (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                      ((> i mr) nil)
          (tagbody
            (setf (f2cl-lib:fref q-%data% (i j) ((1 idimq) (1 1)) q-%offset%)
                    (+
                     (f2cl-lib:fref q-%data% (i j) ((1 idimq) (1 1))
                                    q-%offset%)
                     (f2cl-lib:fref b-%data% (i) ((1 1)) b-%offset%)))
            (setf (f2cl-lib:fref b-%data% (i) ((1 1)) b-%offset%)
                    (+
                     (f2cl-lib:fref q-%data% (i 1) ((1 idimq) (1 1))
                                    q-%offset%)
                     (* 2.0
                        (f2cl-lib:fref q-%data% (i j) ((1 idimq) (1 1))
                                       q-%offset%))))
           label166))
        (cosgen jr 1 0.5 0.0 tcos)
        (trix jr 0 mr a bb c b tcos d w)
        (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                      ((> i mr) nil)
          (tagbody
            (setf (f2cl-lib:fref q-%data% (i 1) ((1 idimq) (1 1)) q-%offset%)
                    (+
                     (-
                      (* 0.5
                         (f2cl-lib:fref q-%data% (i 1) ((1 idimq) (1 1))
                                        q-%offset%))
                      (f2cl-lib:fref q-%data% (i jm1) ((1 idimq) (1 1))
                                     q-%offset%))
                     (f2cl-lib:fref b-%data% (i) ((1 1)) b-%offset%)))
           label167))
        (go label194)
       label168
        (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                      ((> i mr) nil)
          (tagbody
            (setf (f2cl-lib:fref b-%data% (i) ((1 1)) b-%offset%)
                    (f2cl-lib:fref q-%data% (i 2) ((1 idimq) (1 1))
                                   q-%offset%))
            (setf (f2cl-lib:fref q-%data% (i 2) ((1 idimq) (1 1)) q-%offset%)
                    0.0)
            (setf (f2cl-lib:fref b2-%data% (i) ((1 1)) b2-%offset%)
                    (f2cl-lib:fref q-%data% (i 3) ((1 idimq) (1 1))
                                   q-%offset%))
            (setf (f2cl-lib:fref b3-%data% (i) ((1 1)) b3-%offset%)
                    (f2cl-lib:fref q-%data% (i 1) ((1 idimq) (1 1))
                                   q-%offset%))
           label169))
        (setf jr 1)
        (setf i2r 0)
        (setf j 2)
        (go label177)
       label170
        (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                      ((> i mr) nil)
          (tagbody
            (setf (f2cl-lib:fref b-%data% (i) ((1 1)) b-%offset%)
                    (+
                     (-
                      (* 0.5
                         (f2cl-lib:fref q-%data% (i 1) ((1 idimq) (1 1))
                                        q-%offset%))
                      (f2cl-lib:fref q-%data% (i jm1) ((1 idimq) (1 1))
                                     q-%offset%))
                     (f2cl-lib:fref q-%data% (i j) ((1 idimq) (1 1))
                                    q-%offset%)))
           label171))
        (if (/= nrod 0)
            (go label173))
        (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                      ((> i mr) nil)
          (tagbody
            (setf ii (f2cl-lib:int-add ip i))
            (setf (f2cl-lib:fref b-%data% (i) ((1 1)) b-%offset%)
                    (+ (f2cl-lib:fref b-%data% (i) ((1 1)) b-%offset%)
                       (f2cl-lib:fref p-%data% (ii) ((1 1)) p-%offset%)))
           label172))
        (go label175)
       label173
        (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                      ((> i mr) nil)
          (tagbody
            (setf (f2cl-lib:fref b-%data% (i) ((1 1)) b-%offset%)
                    (-
                     (+ (f2cl-lib:fref b-%data% (i) ((1 1)) b-%offset%)
                        (f2cl-lib:fref q-%data% (i nlast) ((1 idimq) (1 1))
                                       q-%offset%))
                     (f2cl-lib:fref q-%data% (i jm2) ((1 idimq) (1 1))
                                    q-%offset%)))
           label174))
       label175
        (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                      ((> i mr) nil)
          (tagbody
            (setf t$
                    (* 0.5
                       (-
                        (f2cl-lib:fref q-%data% (i j) ((1 idimq) (1 1))
                                       q-%offset%)
                        (f2cl-lib:fref q-%data% (i jm1) ((1 idimq) (1 1))
                                       q-%offset%)
                        (f2cl-lib:fref q-%data% (i jp1) ((1 idimq) (1 1))
                                       q-%offset%))))
            (setf (f2cl-lib:fref q-%data% (i j) ((1 idimq) (1 1)) q-%offset%)
                    t$)
            (setf (f2cl-lib:fref b2-%data% (i) ((1 1)) b2-%offset%)
                    (+
                     (f2cl-lib:fref q-%data% (i nlast) ((1 idimq) (1 1))
                                    q-%offset%)
                     t$))
            (setf (f2cl-lib:fref b3-%data% (i) ((1 1)) b3-%offset%)
                    (+
                     (f2cl-lib:fref q-%data% (i 1) ((1 idimq) (1 1))
                                    q-%offset%)
                     (* 2.0 t$)))
           label176))
       label177
        (setf k1
                (f2cl-lib:int-sub (f2cl-lib:int-add kr (f2cl-lib:int-mul 2 jr))
                                  1))
        (setf k2 (f2cl-lib:int-add kr jr))
        (setf (f2cl-lib:fref tcos-%data% ((f2cl-lib:int-add k1 1)) ((1 1))
                             tcos-%offset%)
                -2.0)
        (setf k4 (f2cl-lib:int-sub (f2cl-lib:int-add k1 3) istag))
        (cosgen (f2cl-lib:int-sub (f2cl-lib:int-add k2 istag) 2) 1 0.0 fnum
         (f2cl-lib:array-slice tcos-%data% single-float (k4) ((1 1))
                               tcos-%offset%))
        (setf k4 (f2cl-lib:int-add k1 k2 1))
        (cosgen (f2cl-lib:int-sub jr 1) 1 0.0 1.0
         (f2cl-lib:array-slice tcos-%data% single-float (k4) ((1 1))
                               tcos-%offset%))
        (merge$ tcos k1 k2 (f2cl-lib:int-add k1 k2) (f2cl-lib:int-sub jr 1) 0)
        (setf k3 (f2cl-lib:int-add k1 k2 lr))
        (cosgen jr 1 0.5 0.0
         (f2cl-lib:array-slice tcos-%data% single-float ((+ k3 1)) ((1 1))
                               tcos-%offset%))
        (setf k4 (f2cl-lib:int-add k3 jr 1))
        (cosgen kr 1 0.5 fden
         (f2cl-lib:array-slice tcos-%data% single-float (k4) ((1 1))
                               tcos-%offset%))
        (merge$ tcos k3 jr (f2cl-lib:int-add k3 jr) kr k1)
        (if (= lr 0)
            (go label178))
        (cosgen lr 1 0.5 fden
         (f2cl-lib:array-slice tcos-%data% single-float (k4) ((1 1))
                               tcos-%offset%))
        (merge$ tcos k3 jr (f2cl-lib:int-add k3 jr) lr
         (f2cl-lib:int-sub k3 lr))
        (cosgen kr 1 0.5 fden
         (f2cl-lib:array-slice tcos-%data% single-float (k4) ((1 1))
                               tcos-%offset%))
       label178
        (setf k3 kr)
        (setf k4 kr)
        (tri3 mr a bb c k b b2 b3 tcos d w w2 w3)
        (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                      ((> i mr) nil)
          (tagbody
            (setf (f2cl-lib:fref b-%data% (i) ((1 1)) b-%offset%)
                    (+ (f2cl-lib:fref b-%data% (i) ((1 1)) b-%offset%)
                       (f2cl-lib:fref b2-%data% (i) ((1 1)) b2-%offset%)
                       (f2cl-lib:fref b3-%data% (i) ((1 1)) b3-%offset%)))
           label179))
        (setf (f2cl-lib:fref tcos-%data% (1) ((1 1)) tcos-%offset%) 2.0)
        (trix 1 0 mr a bb c b tcos d w)
        (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                      ((> i mr) nil)
          (tagbody
            (setf (f2cl-lib:fref q-%data% (i j) ((1 idimq) (1 1)) q-%offset%)
                    (+
                     (f2cl-lib:fref q-%data% (i j) ((1 idimq) (1 1))
                                    q-%offset%)
                     (f2cl-lib:fref b-%data% (i) ((1 1)) b-%offset%)))
            (setf (f2cl-lib:fref b-%data% (i) ((1 1)) b-%offset%)
                    (+
                     (f2cl-lib:fref q-%data% (i 1) ((1 idimq) (1 1))
                                    q-%offset%)
                     (* 2.0
                        (f2cl-lib:fref q-%data% (i j) ((1 idimq) (1 1))
                                       q-%offset%))))
           label180))
        (cosgen jr 1 0.5 0.0 tcos)
        (trix jr 0 mr a bb c b tcos d w)
        (if (/= jr 1)
            (go label182))
        (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                      ((> i mr) nil)
          (tagbody
            (setf (f2cl-lib:fref q-%data% (i 1) ((1 idimq) (1 1)) q-%offset%)
                    (f2cl-lib:fref b-%data% (i) ((1 1)) b-%offset%))
           label181))
        (go label194)
       label182
        (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                      ((> i mr) nil)
          (tagbody
            (setf (f2cl-lib:fref q-%data% (i 1) ((1 idimq) (1 1)) q-%offset%)
                    (+
                     (-
                      (* 0.5
                         (f2cl-lib:fref q-%data% (i 1) ((1 idimq) (1 1))
                                        q-%offset%))
                      (f2cl-lib:fref q-%data% (i jm1) ((1 idimq) (1 1))
                                     q-%offset%))
                     (f2cl-lib:fref b-%data% (i) ((1 1)) b-%offset%)))
           label183))
        (go label194)
       label184
        (if (/= n 2)
            (go label188))
        (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                      ((> i mr) nil)
          (tagbody
            (setf (f2cl-lib:fref b-%data% (i) ((1 1)) b-%offset%)
                    (f2cl-lib:fref q-%data% (i 1) ((1 idimq) (1 1))
                                   q-%offset%))
           label185))
        (setf (f2cl-lib:fref tcos-%data% (1) ((1 1)) tcos-%offset%) 0.0)
        (trix 1 0 mr a bb c b tcos d w)
        (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                      ((> i mr) nil)
          (tagbody
            (setf (f2cl-lib:fref q-%data% (i 1) ((1 idimq) (1 1)) q-%offset%)
                    (f2cl-lib:fref b-%data% (i) ((1 1)) b-%offset%))
            (setf (f2cl-lib:fref b-%data% (i) ((1 1)) b-%offset%)
                    (* 2.0
                       (+
                        (f2cl-lib:fref q-%data% (i 2) ((1 idimq) (1 1))
                                       q-%offset%)
                        (f2cl-lib:fref b-%data% (i) ((1 1)) b-%offset%))
                       fistag))
           label186))
        (setf (f2cl-lib:fref tcos-%data% (1) ((1 1)) tcos-%offset%) (- fistag))
        (setf (f2cl-lib:fref tcos-%data% (2) ((1 1)) tcos-%offset%) 2.0)
        (trix 2 0 mr a bb c b tcos d w)
        (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                      ((> i mr) nil)
          (tagbody
            (setf (f2cl-lib:fref q-%data% (i 1) ((1 idimq) (1 1)) q-%offset%)
                    (+
                     (f2cl-lib:fref q-%data% (i 1) ((1 idimq) (1 1))
                                    q-%offset%)
                     (f2cl-lib:fref b-%data% (i) ((1 1)) b-%offset%)))
           label187))
        (setf jr 1)
        (setf i2r 0)
        (go label194)
       label188
        (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                      ((> i mr) nil)
          (tagbody
            (setf ii (f2cl-lib:int-add ip i))
            (setf (f2cl-lib:fref b3-%data% (i) ((1 1)) b3-%offset%) 0.0)
            (setf (f2cl-lib:fref b-%data% (i) ((1 1)) b-%offset%)
                    (+
                     (f2cl-lib:fref q-%data% (i 1) ((1 idimq) (1 1))
                                    q-%offset%)
                     (* 2.0 (f2cl-lib:fref p-%data% (ii) ((1 1)) p-%offset%))))
            (setf (f2cl-lib:fref q-%data% (i 1) ((1 idimq) (1 1)) q-%offset%)
                    (-
                     (* 0.5
                        (f2cl-lib:fref q-%data% (i 1) ((1 idimq) (1 1))
                                       q-%offset%))
                     (f2cl-lib:fref q-%data% (i jm1) ((1 idimq) (1 1))
                                    q-%offset%)))
            (setf (f2cl-lib:fref b2-%data% (i) ((1 1)) b2-%offset%)
                    (* 2.0
                       (+
                        (f2cl-lib:fref q-%data% (i 1) ((1 idimq) (1 1))
                                       q-%offset%)
                        (f2cl-lib:fref q-%data% (i nlast) ((1 idimq) (1 1))
                                       q-%offset%))))
           label189))
        (setf k1 (f2cl-lib:int-sub (f2cl-lib:int-add kr jr) 1))
        (setf (f2cl-lib:fref tcos-%data% ((f2cl-lib:int-add k1 1)) ((1 1))
                             tcos-%offset%)
                -2.0)
        (setf k4 (f2cl-lib:int-sub (f2cl-lib:int-add k1 3) istag))
        (cosgen (f2cl-lib:int-sub (f2cl-lib:int-add kr istag) 2) 1 0.0 fnum
         (f2cl-lib:array-slice tcos-%data% single-float (k4) ((1 1))
                               tcos-%offset%))
        (setf k4 (f2cl-lib:int-add k1 kr 1))
        (cosgen (f2cl-lib:int-sub jr 1) 1 0.0 1.0
         (f2cl-lib:array-slice tcos-%data% single-float (k4) ((1 1))
                               tcos-%offset%))
        (merge$ tcos k1 kr (f2cl-lib:int-add k1 kr) (f2cl-lib:int-sub jr 1) 0)
        (cosgen kr 1 0.5 fden
         (f2cl-lib:array-slice tcos-%data% single-float ((+ k1 1)) ((1 1))
                               tcos-%offset%))
        (setf k2 kr)
        (setf k4 (f2cl-lib:int-add k1 k2 1))
        (cosgen lr 1 0.5 fden
         (f2cl-lib:array-slice tcos-%data% single-float (k4) ((1 1))
                               tcos-%offset%))
        (setf k3 lr)
        (setf k4 0)
        (tri3 mr a bb c k b b2 b3 tcos d w w2 w3)
        (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                      ((> i mr) nil)
          (tagbody
            (setf (f2cl-lib:fref b-%data% (i) ((1 1)) b-%offset%)
                    (+ (f2cl-lib:fref b-%data% (i) ((1 1)) b-%offset%)
                       (f2cl-lib:fref b2-%data% (i) ((1 1)) b2-%offset%)))
           label190))
        (setf (f2cl-lib:fref tcos-%data% (1) ((1 1)) tcos-%offset%) 2.0)
        (trix 1 0 mr a bb c b tcos d w)
        (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                      ((> i mr) nil)
          (tagbody
            (setf (f2cl-lib:fref q-%data% (i 1) ((1 idimq) (1 1)) q-%offset%)
                    (+
                     (f2cl-lib:fref q-%data% (i 1) ((1 idimq) (1 1))
                                    q-%offset%)
                     (f2cl-lib:fref b-%data% (i) ((1 1)) b-%offset%)))
           label191))
        (go label194)
       label192
        (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                      ((> i mr) nil)
          (tagbody
            (setf (f2cl-lib:fref b-%data% (i) ((1 1)) b-%offset%)
                    (f2cl-lib:fref q-%data% (i nlast) ((1 idimq) (1 1))
                                   q-%offset%))
           label193))
        (go label196)
       label194
        (setf j (f2cl-lib:int-sub nlast jr))
        (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                      ((> i mr) nil)
          (tagbody
            (setf (f2cl-lib:fref b-%data% (i) ((1 1)) b-%offset%)
                    (+
                     (f2cl-lib:fref q-%data% (i nlast) ((1 idimq) (1 1))
                                    q-%offset%)
                     (f2cl-lib:fref q-%data% (i j) ((1 idimq) (1 1))
                                    q-%offset%)))
           label195))
       label196
        (setf jm2 (f2cl-lib:int-sub nlast i2r))
        (if (/= jr 1)
            (go label198))
        (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                      ((> i mr) nil)
          (tagbody
            (setf (f2cl-lib:fref q-%data% (i nlast) ((1 idimq) (1 1))
                                 q-%offset%)
                    0.0)
           label197))
        (go label202)
       label198
        (if (/= nrod 0)
            (go label200))
        (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                      ((> i mr) nil)
          (tagbody
            (setf ii (f2cl-lib:int-add ip i))
            (setf (f2cl-lib:fref q-%data% (i nlast) ((1 idimq) (1 1))
                                 q-%offset%)
                    (f2cl-lib:fref p-%data% (ii) ((1 1)) p-%offset%))
           label199))
        (setf ip (f2cl-lib:int-sub ip mr))
        (go label202)
       label200
        (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                      ((> i mr) nil)
          (tagbody
            (setf (f2cl-lib:fref q-%data% (i nlast) ((1 idimq) (1 1))
                                 q-%offset%)
                    (-
                     (f2cl-lib:fref q-%data% (i nlast) ((1 idimq) (1 1))
                                    q-%offset%)
                     (f2cl-lib:fref q-%data% (i jm2) ((1 idimq) (1 1))
                                    q-%offset%)))
           label201))
       label202
        (cosgen kr 1 0.5 fden tcos)
        (cosgen lr 1 0.5 fden
         (f2cl-lib:array-slice tcos-%data% single-float ((+ kr 1)) ((1 1))
                               tcos-%offset%))
        (if (/= lr 0)
            (go label204))
        (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                      ((> i mr) nil)
          (tagbody
            (setf (f2cl-lib:fref b-%data% (i) ((1 1)) b-%offset%)
                    (* fistag (f2cl-lib:fref b-%data% (i) ((1 1)) b-%offset%)))
           label203))
       label204
        (trix kr lr mr a bb c b tcos d w)
        (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                      ((> i mr) nil)
          (tagbody
            (setf (f2cl-lib:fref q-%data% (i nlast) ((1 idimq) (1 1))
                                 q-%offset%)
                    (+
                     (f2cl-lib:fref q-%data% (i nlast) ((1 idimq) (1 1))
                                    q-%offset%)
                     (f2cl-lib:fref b-%data% (i) ((1 1)) b-%offset%)))
           label205))
        (setf nlastp nlast)
       label206
        (setf jstep jr)
        (setf jr i2r)
        (setf i2r (the f2cl-lib:integer4 (truncate i2r 2)))
        (if (= jr 0)
            (go label222))
        (f2cl-lib:computed-goto (label207 label208) mixbnd)
       label207
        (setf jstart (f2cl-lib:int-add 1 jr))
        (go label209)
       label208
        (setf jstart jr)
       label209
        (setf kr (f2cl-lib:int-sub kr jr))
        (if (> (f2cl-lib:int-add nlast jr) n)
            (go label210))
        (setf kr (f2cl-lib:int-sub kr jr))
        (setf nlast (f2cl-lib:int-add nlast jr))
        (setf jstop (f2cl-lib:int-sub nlast jstep))
        (go label211)
       label210
        (setf jstop (f2cl-lib:int-sub nlast jr))
       label211
        (setf lr (f2cl-lib:int-sub kr jr))
        (cosgen jr 1 0.5 0.0 tcos)
        (f2cl-lib:fdo (j jstart (f2cl-lib:int-add j jstep))
                      ((> j jstop) nil)
          (tagbody
            (setf jm2 (f2cl-lib:int-sub j jr))
            (setf jp2 (f2cl-lib:int-add j jr))
            (if (/= j jr)
                (go label213))
            (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                          ((> i mr) nil)
              (tagbody
                (setf (f2cl-lib:fref b-%data% (i) ((1 1)) b-%offset%)
                        (+
                         (f2cl-lib:fref q-%data% (i j) ((1 idimq) (1 1))
                                        q-%offset%)
                         (f2cl-lib:fref q-%data% (i jp2) ((1 idimq) (1 1))
                                        q-%offset%)))
               label212))
            (go label215)
           label213
            (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                          ((> i mr) nil)
              (tagbody
                (setf (f2cl-lib:fref b-%data% (i) ((1 1)) b-%offset%)
                        (+
                         (f2cl-lib:fref q-%data% (i j) ((1 idimq) (1 1))
                                        q-%offset%)
                         (f2cl-lib:fref q-%data% (i jm2) ((1 idimq) (1 1))
                                        q-%offset%)
                         (f2cl-lib:fref q-%data% (i jp2) ((1 idimq) (1 1))
                                        q-%offset%)))
               label214))
           label215
            (if (/= jr 1)
                (go label217))
            (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                          ((> i mr) nil)
              (tagbody
                (setf (f2cl-lib:fref q-%data% (i j) ((1 idimq) (1 1))
                                     q-%offset%)
                        0.0)
               label216))
            (go label219)
           label217
            (setf jm1 (f2cl-lib:int-sub j i2r))
            (setf jp1 (f2cl-lib:int-add j i2r))
            (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                          ((> i mr) nil)
              (tagbody
                (setf (f2cl-lib:fref q-%data% (i j) ((1 idimq) (1 1))
                                     q-%offset%)
                        (* 0.5
                           (-
                            (f2cl-lib:fref q-%data% (i j) ((1 idimq) (1 1))
                                           q-%offset%)
                            (f2cl-lib:fref q-%data% (i jm1) ((1 idimq) (1 1))
                                           q-%offset%)
                            (f2cl-lib:fref q-%data% (i jp1) ((1 idimq) (1 1))
                                           q-%offset%))))
               label218))
           label219
            (trix jr 0 mr a bb c b tcos d w)
            (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                          ((> i mr) nil)
              (tagbody
                (setf (f2cl-lib:fref q-%data% (i j) ((1 idimq) (1 1))
                                     q-%offset%)
                        (+
                         (f2cl-lib:fref q-%data% (i j) ((1 idimq) (1 1))
                                        q-%offset%)
                         (f2cl-lib:fref b-%data% (i) ((1 1)) b-%offset%)))
               label220))
           label221))
        (setf nrod 1)
        (if (<= (f2cl-lib:int-add nlast i2r) n)
            (setf nrod 0))
        (if (/= nlastp nlast)
            (go label194))
        (go label206)
       label222
        (setf (f2cl-lib:fref w-%data% (1) ((1 1)) w-%offset%)
                (coerce (the f2cl-lib:integer4 ipstor) 'single-float))
        (go end_label)
       end_label
        (return
         (values nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
                 nil nil nil))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::poisn2
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo :arg-types
                                            '((fortran-to-lisp::integer4)
                                              (fortran-to-lisp::integer4)
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
                                              (array single-float (*))
                                              (array single-float (*))
                                              (array single-float (*))
                                              (array single-float (*))
                                              (array single-float (*)))
                                            :return-values
                                            '(nil nil nil nil nil nil nil nil
                                              nil nil nil nil nil nil nil nil
                                              nil nil)
                                            :calls
                                            '(fortran-to-lisp::tri3
                                              fortran-to-lisp::merge$
                                              fortran-to-lisp::trix
                                              fortran-to-lisp::cosgen))))

