;;; Compiled by f2cl version:
;;; ("" "" "" "" "" "" "")

;;; Using Lisp SBCL 1.4.2
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing t) (:declare-common nil)
;;;           (:float-format single-float))

(in-package "FISHPACK")


(defun hwsss1
       (ts tf m mbdcnd bdts bdtf ps pf n nbdcnd bdps bdpf elmbda f idimf pertrb
        am bm cm sn ss sint d)
  (declare (type (array single-float (*)) f)
           (type (array single-float (*)) d sint ss sn cm bm am bdpf bdps bdtf
            bdts)
           (type (f2cl-lib:integer4) idimf nbdcnd n mbdcnd m)
           (type (single-float) pertrb elmbda pf ps tf ts))
  (f2cl-lib:with-multi-array-data
      ((bdts single-float bdts-%data% bdts-%offset%)
       (bdtf single-float bdtf-%data% bdtf-%offset%)
       (bdps single-float bdps-%data% bdps-%offset%)
       (bdpf single-float bdpf-%data% bdpf-%offset%)
       (am single-float am-%data% am-%offset%)
       (bm single-float bm-%data% bm-%offset%)
       (cm single-float cm-%data% cm-%offset%)
       (sn single-float sn-%data% sn-%offset%)
       (ss single-float ss-%data% ss-%offset%)
       (sint single-float sint-%data% sint-%offset%)
       (d single-float d-%data% d-%offset%)
       (f single-float f-%data% f-%offset%))
    (prog ((den 0.0) (rts 0.0) (rtn 0.0) (csp 0.0) (dns 0.0) (dss 0.0)
           (dfs 0.0) (hld 0.0) (cnp 0.0) (dsn 0.0) (dnn 0.0) (dfn 0.0)
           (ierror 0) (sum2 0.0) (j 0) (yhld 0.0) (hne 0.0) (sum1 0.0)
           (sum 0.0) (ising 0) (cf 0.0) (fjj 0.0) (nunk 0) (jpfm 0) (jpsp 0)
           (jpf 0) (jps 0) (wpf 0.0) (wps 0.0) (nbr 0) (ii 0) (iid 0) (munk 0)
           (wtf 0.0) (wts 0.0) (itfm 0) (itsp 0) (ct 0.0) (itf 0) (at 0.0)
           (its 0) (mbr 0) (isp 0) (inp 0) (t1 0.0) (theta 0.0) (fim1 0.0)
           (i 0) (wp 0.0) (cp 0.0) (dth2 0.0) (edp2 0.0) (dphi2 0.0) (tdp 0.0)
           (dphi 0.0) (tdt 0.0) (hdth 0.0) (dth 0.0) (fm 0.0) (fn 0.0) (np1 0)
           (mp1 0) (hpi 0.0) (tpi 0.0) (dum 0.0) (pi$ 0.0))
      (declare
       (type (f2cl-lib:integer4) mp1 np1 i inp isp mbr its itf itsp itfm munk
        iid ii nbr jps jpf jpsp jpfm nunk ising j ierror)
       (type (single-float) pi$ dum tpi hpi fn fm dth hdth tdt dphi tdp dphi2
        edp2 dth2 cp wp fim1 theta t1 at ct wts wtf wps wpf fjj cf sum sum1 hne
        yhld sum2 dfn dnn dsn cnp hld dfs dss dns csp rtn rts den))
      (setf pi$ (pimach dum))
      (setf tpi (+ pi$ pi$))
      (setf hpi (/ pi$ 2.0))
      (setf mp1 (f2cl-lib:int-add m 1))
      (setf np1 (f2cl-lib:int-add n 1))
      (setf fn (coerce (the f2cl-lib:integer4 n) 'single-float))
      (setf fm (coerce (the f2cl-lib:integer4 m) 'single-float))
      (setf dth (/ (- tf ts) fm))
      (setf hdth (/ dth 2.0))
      (setf tdt (+ dth dth))
      (setf dphi (/ (- pf ps) fn))
      (setf tdp (+ dphi dphi))
      (setf dphi2 (* dphi dphi))
      (setf edp2 (* elmbda dphi2))
      (setf dth2 (* dth dth))
      (setf cp (/ 4.0 (* fn dth2)))
      (setf wp (/ (* fn (sin hdth)) 4.0))
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i mp1) nil)
        (tagbody
          (setf fim1
                  (coerce (the f2cl-lib:integer4 (f2cl-lib:int-sub i 1))
                          'single-float))
          (setf theta (+ (* fim1 dth) ts))
          (setf (f2cl-lib:fref sint-%data% (i) ((1 1)) sint-%offset%)
                  (sin theta))
          (f2cl-lib:arithmetic-if
           (f2cl-lib:fref sint-%data% (i) ((1 1)) sint-%offset%) (go label101)
           (go label102) (go label101))
         label101
          (setf t1
                  (/ 1.0
                     (* dth2
                        (f2cl-lib:fref sint-%data% (i) ((1 1))
                                       sint-%offset%))))
          (setf (f2cl-lib:fref am-%data% (i) ((1 1)) am-%offset%)
                  (* t1 (sin (- theta hdth))))
          (setf (f2cl-lib:fref cm-%data% (i) ((1 1)) cm-%offset%)
                  (* t1 (sin (+ theta hdth))))
          (setf (f2cl-lib:fref bm-%data% (i) ((1 1)) bm-%offset%)
                  (+
                   (- (- (f2cl-lib:fref cm-%data% (i) ((1 1)) cm-%offset%))
                      (f2cl-lib:fref am-%data% (i) ((1 1)) am-%offset%))
                   elmbda))
         label102))
      (setf inp 0)
      (setf isp 0)
      (setf mbr (f2cl-lib:int-add mbdcnd 1))
      (f2cl-lib:computed-goto
       (label103 label104 label104 label105 label105 label106 label106 label104
        label105 label106)
       mbr)
     label103
      (setf its 1)
      (go label107)
     label104
      (setf at (f2cl-lib:fref am-%data% (2) ((1 1)) am-%offset%))
      (setf its 2)
      (go label107)
     label105
      (setf at (f2cl-lib:fref am-%data% (1) ((1 1)) am-%offset%))
      (setf its 1)
      (setf (f2cl-lib:fref cm-%data% (1) ((1 1)) cm-%offset%)
              (+ (f2cl-lib:fref am-%data% (1) ((1 1)) am-%offset%)
                 (f2cl-lib:fref cm-%data% (1) ((1 1)) cm-%offset%)))
      (go label107)
     label106
      (setf at (f2cl-lib:fref am-%data% (2) ((1 1)) am-%offset%))
      (setf inp 1)
      (setf its 2)
     label107
      (f2cl-lib:computed-goto
       (label108 label109 label110 label110 label109 label109 label110 label111
        label111 label111)
       mbr)
     label108
      (setf itf m)
      (go label112)
     label109
      (setf ct (f2cl-lib:fref cm-%data% (m) ((1 1)) cm-%offset%))
      (setf itf m)
      (go label112)
     label110
      (setf ct
              (f2cl-lib:fref cm-%data% ((f2cl-lib:int-add m 1)) ((1 1))
                             cm-%offset%))
      (setf (f2cl-lib:fref am-%data% ((f2cl-lib:int-add m 1)) ((1 1))
                           am-%offset%)
              (+
               (f2cl-lib:fref am-%data% ((f2cl-lib:int-add m 1)) ((1 1))
                              am-%offset%)
               (f2cl-lib:fref cm-%data% ((f2cl-lib:int-add m 1)) ((1 1))
                              cm-%offset%)))
      (setf itf (f2cl-lib:int-add m 1))
      (go label112)
     label111
      (setf itf m)
      (setf isp 1)
      (setf ct (f2cl-lib:fref cm-%data% (m) ((1 1)) cm-%offset%))
     label112
      (setf itsp (f2cl-lib:int-add its 1))
      (setf itfm (f2cl-lib:int-sub itf 1))
      (setf wts
              (/
               (*
                (f2cl-lib:fref sint-%data% ((f2cl-lib:int-add its 1)) ((1 1))
                               sint-%offset%)
                (f2cl-lib:fref am-%data% ((f2cl-lib:int-add its 1)) ((1 1))
                               am-%offset%))
               (f2cl-lib:fref cm-%data% (its) ((1 1)) cm-%offset%)))
      (setf wtf
              (/
               (*
                (f2cl-lib:fref sint-%data% ((f2cl-lib:int-sub itf 1)) ((1 1))
                               sint-%offset%)
                (f2cl-lib:fref cm-%data% ((f2cl-lib:int-sub itf 1)) ((1 1))
                               cm-%offset%))
               (f2cl-lib:fref am-%data% (itf) ((1 1)) am-%offset%)))
      (setf munk (f2cl-lib:int-add (f2cl-lib:int-sub itf its) 1))
      (f2cl-lib:arithmetic-if isp (go label116) (go label116) (go label113))
     label113
      (setf (f2cl-lib:fref d-%data% (its) ((1 1)) d-%offset%)
              (/ (f2cl-lib:fref cm-%data% (its) ((1 1)) cm-%offset%)
                 (f2cl-lib:fref bm-%data% (its) ((1 1)) bm-%offset%)))
      (f2cl-lib:fdo (i itsp (f2cl-lib:int-add i 1))
                    ((> i m) nil)
        (tagbody
          (setf (f2cl-lib:fref d-%data% (i) ((1 1)) d-%offset%)
                  (/ (f2cl-lib:fref cm-%data% (i) ((1 1)) cm-%offset%)
                     (- (f2cl-lib:fref bm-%data% (i) ((1 1)) bm-%offset%)
                        (* (f2cl-lib:fref am-%data% (i) ((1 1)) am-%offset%)
                           (f2cl-lib:fref d-%data% ((f2cl-lib:int-sub i 1))
                                          ((1 1)) d-%offset%)))))
         label114))
      (setf (f2cl-lib:fref ss-%data% (m) ((1 1)) ss-%offset%)
              (- (f2cl-lib:fref d-%data% (m) ((1 1)) d-%offset%)))
      (setf iid (f2cl-lib:int-sub m its))
      (f2cl-lib:fdo (ii 1 (f2cl-lib:int-add ii 1))
                    ((> ii iid) nil)
        (tagbody
          (setf i (f2cl-lib:int-sub m ii))
          (setf (f2cl-lib:fref ss-%data% (i) ((1 1)) ss-%offset%)
                  (* (- (f2cl-lib:fref d-%data% (i) ((1 1)) d-%offset%))
                     (f2cl-lib:fref ss-%data% ((f2cl-lib:int-add i 1)) ((1 1))
                                    ss-%offset%)))
         label115))
      (setf (f2cl-lib:fref ss-%data% ((f2cl-lib:int-add m 1)) ((1 1))
                           ss-%offset%)
              1.0)
     label116
      (f2cl-lib:arithmetic-if inp (go label120) (go label120) (go label117))
     label117
      (setf (f2cl-lib:fref sn-%data% (1) ((1 1)) sn-%offset%) 1.0)
      (setf (f2cl-lib:fref d-%data% (itf) ((1 1)) d-%offset%)
              (/ (f2cl-lib:fref am-%data% (itf) ((1 1)) am-%offset%)
                 (f2cl-lib:fref bm-%data% (itf) ((1 1)) bm-%offset%)))
      (setf iid (f2cl-lib:int-sub itf 2))
      (f2cl-lib:fdo (ii 1 (f2cl-lib:int-add ii 1))
                    ((> ii iid) nil)
        (tagbody
          (setf i (f2cl-lib:int-sub itf ii))
          (setf (f2cl-lib:fref d-%data% (i) ((1 1)) d-%offset%)
                  (/ (f2cl-lib:fref am-%data% (i) ((1 1)) am-%offset%)
                     (- (f2cl-lib:fref bm-%data% (i) ((1 1)) bm-%offset%)
                        (* (f2cl-lib:fref cm-%data% (i) ((1 1)) cm-%offset%)
                           (f2cl-lib:fref d-%data% ((f2cl-lib:int-add i 1))
                                          ((1 1)) d-%offset%)))))
         label118))
      (setf (f2cl-lib:fref sn-%data% (2) ((1 1)) sn-%offset%)
              (- (f2cl-lib:fref d-%data% (2) ((1 1)) d-%offset%)))
      (f2cl-lib:fdo (i 3 (f2cl-lib:int-add i 1))
                    ((> i itf) nil)
        (tagbody
          (setf (f2cl-lib:fref sn-%data% (i) ((1 1)) sn-%offset%)
                  (* (- (f2cl-lib:fref d-%data% (i) ((1 1)) d-%offset%))
                     (f2cl-lib:fref sn-%data% ((f2cl-lib:int-sub i 1)) ((1 1))
                                    sn-%offset%)))
         label119))
     label120
      (setf nbr (f2cl-lib:int-add nbdcnd 1))
      (setf wps 1.0)
      (setf wpf 1.0)
      (f2cl-lib:computed-goto (label121 label122 label122 label123 label123)
                              nbr)
     label121
      (setf jps 1)
      (go label124)
     label122
      (setf jps 2)
      (go label124)
     label123
      (setf jps 1)
      (setf wps 0.5)
     label124
      (f2cl-lib:computed-goto (label125 label126 label127 label127 label126)
                              nbr)
     label125
      (setf jpf n)
      (go label128)
     label126
      (setf jpf n)
      (go label128)
     label127
      (setf wpf 0.5)
      (setf jpf (f2cl-lib:int-add n 1))
     label128
      (setf jpsp (f2cl-lib:int-add jps 1))
      (setf jpfm (f2cl-lib:int-sub jpf 1))
      (setf nunk (f2cl-lib:int-add (f2cl-lib:int-sub jpf jps) 1))
      (setf fjj
              (coerce
               (the f2cl-lib:integer4
                    (f2cl-lib:int-add (f2cl-lib:int-sub jpfm jpsp) 1))
               'single-float))
      (f2cl-lib:fdo (i its (f2cl-lib:int-add i 1))
                    ((> i itf) nil)
        (tagbody
          (setf cf
                  (* dphi2
                     (f2cl-lib:fref sint-%data% (i) ((1 1)) sint-%offset%)
                     (f2cl-lib:fref sint-%data% (i) ((1 1)) sint-%offset%)))
          (setf (f2cl-lib:fref am-%data% (i) ((1 1)) am-%offset%)
                  (* cf (f2cl-lib:fref am-%data% (i) ((1 1)) am-%offset%)))
          (setf (f2cl-lib:fref bm-%data% (i) ((1 1)) bm-%offset%)
                  (* cf (f2cl-lib:fref bm-%data% (i) ((1 1)) bm-%offset%)))
          (setf (f2cl-lib:fref cm-%data% (i) ((1 1)) cm-%offset%)
                  (* cf (f2cl-lib:fref cm-%data% (i) ((1 1)) cm-%offset%)))
         label129))
      (setf (f2cl-lib:fref am-%data% (its) ((1 1)) am-%offset%) 0.0)
      (setf (f2cl-lib:fref cm-%data% (itf) ((1 1)) cm-%offset%) 0.0)
      (setf ising 0)
      (f2cl-lib:computed-goto
       (label130 label138 label138 label130 label138 label138 label130 label138
        label130 label130)
       mbr)
     label130
      (f2cl-lib:computed-goto (label131 label138 label138 label131 label138)
                              nbr)
     label131
      (f2cl-lib:arithmetic-if elmbda (go label138) (go label132) (go label132))
     label132
      (setf ising 1)
      (setf sum (+ (* wts wps) (* wts wpf) (* wtf wps) (* wtf wpf)))
      (f2cl-lib:arithmetic-if inp (go label134) (go label134) (go label133))
     label133
      (setf sum (+ sum wp))
     label134
      (f2cl-lib:arithmetic-if isp (go label136) (go label136) (go label135))
     label135
      (setf sum (+ sum wp))
     label136
      (setf sum1 0.0)
      (f2cl-lib:fdo (i itsp (f2cl-lib:int-add i 1))
                    ((> i itfm) nil)
        (tagbody
          (setf sum1
                  (+ sum1
                     (f2cl-lib:fref sint-%data% (i) ((1 1)) sint-%offset%)))
         label137))
      (setf sum (+ sum (* fjj (+ sum1 wts wtf))))
      (setf sum (+ sum (* (+ wps wpf) sum1)))
      (setf hne sum)
     label138
      (f2cl-lib:computed-goto
       (label146 label142 label142 label144 label144 label139 label139 label142
        label144 label139)
       mbr)
     label139
      (f2cl-lib:arithmetic-if (f2cl-lib:int-sub nbdcnd 3) (go label146)
                              (go label140) (go label146))
     label140
      (setf yhld
              (+ (f2cl-lib:fref f-%data% (1 jps) ((1 idimf) (1 1)) f-%offset%)
                 (* (/ -4.0 (* fn dphi dth2))
                    (- (f2cl-lib:fref bdpf-%data% (2) ((1 1)) bdpf-%offset%)
                       (f2cl-lib:fref bdps-%data% (2) ((1 1))
                                      bdps-%offset%)))))
      (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                    ((> j np1) nil)
        (tagbody
          (setf (f2cl-lib:fref f-%data% (1 j) ((1 idimf) (1 1)) f-%offset%)
                  yhld)
         label141))
      (go label146)
     label142
      (f2cl-lib:fdo (j jps (f2cl-lib:int-add j 1))
                    ((> j jpf) nil)
        (tagbody
          (setf (f2cl-lib:fref f-%data% (2 j) ((1 idimf) (1 1)) f-%offset%)
                  (-
                   (f2cl-lib:fref f-%data% (2 j) ((1 idimf) (1 1)) f-%offset%)
                   (* at
                      (f2cl-lib:fref f-%data% (1 j) ((1 idimf) (1 1))
                                     f-%offset%))))
         label143))
      (go label146)
     label144
      (f2cl-lib:fdo (j jps (f2cl-lib:int-add j 1))
                    ((> j jpf) nil)
        (tagbody
          (setf (f2cl-lib:fref f-%data% (1 j) ((1 idimf) (1 1)) f-%offset%)
                  (+
                   (f2cl-lib:fref f-%data% (1 j) ((1 idimf) (1 1)) f-%offset%)
                   (* tdt (f2cl-lib:fref bdts-%data% (j) ((1 1)) bdts-%offset%)
                      at)))
         label145))
     label146
      (f2cl-lib:computed-goto
       (label154 label150 label152 label152 label150 label150 label152 label147
        label147 label147)
       mbr)
     label147
      (f2cl-lib:arithmetic-if (f2cl-lib:int-sub nbdcnd 3) (go label154)
                              (go label148) (go label154))
     label148
      (setf yhld
              (+
               (f2cl-lib:fref f-%data% ((f2cl-lib:int-add m 1) jps)
                              ((1 idimf) (1 1)) f-%offset%)
               (* (/ -4.0 (* fn dphi dth2))
                  (- (f2cl-lib:fref bdpf-%data% (m) ((1 1)) bdpf-%offset%)
                     (f2cl-lib:fref bdps-%data% (m) ((1 1)) bdps-%offset%)))))
      (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                    ((> j np1) nil)
        (tagbody
          (setf (f2cl-lib:fref f-%data% ((f2cl-lib:int-add m 1) j)
                               ((1 idimf) (1 1)) f-%offset%)
                  yhld)
         label149))
      (go label154)
     label150
      (f2cl-lib:fdo (j jps (f2cl-lib:int-add j 1))
                    ((> j jpf) nil)
        (tagbody
          (setf (f2cl-lib:fref f-%data% (m j) ((1 idimf) (1 1)) f-%offset%)
                  (-
                   (f2cl-lib:fref f-%data% (m j) ((1 idimf) (1 1)) f-%offset%)
                   (* ct
                      (f2cl-lib:fref f-%data% ((f2cl-lib:int-add m 1) j)
                                     ((1 idimf) (1 1)) f-%offset%))))
         label151))
      (go label154)
     label152
      (f2cl-lib:fdo (j jps (f2cl-lib:int-add j 1))
                    ((> j jpf) nil)
        (tagbody
          (setf (f2cl-lib:fref f-%data% ((f2cl-lib:int-add m 1) j)
                               ((1 idimf) (1 1)) f-%offset%)
                  (+
                   (f2cl-lib:fref f-%data% ((f2cl-lib:int-add m 1) j)
                                  ((1 idimf) (1 1)) f-%offset%)
                   (* (- tdt)
                      (f2cl-lib:fref bdtf-%data% (j) ((1 1)) bdtf-%offset%)
                      ct)))
         label153))
     label154
      (f2cl-lib:computed-goto (label159 label155 label155 label157 label157)
                              nbr)
     label155
      (f2cl-lib:fdo (i its (f2cl-lib:int-add i 1))
                    ((> i itf) nil)
        (tagbody
          (setf (f2cl-lib:fref f-%data% (i 2) ((1 idimf) (1 1)) f-%offset%)
                  (+
                   (f2cl-lib:fref f-%data% (i 2) ((1 idimf) (1 1)) f-%offset%)
                   (/
                    (-
                     (f2cl-lib:fref f-%data% (i 1) ((1 idimf) (1 1))
                                    f-%offset%))
                    (* dphi2
                       (f2cl-lib:fref sint-%data% (i) ((1 1)) sint-%offset%)
                       (f2cl-lib:fref sint-%data% (i) ((1 1))
                                      sint-%offset%)))))
         label156))
      (go label159)
     label157
      (f2cl-lib:fdo (i its (f2cl-lib:int-add i 1))
                    ((> i itf) nil)
        (tagbody
          (setf (f2cl-lib:fref f-%data% (i 1) ((1 idimf) (1 1)) f-%offset%)
                  (+
                   (f2cl-lib:fref f-%data% (i 1) ((1 idimf) (1 1)) f-%offset%)
                   (/
                    (* tdp
                       (f2cl-lib:fref bdps-%data% (i) ((1 1)) bdps-%offset%))
                    (* dphi2
                       (f2cl-lib:fref sint-%data% (i) ((1 1)) sint-%offset%)
                       (f2cl-lib:fref sint-%data% (i) ((1 1))
                                      sint-%offset%)))))
         label158))
     label159
      (f2cl-lib:computed-goto (label164 label160 label162 label162 label160)
                              nbr)
     label160
      (f2cl-lib:fdo (i its (f2cl-lib:int-add i 1))
                    ((> i itf) nil)
        (tagbody
          (setf (f2cl-lib:fref f-%data% (i n) ((1 idimf) (1 1)) f-%offset%)
                  (+
                   (f2cl-lib:fref f-%data% (i n) ((1 idimf) (1 1)) f-%offset%)
                   (/
                    (-
                     (f2cl-lib:fref f-%data% (i (f2cl-lib:int-add n 1))
                                    ((1 idimf) (1 1)) f-%offset%))
                    (* dphi2
                       (f2cl-lib:fref sint-%data% (i) ((1 1)) sint-%offset%)
                       (f2cl-lib:fref sint-%data% (i) ((1 1))
                                      sint-%offset%)))))
         label161))
      (go label164)
     label162
      (f2cl-lib:fdo (i its (f2cl-lib:int-add i 1))
                    ((> i itf) nil)
        (tagbody
          (setf (f2cl-lib:fref f-%data% (i (f2cl-lib:int-add n 1))
                               ((1 idimf) (1 1)) f-%offset%)
                  (+
                   (f2cl-lib:fref f-%data% (i (f2cl-lib:int-add n 1))
                                  ((1 idimf) (1 1)) f-%offset%)
                   (/
                    (* (- tdp)
                       (f2cl-lib:fref bdpf-%data% (i) ((1 1)) bdpf-%offset%))
                    (* dphi2
                       (f2cl-lib:fref sint-%data% (i) ((1 1)) sint-%offset%)
                       (f2cl-lib:fref sint-%data% (i) ((1 1))
                                      sint-%offset%)))))
         label163))
     label164
      (setf pertrb 0.0)
      (f2cl-lib:arithmetic-if ising (go label165) (go label176) (go label165))
     label165
      (setf sum
              (+
               (* wts wps
                  (f2cl-lib:fref f-%data% (its jps) ((1 idimf) (1 1))
                                 f-%offset%))
               (* wts wpf
                  (f2cl-lib:fref f-%data% (its jpf) ((1 idimf) (1 1))
                                 f-%offset%))
               (* wtf wps
                  (f2cl-lib:fref f-%data% (itf jps) ((1 idimf) (1 1))
                                 f-%offset%))
               (* wtf wpf
                  (f2cl-lib:fref f-%data% (itf jpf) ((1 idimf) (1 1))
                                 f-%offset%))))
      (f2cl-lib:arithmetic-if inp (go label167) (go label167) (go label166))
     label166
      (setf sum
              (+ sum
                 (* wp
                    (f2cl-lib:fref f-%data% (1 jps) ((1 idimf) (1 1))
                                   f-%offset%))))
     label167
      (f2cl-lib:arithmetic-if isp (go label169) (go label169) (go label168))
     label168
      (setf sum
              (+ sum
                 (* wp
                    (f2cl-lib:fref f-%data% ((f2cl-lib:int-add m 1) jps)
                                   ((1 idimf) (1 1)) f-%offset%))))
     label169
      (f2cl-lib:fdo (i itsp (f2cl-lib:int-add i 1))
                    ((> i itfm) nil)
        (tagbody
          (setf sum1 0.0)
          (f2cl-lib:fdo (j jpsp (f2cl-lib:int-add j 1))
                        ((> j jpfm) nil)
            (tagbody
              (setf sum1
                      (+ sum1
                         (f2cl-lib:fref f-%data% (i j) ((1 idimf) (1 1))
                                        f-%offset%)))
             label170))
          (setf sum
                  (+ sum
                     (* (f2cl-lib:fref sint-%data% (i) ((1 1)) sint-%offset%)
                        sum1)))
         label171))
      (setf sum1 0.0)
      (setf sum2 0.0)
      (f2cl-lib:fdo (j jpsp (f2cl-lib:int-add j 1))
                    ((> j jpfm) nil)
        (tagbody
          (setf sum1
                  (+ sum1
                     (f2cl-lib:fref f-%data% (its j) ((1 idimf) (1 1))
                                    f-%offset%)))
          (setf sum2
                  (+ sum2
                     (f2cl-lib:fref f-%data% (itf j) ((1 idimf) (1 1))
                                    f-%offset%)))
         label172))
      (setf sum (+ sum (* wts sum1) (* wtf sum2)))
      (setf sum1 0.0)
      (setf sum2 0.0)
      (f2cl-lib:fdo (i itsp (f2cl-lib:int-add i 1))
                    ((> i itfm) nil)
        (tagbody
          (setf sum1
                  (+ sum1
                     (* (f2cl-lib:fref sint-%data% (i) ((1 1)) sint-%offset%)
                        (f2cl-lib:fref f-%data% (i jps) ((1 idimf) (1 1))
                                       f-%offset%))))
          (setf sum2
                  (+ sum2
                     (* (f2cl-lib:fref sint-%data% (i) ((1 1)) sint-%offset%)
                        (f2cl-lib:fref f-%data% (i jpf) ((1 idimf) (1 1))
                                       f-%offset%))))
         label173))
      (setf sum (+ sum (* wps sum1) (* wpf sum2)))
      (setf pertrb (/ sum hne))
      (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                    ((> j np1) nil)
        (tagbody
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i mp1) nil)
            (tagbody
              (setf (f2cl-lib:fref f-%data% (i j) ((1 idimf) (1 1)) f-%offset%)
                      (-
                       (f2cl-lib:fref f-%data% (i j) ((1 idimf) (1 1))
                                      f-%offset%)
                       pertrb))
             label174))
         label175))
     label176
      (f2cl-lib:fdo (i its (f2cl-lib:int-add i 1))
                    ((> i itf) nil)
        (tagbody
          (setf cf
                  (* dphi2
                     (f2cl-lib:fref sint-%data% (i) ((1 1)) sint-%offset%)
                     (f2cl-lib:fref sint-%data% (i) ((1 1)) sint-%offset%)))
          (f2cl-lib:fdo (j jps (f2cl-lib:int-add j 1))
                        ((> j jpf) nil)
            (tagbody
              (setf (f2cl-lib:fref f-%data% (i j) ((1 idimf) (1 1)) f-%offset%)
                      (* cf
                         (f2cl-lib:fref f-%data% (i j) ((1 idimf) (1 1))
                                        f-%offset%)))
             label177))
         label178))
      (multiple-value-bind
          (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9 var-10)
          (genbun nbdcnd nunk 1 munk
           (f2cl-lib:array-slice am-%data% single-float (its) ((1 1))
                                 am-%offset%)
           (f2cl-lib:array-slice bm-%data% single-float (its) ((1 1))
                                 bm-%offset%)
           (f2cl-lib:array-slice cm-%data% single-float (its) ((1 1))
                                 cm-%offset%)
           idimf
           (f2cl-lib:array-slice f-%data% single-float (its jps)
                                 ((1 idimf) (1 1)) f-%offset%)
           ierror d)
        (declare
         (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-8 var-10))
        (setf idimf var-7)
        (setf ierror var-9))
      (f2cl-lib:arithmetic-if ising (go label186) (go label186) (go label179))
     label179
      (f2cl-lib:arithmetic-if inp (go label183) (go label183) (go label180))
     label180
      (f2cl-lib:arithmetic-if isp (go label181) (go label181) (go label186))
     label181
      (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                    ((> j np1) nil)
        (tagbody
          (setf (f2cl-lib:fref f-%data% (1 j) ((1 idimf) (1 1)) f-%offset%)
                  0.0)
         label182))
      (go label209)
     label183
      (f2cl-lib:arithmetic-if isp (go label186) (go label186) (go label184))
     label184
      (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                    ((> j np1) nil)
        (tagbody
          (setf (f2cl-lib:fref f-%data% ((f2cl-lib:int-add m 1) j)
                               ((1 idimf) (1 1)) f-%offset%)
                  0.0)
         label185))
      (go label209)
     label186
      (f2cl-lib:arithmetic-if inp (go label193) (go label193) (go label187))
     label187
      (setf sum
              (+
               (* wps
                  (f2cl-lib:fref f-%data% (its jps) ((1 idimf) (1 1))
                                 f-%offset%))
               (* wpf
                  (f2cl-lib:fref f-%data% (its jpf) ((1 idimf) (1 1))
                                 f-%offset%))))
      (f2cl-lib:fdo (j jpsp (f2cl-lib:int-add j 1))
                    ((> j jpfm) nil)
        (tagbody
          (setf sum
                  (+ sum
                     (f2cl-lib:fref f-%data% (its j) ((1 idimf) (1 1))
                                    f-%offset%)))
         label188))
      (setf dfn (* cp sum))
      (setf dnn
              (+
               (* cp
                  (* (+ wps wpf fjj)
                     (- (f2cl-lib:fref sn-%data% (2) ((1 1)) sn-%offset%)
                        1.0)))
               elmbda))
      (setf dsn
              (* cp (+ wps wpf fjj)
                 (f2cl-lib:fref sn-%data% (m) ((1 1)) sn-%offset%)))
      (f2cl-lib:arithmetic-if isp (go label189) (go label189) (go label194))
     label189
      (setf cnp
              (/
               (- (f2cl-lib:fref f-%data% (1 1) ((1 idimf) (1 1)) f-%offset%)
                  dfn)
               dnn))
      (f2cl-lib:fdo (i its (f2cl-lib:int-add i 1))
                    ((> i itf) nil)
        (tagbody
          (setf hld (* cnp (f2cl-lib:fref sn-%data% (i) ((1 1)) sn-%offset%)))
          (f2cl-lib:fdo (j jps (f2cl-lib:int-add j 1))
                        ((> j jpf) nil)
            (tagbody
              (setf (f2cl-lib:fref f-%data% (i j) ((1 idimf) (1 1)) f-%offset%)
                      (+
                       (f2cl-lib:fref f-%data% (i j) ((1 idimf) (1 1))
                                      f-%offset%)
                       hld))
             label190))
         label191))
      (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                    ((> j np1) nil)
        (tagbody
          (setf (f2cl-lib:fref f-%data% (1 j) ((1 idimf) (1 1)) f-%offset%)
                  cnp)
         label192))
      (go label209)
     label193
      (f2cl-lib:arithmetic-if isp (go label209) (go label209) (go label194))
     label194
      (setf sum
              (+
               (* wps
                  (f2cl-lib:fref f-%data% (itf jps) ((1 idimf) (1 1))
                                 f-%offset%))
               (* wpf
                  (f2cl-lib:fref f-%data% (itf jpf) ((1 idimf) (1 1))
                                 f-%offset%))))
      (f2cl-lib:fdo (j jpsp (f2cl-lib:int-add j 1))
                    ((> j jpfm) nil)
        (tagbody
          (setf sum
                  (+ sum
                     (f2cl-lib:fref f-%data% (itf j) ((1 idimf) (1 1))
                                    f-%offset%)))
         label195))
      (setf dfs (* cp sum))
      (setf dss
              (+
               (* cp
                  (* (+ wps wpf fjj)
                     (- (f2cl-lib:fref ss-%data% (m) ((1 1)) ss-%offset%)
                        1.0)))
               elmbda))
      (setf dns
              (* cp (+ wps wpf fjj)
                 (f2cl-lib:fref ss-%data% (2) ((1 1)) ss-%offset%)))
      (f2cl-lib:arithmetic-if inp (go label196) (go label196) (go label200))
     label196
      (setf csp
              (/
               (-
                (f2cl-lib:fref f-%data% ((f2cl-lib:int-add m 1) 1)
                               ((1 idimf) (1 1)) f-%offset%)
                dfs)
               dss))
      (f2cl-lib:fdo (i its (f2cl-lib:int-add i 1))
                    ((> i itf) nil)
        (tagbody
          (setf hld (* csp (f2cl-lib:fref ss-%data% (i) ((1 1)) ss-%offset%)))
          (f2cl-lib:fdo (j jps (f2cl-lib:int-add j 1))
                        ((> j jpf) nil)
            (tagbody
              (setf (f2cl-lib:fref f-%data% (i j) ((1 idimf) (1 1)) f-%offset%)
                      (+
                       (f2cl-lib:fref f-%data% (i j) ((1 idimf) (1 1))
                                      f-%offset%)
                       hld))
             label197))
         label198))
      (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                    ((> j np1) nil)
        (tagbody
          (setf (f2cl-lib:fref f-%data% ((f2cl-lib:int-add m 1) j)
                               ((1 idimf) (1 1)) f-%offset%)
                  csp)
         label199))
      (go label209)
     label200
      (setf rtn
              (- (f2cl-lib:fref f-%data% (1 1) ((1 idimf) (1 1)) f-%offset%)
                 dfn))
      (setf rts
              (-
               (f2cl-lib:fref f-%data% ((f2cl-lib:int-add m 1) 1)
                              ((1 idimf) (1 1)) f-%offset%)
               dfs))
      (f2cl-lib:arithmetic-if ising (go label202) (go label202) (go label201))
     label201
      (setf csp 0.0)
      (setf cnp (/ rtn dnn))
      (go label205)
     label202
      (f2cl-lib:arithmetic-if (- (abs dnn) (abs dsn)) (go label204)
                              (go label204) (go label203))
     label203
      (setf den (+ dss (/ (* (- dns) dsn) dnn)))
      (setf rts (+ rts (/ (* (- rtn) dsn) dnn)))
      (setf csp (/ rts den))
      (setf cnp (/ (- rtn (* csp dns)) dnn))
      (go label205)
     label204
      (setf den (+ dns (/ (* (- dss) dnn) dsn)))
      (setf rtn (+ rtn (/ (* (- rts) dnn) dsn)))
      (setf csp (/ rtn den))
      (setf cnp (/ (- rts (* dss csp)) dsn))
     label205
      (f2cl-lib:fdo (i its (f2cl-lib:int-add i 1))
                    ((> i itf) nil)
        (tagbody
          (setf hld
                  (+ (* cnp (f2cl-lib:fref sn-%data% (i) ((1 1)) sn-%offset%))
                     (* csp
                        (f2cl-lib:fref ss-%data% (i) ((1 1)) ss-%offset%))))
          (f2cl-lib:fdo (j jps (f2cl-lib:int-add j 1))
                        ((> j jpf) nil)
            (tagbody
              (setf (f2cl-lib:fref f-%data% (i j) ((1 idimf) (1 1)) f-%offset%)
                      (+
                       (f2cl-lib:fref f-%data% (i j) ((1 idimf) (1 1))
                                      f-%offset%)
                       hld))
             label206))
         label207))
      (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                    ((> j np1) nil)
        (tagbody
          (setf (f2cl-lib:fref f-%data% (1 j) ((1 idimf) (1 1)) f-%offset%)
                  cnp)
          (setf (f2cl-lib:fref f-%data% ((f2cl-lib:int-add m 1) j)
                               ((1 idimf) (1 1)) f-%offset%)
                  csp)
         label208))
     label209
      (f2cl-lib:arithmetic-if nbdcnd (go label212) (go label210) (go label212))
     label210
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i mp1) nil)
        (tagbody
          (setf (f2cl-lib:fref f-%data% (i (f2cl-lib:int-add jpf 1))
                               ((1 idimf) (1 1)) f-%offset%)
                  (f2cl-lib:fref f-%data% (i jps) ((1 idimf) (1 1))
                                 f-%offset%))
         label211))
     label212
      (go end_label)
     end_label
      (return
       (values nil nil nil nil nil nil nil nil nil nil nil nil nil nil idimf
               pertrb nil nil nil nil nil nil nil)))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::hwsss1
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
                                              (array single-float (*))
                                              (array single-float (*))
                                              (array single-float (*))
                                              (array single-float (*))
                                              (array single-float (*))
                                              (array single-float (*))
                                              (array single-float (*)))
                                            :return-values
                                            '(nil nil nil nil nil nil nil nil
                                              nil nil nil nil nil nil
                                              fortran-to-lisp::idimf
                                              fortran-to-lisp::pertrb nil nil
                                              nil nil nil nil nil)
                                            :calls
                                            '(fortran-to-lisp::genbun
                                              fortran-to-lisp::pimach))))

