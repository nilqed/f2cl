;;; Compiled by f2cl version:
;;; ("" "" "" "" "" "" "")

;;; Using Lisp SBCL 1.3.18
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing t) (:declare-common nil)
;;;           (:float-format single-float))

(in-package "QUADPACK")


(let ((x1
       (make-array 5 :element-type 'double-float :initial-contents
                   '(0.9739065285171717d0 0.8650633666889845d0
                     0.6794095682990244d0 0.4333953941292472d0
                     0.14887433898163122d0)))
      (w10
       (make-array 5 :element-type 'double-float :initial-contents
                   '(0.06667134430868814d0 0.1494513491505806d0
                     0.21908636251598204d0 0.26926671930999635d0
                     0.29552422471475287d0)))
      (x2
       (make-array 5 :element-type 'double-float :initial-contents
                   '(0.9956571630258081d0 0.9301574913557082d0
                     0.7808177265864169d0 0.5627571346686047d0
                     0.2943928627014602d0)))
      (w21a
       (make-array 5 :element-type 'double-float :initial-contents
                   '(0.032558162307964725d0 0.07503967481091996d0
                     0.10938715880229764d0 0.13470921731147334d0
                     0.14773910490133849d0)))
      (w21b
       (make-array 6 :element-type 'double-float :initial-contents
                   '(0.011694638867371874d0 0.054755896574351995d0
                     0.0931254545836976d0 0.12349197626206584d0
                     0.14277593857706009d0 0.1494455540029169d0)))
      (x3
       (make-array 11 :element-type 'double-float :initial-contents
                   '(0.999333360901932d0 0.9874334029080889d0
                     0.9548079348142663d0 0.9001486957483283d0
                     0.8251983149831141d0 0.732148388989305d0
                     0.6228479705377252d0 0.4994795740710565d0
                     0.36490166134658075d0 0.2222549197766013d0
                     0.07465061746138332d0)))
      (w43a
       (make-array 10 :element-type 'double-float :initial-contents
                   '(0.016296734289666565d0 0.0375228761208695d0
                     0.05469490205825544d0 0.06735541460947808d0
                     0.07387019963239395d0 0.005768556059769796d0
                     0.027371890593248842d0 0.04656082691042883d0
                     0.06174499520144257d0 0.07138726726869339d0)))
      (w43b
       (make-array 12 :element-type 'double-float :initial-contents
                   '(0.001844477640212414d0 0.010798689585891651d0
                     0.021895363867795427d0 0.032597463975345686d0
                     0.04216313793519181d0 0.050741939600184575d0
                     0.05837939554261925d0 0.06474640495144589d0
                     0.06956619791235648d0 0.07282444147183322d0
                     0.07450775101417512d0 0.07472214751740301d0)))
      (x4
       (make-array 22 :element-type 'double-float :initial-contents
                   '(0.9999029772627293d0 0.9979898959866788d0
                     0.9921754978606873d0 0.9813581635727128d0
                     0.9650576238583847d0 0.9431676131336706d0
                     0.9158064146855072d0 0.8832216577713164d0
                     0.8457107484624157d0 0.8035576580352309d0
                     0.7570057306854956d0 0.7062732097873218d0
                     0.6515894665011779d0 0.5932233740579611d0
                     0.531493605970832d0 0.46676362304202285d0
                     0.3994248478592188d0 0.3298748771061883d0
                     0.25850355920216156d0 0.18569539656834666d0
                     0.11184221317990747d0 0.03735212339461987d0)))
      (w87a
       (make-array 21 :element-type 'double-float :initial-contents
                   '(0.008148377384149173d0 0.018761438201562824d0
                     0.027347451050052287d0 0.03367770731163793d0
                     0.036935099820427905d0 0.0028848724302115306d0
                     0.013685946022712702d0 0.02328041350288831d0
                     0.03087249761171336d0 0.03569363363941877d0
                     9.152833452022414d-4 0.005399280219300471d0
                     0.01094767960111893d0 0.016298731696787336d0
                     0.021081568889203834d0 0.025370969769253827d0
                     0.029189697756475754d0 0.03237320246720279d0
                     0.034783098950365146d0 0.03641222073135179d0
                     0.037253875503047706d0)))
      (w87b
       (make-array 23 :element-type 'double-float :initial-contents
                   '(2.7414556376207234d-4 0.0018071241550579428d0
                     0.0040968692827591646d0 0.006758290051847379d0
                     0.009549957672201646d0 0.012329447652244854d0
                     0.015010447346388952d0 0.01754896798624319d0
                     0.019938037786440887d0 0.022194935961012286d0
                     0.024339147126000805d0 0.026374505414839208d0
                     0.0282869107887712d0 0.030052581128092695d0
                     0.03164675137143993d0 0.033050413419978504d0
                     0.034255099704226064d0 0.03526241266015668d0
                     0.0360769896228887d0 0.03669860449845609d0
                     0.037120549269832576d0 0.03733422875193504d0
                     0.037361073762679026d0))))
  (declare (type (array double-float (5)) x1 w10 x2 w21a)
           (type (array double-float (6)) w21b)
           (type (array double-float (11)) x3)
           (type (array double-float (10)) w43a)
           (type (array double-float (12)) w43b)
           (type (array double-float (22)) x4)
           (type (array double-float (21)) w87a)
           (type (array double-float (23)) w87b))
  (defun dqng (f a b epsabs epsrel result abserr neval ier)
    (declare (type (f2cl-lib:integer4) ier neval)
             (type (double-float) abserr result epsrel epsabs b a))
    (prog ((fv1 (make-array 5 :element-type 'double-float))
           (fv2 (make-array 5 :element-type 'double-float))
           (fv3 (make-array 5 :element-type 'double-float))
           (fv4 (make-array 5 :element-type 'double-float))
           (savfun (make-array 21 :element-type 'double-float)) (ipx 0) (k 0)
           (l 0) (absc 0.0d0) (centr 0.0d0) (dhlgth 0.0d0) (epmach 0.0d0)
           (fcentr 0.0d0) (fval 0.0d0) (fval1 0.0d0) (fval2 0.0d0)
           (hlgth 0.0d0) (res10 0.0d0) (res21 0.0d0) (res43 0.0d0)
           (res87 0.0d0) (resabs 0.0d0) (resasc 0.0d0) (reskh 0.0d0)
           (uflow 0.0d0))
      (declare (type (array double-float (21)) savfun)
               (type (array double-float (5)) fv4 fv3 fv2 fv1)
               (type (double-float) uflow reskh resasc resabs res87 res43 res21
                res10 hlgth fval2 fval1 fval fcentr epmach dhlgth centr absc)
               (type (f2cl-lib:integer4) l k ipx))
      (setf epmach (f2cl-lib:d1mach 4))
      (setf uflow (f2cl-lib:d1mach 1))
      (setf result 0.0d0)
      (setf abserr 0.0d0)
      (setf neval 0)
      (setf ier 6)
      (if (and (<= epsabs 0.0d0)
               (< epsrel (f2cl-lib:dmax1 (* 50.0d0 epmach) 5.0d-29)))
          (go label80))
      (setf hlgth (* 0.5d0 (- b a)))
      (setf dhlgth (f2cl-lib:dabs hlgth))
      (setf centr (* 0.5d0 (+ b a)))
      (setf fcentr
              (multiple-value-bind (ret-val var-0)
                  (funcall f centr)
                (declare (ignore))
                (when var-0 (setf centr var-0))
                ret-val))
      (setf neval 21)
      (setf ier 1)
      (f2cl-lib:fdo (l 1 (f2cl-lib:int-add l 1))
                    ((> l 3) nil)
        (tagbody
          (f2cl-lib:computed-goto (label5 label25 label45) l)
         label5
          (setf res10 0.0d0)
          (setf res21 (* (f2cl-lib:fref w21b (6) ((1 6))) fcentr))
          (setf resabs
                  (* (f2cl-lib:fref w21b (6) ((1 6))) (f2cl-lib:dabs fcentr)))
          (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                        ((> k 5) nil)
            (tagbody
              (setf absc (* hlgth (f2cl-lib:fref x1 (k) ((1 5)))))
              (setf fval1 (funcall f (+ centr absc)))
              (setf fval2 (funcall f (- centr absc)))
              (setf fval (+ fval1 fval2))
              (setf res10 (+ res10 (* (f2cl-lib:fref w10 (k) ((1 5))) fval)))
              (setf res21 (+ res21 (* (f2cl-lib:fref w21a (k) ((1 5))) fval)))
              (setf resabs
                      (+ resabs
                         (* (f2cl-lib:fref w21a (k) ((1 5)))
                            (+ (f2cl-lib:dabs fval1) (f2cl-lib:dabs fval2)))))
              (setf (f2cl-lib:fref savfun (k) ((1 21))) fval)
              (setf (f2cl-lib:fref fv1 (k) ((1 5))) fval1)
              (setf (f2cl-lib:fref fv2 (k) ((1 5))) fval2)
             label10))
          (setf ipx 5)
          (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                        ((> k 5) nil)
            (tagbody
              (setf ipx (f2cl-lib:int-add ipx 1))
              (setf absc (* hlgth (f2cl-lib:fref x2 (k) ((1 5)))))
              (setf fval1 (funcall f (+ centr absc)))
              (setf fval2 (funcall f (- centr absc)))
              (setf fval (+ fval1 fval2))
              (setf res21 (+ res21 (* (f2cl-lib:fref w21b (k) ((1 6))) fval)))
              (setf resabs
                      (+ resabs
                         (* (f2cl-lib:fref w21b (k) ((1 6)))
                            (+ (f2cl-lib:dabs fval1) (f2cl-lib:dabs fval2)))))
              (setf (f2cl-lib:fref savfun (ipx) ((1 21))) fval)
              (setf (f2cl-lib:fref fv3 (k) ((1 5))) fval1)
              (setf (f2cl-lib:fref fv4 (k) ((1 5))) fval2)
             label15))
          (setf result (* res21 hlgth))
          (setf resabs (* resabs dhlgth))
          (setf reskh (* 0.5d0 res21))
          (setf resasc
                  (* (f2cl-lib:fref w21b (6) ((1 6)))
                     (f2cl-lib:dabs (- fcentr reskh))))
          (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                        ((> k 5) nil)
            (tagbody
              (setf resasc
                      (+ resasc
                         (* (f2cl-lib:fref w21a (k) ((1 5)))
                            (+
                             (f2cl-lib:dabs
                              (- (f2cl-lib:fref fv1 (k) ((1 5))) reskh))
                             (f2cl-lib:dabs
                              (- (f2cl-lib:fref fv2 (k) ((1 5))) reskh))))
                         (* (f2cl-lib:fref w21b (k) ((1 6)))
                            (+
                             (f2cl-lib:dabs
                              (- (f2cl-lib:fref fv3 (k) ((1 5))) reskh))
                             (f2cl-lib:dabs
                              (- (f2cl-lib:fref fv4 (k) ((1 5))) reskh))))))
             label20))
          (setf abserr (f2cl-lib:dabs (* (- res21 res10) hlgth)))
          (setf resasc (* resasc dhlgth))
          (go label65)
         label25
          (setf res43 (* (f2cl-lib:fref w43b (12) ((1 12))) fcentr))
          (setf neval 43)
          (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                        ((> k 10) nil)
            (tagbody
              (setf res43
                      (+ res43
                         (* (f2cl-lib:fref savfun (k) ((1 21)))
                            (f2cl-lib:fref w43a (k) ((1 10))))))
             label30))
          (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                        ((> k 11) nil)
            (tagbody
              (setf ipx (f2cl-lib:int-add ipx 1))
              (setf absc (* hlgth (f2cl-lib:fref x3 (k) ((1 11)))))
              (setf fval
                      (+ (funcall f (+ absc centr))
                         (funcall f (- centr absc))))
              (setf res43 (+ res43 (* fval (f2cl-lib:fref w43b (k) ((1 12))))))
              (setf (f2cl-lib:fref savfun (ipx) ((1 21))) fval)
             label40))
          (setf result (* res43 hlgth))
          (setf abserr (f2cl-lib:dabs (* (- res43 res21) hlgth)))
          (go label65)
         label45
          (setf res87 (* (f2cl-lib:fref w87b (23) ((1 23))) fcentr))
          (setf neval 87)
          (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                        ((> k 21) nil)
            (tagbody
              (setf res87
                      (+ res87
                         (* (f2cl-lib:fref savfun (k) ((1 21)))
                            (f2cl-lib:fref w87a (k) ((1 21))))))
             label50))
          (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                        ((> k 22) nil)
            (tagbody
              (setf absc (* hlgth (f2cl-lib:fref x4 (k) ((1 22)))))
              (setf res87
                      (+ res87
                         (* (f2cl-lib:fref w87b (k) ((1 23)))
                            (+ (funcall f (+ absc centr))
                               (funcall f (- centr absc))))))
             label60))
          (setf result (* res87 hlgth))
          (setf abserr (f2cl-lib:dabs (* (- res87 res43) hlgth)))
         label65
          (if (and (/= resasc 0.0d0) (/= abserr 0.0d0))
              (setf abserr
                      (* resasc
                         (f2cl-lib:dmin1 1.0d0
                                         (expt (/ (* 200.0d0 abserr) resasc)
                                               1.5d0)))))
          (if (> resabs (/ uflow (* 50.0d0 epmach)))
              (setf abserr (f2cl-lib:dmax1 (* epmach 50.0d0 resabs) abserr)))
          (if (<= abserr
                  (f2cl-lib:dmax1 epsabs (* epsrel (f2cl-lib:dabs result))))
              (setf ier 0))
          (if (= ier 0)
              (go label999))
         label70))
     label80
      (multiple-value-bind (var-0 var-1 var-2 var-3)
          (xerror "abnormal return from dqng " 26 ier 0)
        (declare (ignore var-0 var-1 var-3))
        (setf ier var-2))
     label999
      (go end_label)
     end_label
      (return (values nil nil nil nil nil result abserr neval ier)))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dqng fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo :arg-types
                                            '(t (double-float) (double-float)
                                              (double-float) (double-float)
                                              (double-float) (double-float)
                                              (fortran-to-lisp::integer4)
                                              (fortran-to-lisp::integer4))
                                            :return-values
                                            '(nil nil nil nil nil
                                              fortran-to-lisp::result
                                              fortran-to-lisp::abserr
                                              fortran-to-lisp::neval
                                              fortran-to-lisp::ier)
                                            :calls
                                            '(fortran-to-lisp::xerror
                                              fortran-to-lisp::d1mach))))

