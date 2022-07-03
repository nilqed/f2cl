;;; Compiled by f2cl version:
;;; ("" "" "" "" "" "" "")

;;; Using Lisp SBCL 1.4.2
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing t) (:declare-common nil)
;;;           (:float-format single-float))

(in-package "QUADPACK")


(let ((wg
       (make-array 13 :element-type 'double-float :initial-contents
                   '(0.011393798501026288d0 0.026354986615032137d0
                     0.040939156701306316d0 0.054904695975835194d0
                     0.06803833381235691d0 0.08014070033500102d0
                     0.09102826198296365d0 0.10053594906705064d0
                     0.10851962447426365d0 0.11485825914571164d0
                     0.11945576353578477d0 0.12224244299031004d0
                     0.12317605372671545d0)))
      (xgk
       (make-array 26 :element-type 'double-float :initial-contents
                   '(0.9992621049926098d0 0.9955569697904981d0
                     0.9880357945340772d0 0.9766639214595175d0
                     0.9616149864258425d0 0.9429745712289743d0
                     0.9207471152817016d0 0.8949919978782753d0
                     0.8658470652932756d0 0.833442628760834d0
                     0.7978737979985001d0 0.7592592630373576d0
                     0.7177664068130843d0 0.6735663684734684d0
                     0.6268100990103174d0 0.577662930241223d0
                     0.5263252843347191d0 0.473002731445715d0
                     0.4178853821930377d0 0.36117230580938786d0
                     0.30308953893110785d0 0.24386688372098844d0
                     0.1837189394210489d0 0.1228646926107104d0
                     0.06154448300568508d0 0.0d0)))
      (wgk
       (make-array 26 :element-type 'double-float :initial-contents
                   '(0.001987383892330316d0 0.005561932135356714d0
                     0.009473973386174152d0 0.013236229195571676d0
                     0.0168478177091283d0 0.020435371145882834d0
                     0.024009945606953215d0 0.02747531758785174d0
                     0.030792300167387487d0 0.034002130274329335d0
                     0.03711627148341554d0 0.04008382550403238d0
                     0.04287284502017005d0 0.04550291304992179d0
                     0.04798253713883671d0 0.05027767908071567d0
                     0.05236288580640747d0 0.05425112988854549d0
                     0.055950811220412316d0 0.057437116361567835d0
                     0.058689680022394206d0 0.05972034032417406d0
                     0.06053945537604586d0 0.061128509717053046d0
                     0.061471189871425316d0 0.061580818067832936d0))))
  (declare (type (array double-float (13)) wg)
           (type (array double-float (26)) xgk wgk))
  (defun dqk51 (f a b result abserr resabs resasc)
    (declare (type (double-float) resasc resabs abserr result b a))
    (prog ((fv1 (make-array 25 :element-type 'double-float))
           (fv2 (make-array 25 :element-type 'double-float)) (j 0) (jtw 0)
           (jtwm1 0) (absc 0.0d0) (centr 0.0d0) (dhlgth 0.0d0) (epmach 0.0d0)
           (fc 0.0d0) (fsum 0.0d0) (fval1 0.0d0) (fval2 0.0d0) (hlgth 0.0d0)
           (resg 0.0d0) (resk 0.0d0) (reskh 0.0d0) (uflow 0.0d0))
      (declare (type (array double-float (25)) fv2 fv1)
               (type (double-float) uflow reskh resk resg hlgth fval2 fval1
                fsum fc epmach dhlgth centr absc)
               (type (f2cl-lib:integer4) jtwm1 jtw j))
      (setf epmach (f2cl-lib:d1mach 4))
      (setf uflow (f2cl-lib:d1mach 1))
      (setf centr (* 0.5d0 (+ a b)))
      (setf hlgth (* 0.5d0 (- b a)))
      (setf dhlgth (f2cl-lib:dabs hlgth))
      (setf fc
              (multiple-value-bind (ret-val var-0)
                  (funcall f centr)
                (declare (ignore))
                (when var-0 (setf centr var-0))
                ret-val))
      (setf resg (* (f2cl-lib:fref wg (13) ((1 13))) fc))
      (setf resk (* (f2cl-lib:fref wgk (26) ((1 26))) fc))
      (setf resabs (f2cl-lib:dabs resk))
      (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                    ((> j 12) nil)
        (tagbody
          (setf jtw (f2cl-lib:int-mul j 2))
          (setf absc (* hlgth (f2cl-lib:fref xgk (jtw) ((1 26)))))
          (setf fval1 (funcall f (- centr absc)))
          (setf fval2 (funcall f (+ centr absc)))
          (setf (f2cl-lib:fref fv1 (jtw) ((1 25))) fval1)
          (setf (f2cl-lib:fref fv2 (jtw) ((1 25))) fval2)
          (setf fsum (+ fval1 fval2))
          (setf resg (+ resg (* (f2cl-lib:fref wg (j) ((1 13))) fsum)))
          (setf resk (+ resk (* (f2cl-lib:fref wgk (jtw) ((1 26))) fsum)))
          (setf resabs
                  (+ resabs
                     (* (f2cl-lib:fref wgk (jtw) ((1 26)))
                        (+ (f2cl-lib:dabs fval1) (f2cl-lib:dabs fval2)))))
         label10))
      (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                    ((> j 13) nil)
        (tagbody
          (setf jtwm1 (f2cl-lib:int-sub (f2cl-lib:int-mul j 2) 1))
          (setf absc (* hlgth (f2cl-lib:fref xgk (jtwm1) ((1 26)))))
          (setf fval1 (funcall f (- centr absc)))
          (setf fval2 (funcall f (+ centr absc)))
          (setf (f2cl-lib:fref fv1 (jtwm1) ((1 25))) fval1)
          (setf (f2cl-lib:fref fv2 (jtwm1) ((1 25))) fval2)
          (setf fsum (+ fval1 fval2))
          (setf resk (+ resk (* (f2cl-lib:fref wgk (jtwm1) ((1 26))) fsum)))
          (setf resabs
                  (+ resabs
                     (* (f2cl-lib:fref wgk (jtwm1) ((1 26)))
                        (+ (f2cl-lib:dabs fval1) (f2cl-lib:dabs fval2)))))
         label15))
      (setf reskh (* resk 0.5d0))
      (setf resasc
              (* (f2cl-lib:fref wgk (26) ((1 26)))
                 (f2cl-lib:dabs (- fc reskh))))
      (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                    ((> j 25) nil)
        (tagbody
          (setf resasc
                  (+ resasc
                     (* (f2cl-lib:fref wgk (j) ((1 26)))
                        (+
                         (f2cl-lib:dabs
                          (- (f2cl-lib:fref fv1 (j) ((1 25))) reskh))
                         (f2cl-lib:dabs
                          (- (f2cl-lib:fref fv2 (j) ((1 25))) reskh))))))
         label20))
      (setf result (* resk hlgth))
      (setf resabs (* resabs dhlgth))
      (setf resasc (* resasc dhlgth))
      (setf abserr (f2cl-lib:dabs (* (- resk resg) hlgth)))
      (if (and (/= resasc 0.0d0) (/= abserr 0.0d0))
          (setf abserr
                  (* resasc
                     (f2cl-lib:dmin1 1.0d0
                                     (expt (/ (* 200.0d0 abserr) resasc)
                                           1.5d0)))))
      (if (> resabs (/ uflow (* 50.0d0 epmach)))
          (setf abserr (f2cl-lib:dmax1 (* epmach 50.0d0 resabs) abserr)))
      (go end_label)
     end_label
      (return (values nil nil nil result abserr resabs resasc)))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dqk51 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo :arg-types
                                            '(t (double-float) (double-float)
                                              (double-float) (double-float)
                                              (double-float) (double-float))
                                            :return-values
                                            '(nil nil nil
                                              fortran-to-lisp::result
                                              fortran-to-lisp::abserr
                                              fortran-to-lisp::resabs
                                              fortran-to-lisp::resasc)
                                            :calls '(fortran-to-lisp::d1mach))))

