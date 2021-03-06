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
       (make-array 10 :element-type 'double-float :initial-contents
                   '(0.017614007139152118d0 0.04060142980038694d0
                     0.06267204833410907d0 0.08327674157670475d0
                     0.10193011981724044d0 0.11819453196151841d0
                     0.13168863844917664d0 0.14209610931838204d0
                     0.14917298647260374d0 0.15275338713072584d0)))
      (xgk
       (make-array 21 :element-type 'double-float :initial-contents
                   '(0.9988590315882777d0 0.9931285991850949d0
                     0.9815078774502503d0 0.9639719272779138d0
                     0.9408226338317548d0 0.912234428251326d0
                     0.878276811252282d0 0.8391169718222188d0
                     0.7950414288375512d0 0.7463319064601508d0
                     0.6932376563347514d0 0.636053680726515d0
                     0.5751404468197103d0 0.5108670019508271d0
                     0.4435931752387251d0 0.37370608871541955d0
                     0.301627868114913d0 0.22778585114164507d0
                     0.15260546524092267d0 0.07652652113349734d0 0.0d0)))
      (wgk
       (make-array 21 :element-type 'double-float :initial-contents
                   '(0.0030735837185205317d0 0.008600269855642943d0
                     0.014626169256971253d0 0.020388373461266523d0
                     0.02588213360495116d0 0.0312873067770328d0
                     0.036600169758200796d0 0.041668873327973685d0
                     0.04643482186749767d0 0.05094457392372869d0
                     0.05519510534828599d0 0.05911140088063957d0
                     0.06265323755478117d0 0.06583459713361842d0
                     0.06864867292852161d0 0.07105442355344407d0
                     0.07303069033278667d0 0.07458287540049918d0
                     0.07570449768455667d0 0.07637786767208074d0
                     0.07660071191799965d0))))
  (declare (type (array double-float (10)) wg)
           (type (array double-float (21)) xgk wgk))
  (defun dqk41 (f a b result abserr resabs resasc)
    (declare (type (double-float) resasc resabs abserr result b a))
    (prog ((fv1 (make-array 20 :element-type 'double-float))
           (fv2 (make-array 20 :element-type 'double-float)) (j 0) (jtw 0)
           (jtwm1 0) (absc 0.0d0) (centr 0.0d0) (dhlgth 0.0d0) (epmach 0.0d0)
           (fc 0.0d0) (fsum 0.0d0) (fval1 0.0d0) (fval2 0.0d0) (hlgth 0.0d0)
           (resg 0.0d0) (resk 0.0d0) (reskh 0.0d0) (uflow 0.0d0))
      (declare (type (array double-float (20)) fv2 fv1)
               (type (double-float) uflow reskh resk resg hlgth fval2 fval1
                fsum fc epmach dhlgth centr absc)
               (type (f2cl-lib:integer4) jtwm1 jtw j))
      (setf epmach (f2cl-lib:d1mach 4))
      (setf uflow (f2cl-lib:d1mach 1))
      (setf centr (* 0.5d0 (+ a b)))
      (setf hlgth (* 0.5d0 (- b a)))
      (setf dhlgth (f2cl-lib:dabs hlgth))
      (setf resg 0.0d0)
      (setf fc
              (multiple-value-bind (ret-val var-0)
                  (funcall f centr)
                (declare (ignore))
                (when var-0 (setf centr var-0))
                ret-val))
      (setf resk (* (f2cl-lib:fref wgk (21) ((1 21))) fc))
      (setf resabs (f2cl-lib:dabs resk))
      (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                    ((> j 10) nil)
        (tagbody
          (setf jtw (f2cl-lib:int-mul j 2))
          (setf absc (* hlgth (f2cl-lib:fref xgk (jtw) ((1 21)))))
          (setf fval1 (funcall f (- centr absc)))
          (setf fval2 (funcall f (+ centr absc)))
          (setf (f2cl-lib:fref fv1 (jtw) ((1 20))) fval1)
          (setf (f2cl-lib:fref fv2 (jtw) ((1 20))) fval2)
          (setf fsum (+ fval1 fval2))
          (setf resg (+ resg (* (f2cl-lib:fref wg (j) ((1 10))) fsum)))
          (setf resk (+ resk (* (f2cl-lib:fref wgk (jtw) ((1 21))) fsum)))
          (setf resabs
                  (+ resabs
                     (* (f2cl-lib:fref wgk (jtw) ((1 21)))
                        (+ (f2cl-lib:dabs fval1) (f2cl-lib:dabs fval2)))))
         label10))
      (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                    ((> j 10) nil)
        (tagbody
          (setf jtwm1 (f2cl-lib:int-sub (f2cl-lib:int-mul j 2) 1))
          (setf absc (* hlgth (f2cl-lib:fref xgk (jtwm1) ((1 21)))))
          (setf fval1 (funcall f (- centr absc)))
          (setf fval2 (funcall f (+ centr absc)))
          (setf (f2cl-lib:fref fv1 (jtwm1) ((1 20))) fval1)
          (setf (f2cl-lib:fref fv2 (jtwm1) ((1 20))) fval2)
          (setf fsum (+ fval1 fval2))
          (setf resk (+ resk (* (f2cl-lib:fref wgk (jtwm1) ((1 21))) fsum)))
          (setf resabs
                  (+ resabs
                     (* (f2cl-lib:fref wgk (jtwm1) ((1 21)))
                        (+ (f2cl-lib:dabs fval1) (f2cl-lib:dabs fval2)))))
         label15))
      (setf reskh (* resk 0.5d0))
      (setf resasc
              (* (f2cl-lib:fref wgk (21) ((1 21)))
                 (f2cl-lib:dabs (- fc reskh))))
      (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                    ((> j 20) nil)
        (tagbody
          (setf resasc
                  (+ resasc
                     (* (f2cl-lib:fref wgk (j) ((1 21)))
                        (+
                         (f2cl-lib:dabs
                          (- (f2cl-lib:fref fv1 (j) ((1 20))) reskh))
                         (f2cl-lib:dabs
                          (- (f2cl-lib:fref fv2 (j) ((1 20))) reskh))))))
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
  (setf (gethash 'fortran-to-lisp::dqk41 fortran-to-lisp::*f2cl-function-info*)
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

