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
       (make-array 4 :element-type 'double-float :initial-contents
                   '(0.1294849661688697d0 0.27970539148927664d0
                     0.3818300505051189d0 0.4179591836734694d0)))
      (xgk
       (make-array 8 :element-type 'double-float :initial-contents
                   '(0.9914553711208126d0 0.9491079123427585d0
                     0.8648644233597691d0 0.7415311855993945d0
                     0.5860872354676911d0 0.4058451513773972d0
                     0.20778495500789848d0 0.0d0)))
      (wgk
       (make-array 8 :element-type 'double-float :initial-contents
                   '(0.022935322010529224d0 0.06309209262997856d0
                     0.10479001032225019d0 0.14065325971552592d0
                     0.1690047266392679d0 0.19035057806478542d0
                     0.20443294007529889d0 0.20948214108472782d0))))
  (declare (type (array double-float (4)) wg)
           (type (array double-float (8)) xgk wgk))
  (defun dqk15 (f a b result abserr resabs resasc)
    (declare (type (double-float) resasc resabs abserr result b a))
    (prog ((fv1 (make-array 7 :element-type 'double-float))
           (fv2 (make-array 7 :element-type 'double-float)) (j 0) (jtw 0)
           (jtwm1 0) (absc 0.0d0) (centr 0.0d0) (dhlgth 0.0d0) (epmach 0.0d0)
           (fc 0.0d0) (fsum 0.0d0) (fval1 0.0d0) (fval2 0.0d0) (hlgth 0.0d0)
           (resg 0.0d0) (resk 0.0d0) (reskh 0.0d0) (uflow 0.0d0))
      (declare (type (array double-float (7)) fv2 fv1)
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
      (setf resg (* fc (f2cl-lib:fref wg (4) ((1 4)))))
      (setf resk (* fc (f2cl-lib:fref wgk (8) ((1 8)))))
      (setf resabs (f2cl-lib:dabs resk))
      (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                    ((> j 3) nil)
        (tagbody
          (setf jtw (f2cl-lib:int-mul j 2))
          (setf absc (* hlgth (f2cl-lib:fref xgk (jtw) ((1 8)))))
          (setf fval1 (funcall f (- centr absc)))
          (setf fval2 (funcall f (+ centr absc)))
          (setf (f2cl-lib:fref fv1 (jtw) ((1 7))) fval1)
          (setf (f2cl-lib:fref fv2 (jtw) ((1 7))) fval2)
          (setf fsum (+ fval1 fval2))
          (setf resg (+ resg (* (f2cl-lib:fref wg (j) ((1 4))) fsum)))
          (setf resk (+ resk (* (f2cl-lib:fref wgk (jtw) ((1 8))) fsum)))
          (setf resabs
                  (+ resabs
                     (* (f2cl-lib:fref wgk (jtw) ((1 8)))
                        (+ (f2cl-lib:dabs fval1) (f2cl-lib:dabs fval2)))))
         label10))
      (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                    ((> j 4) nil)
        (tagbody
          (setf jtwm1 (f2cl-lib:int-sub (f2cl-lib:int-mul j 2) 1))
          (setf absc (* hlgth (f2cl-lib:fref xgk (jtwm1) ((1 8)))))
          (setf fval1 (funcall f (- centr absc)))
          (setf fval2 (funcall f (+ centr absc)))
          (setf (f2cl-lib:fref fv1 (jtwm1) ((1 7))) fval1)
          (setf (f2cl-lib:fref fv2 (jtwm1) ((1 7))) fval2)
          (setf fsum (+ fval1 fval2))
          (setf resk (+ resk (* (f2cl-lib:fref wgk (jtwm1) ((1 8))) fsum)))
          (setf resabs
                  (+ resabs
                     (* (f2cl-lib:fref wgk (jtwm1) ((1 8)))
                        (+ (f2cl-lib:dabs fval1) (f2cl-lib:dabs fval2)))))
         label15))
      (setf reskh (* resk 0.5d0))
      (setf resasc
              (* (f2cl-lib:fref wgk (8) ((1 8))) (f2cl-lib:dabs (- fc reskh))))
      (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                    ((> j 7) nil)
        (tagbody
          (setf resasc
                  (+ resasc
                     (* (f2cl-lib:fref wgk (j) ((1 8)))
                        (+
                         (f2cl-lib:dabs
                          (- (f2cl-lib:fref fv1 (j) ((1 7))) reskh))
                         (f2cl-lib:dabs
                          (- (f2cl-lib:fref fv2 (j) ((1 7))) reskh))))))
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
  (setf (gethash 'fortran-to-lisp::dqk15 fortran-to-lisp::*f2cl-function-info*)
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

