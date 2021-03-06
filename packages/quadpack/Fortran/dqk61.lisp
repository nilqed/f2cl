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
       (make-array 15 :element-type 'double-float :initial-contents
                   '(0.007968192496166605d0 0.01846646831109096d0
                     0.02878470788332337d0 0.03879919256962705d0
                     0.04840267283059405d0 0.057493156217619065d0
                     0.06597422988218049d0 0.0737559747377052d0
                     0.08075589522942021d0 0.08689978720108298d0
                     0.09212252223778612d0 0.09636873717464425d0
                     0.09959342058679527d0 0.1017623897484055d0
                     0.10285265289355884d0)))
      (xgk
       (make-array 31 :element-type 'double-float :initial-contents
                   '(0.9994844100504906d0 0.9968934840746495d0
                     0.9916309968704046d0 0.9836681232797472d0
                     0.9731163225011262d0 0.9600218649683075d0
                     0.94437444474856d0 0.9262000474292743d0
                     0.9055733076999078d0 0.8825605357920527d0
                     0.8572052335460612d0 0.8295657623827684d0
                     0.799727835821839d0 0.7677774321048262d0
                     0.7337900624532268d0 0.6978504947933158d0
                     0.6600610641266269d0 0.6205261829892429d0
                     0.5793452358263617d0 0.5366241481420199d0
                     0.49248046786177857d0 0.44703376953808915d0
                     0.4004012548303944d0 0.3527047255308781d0
                     0.30407320227362505d0 0.25463692616788985d0
                     0.20452511668230988d0 0.15386991360858354d0
                     0.10280693796673702d0 0.0514718425553177d0 0.0d0)))
      (wgk
       (make-array 31 :element-type 'double-float :initial-contents
                   '(0.0013890136986770077d0 0.003890461127099884d0
                     0.0066307039159312926d0 0.009273279659517764d0
                     0.011823015253496341d0 0.014369729507045804d0
                     0.01692088918905327d0 0.019414141193942382d0
                     0.021828035821609193d0 0.0241911620780806d0
                     0.0265099548823331d0 0.02875404876504129d0
                     0.030907257562387762d0 0.03298144705748372d0
                     0.034979338028060025d0 0.03688236465182123d0
                     0.038678945624727595d0 0.040374538951535956d0
                     0.041969810215164244d0 0.04345253970135607d0
                     0.04481480013316266d0 0.04605923827100699d0
                     0.04718554656929915d0 0.04818586175708713d0
                     0.04905543455502978d0 0.04979568342707421d0
                     0.05040592140278235d0 0.05088179589874961d0
                     0.051221547849258774d0 0.05142612853745902d0
                     0.05149472942945157d0))))
  (declare (type (array double-float (15)) wg)
           (type (array double-float (31)) xgk wgk))
  (defun dqk61 (f a b result abserr resabs resasc)
    (declare (type (double-float) resasc resabs abserr result b a))
    (prog ((fv1 (make-array 30 :element-type 'double-float))
           (fv2 (make-array 30 :element-type 'double-float)) (j 0) (jtw 0)
           (jtwm1 0) (dabsc 0.0d0) (centr 0.0d0) (dhlgth 0.0d0) (epmach 0.0d0)
           (fc 0.0d0) (fsum 0.0d0) (fval1 0.0d0) (fval2 0.0d0) (hlgth 0.0d0)
           (resg 0.0d0) (resk 0.0d0) (reskh 0.0d0) (uflow 0.0d0))
      (declare (type (array double-float (30)) fv2 fv1)
               (type (double-float) uflow reskh resk resg hlgth fval2 fval1
                fsum fc epmach dhlgth centr dabsc)
               (type (f2cl-lib:integer4) jtwm1 jtw j))
      (setf epmach (f2cl-lib:d1mach 4))
      (setf uflow (f2cl-lib:d1mach 1))
      (setf centr (* 0.5d0 (+ b a)))
      (setf hlgth (* 0.5d0 (- b a)))
      (setf dhlgth (f2cl-lib:dabs hlgth))
      (setf resg 0.0d0)
      (setf fc
              (multiple-value-bind (ret-val var-0)
                  (funcall f centr)
                (declare (ignore))
                (when var-0 (setf centr var-0))
                ret-val))
      (setf resk (* (f2cl-lib:fref wgk (31) ((1 31))) fc))
      (setf resabs (f2cl-lib:dabs resk))
      (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                    ((> j 15) nil)
        (tagbody
          (setf jtw (f2cl-lib:int-mul j 2))
          (setf dabsc (* hlgth (f2cl-lib:fref xgk (jtw) ((1 31)))))
          (setf fval1 (funcall f (- centr dabsc)))
          (setf fval2 (funcall f (+ centr dabsc)))
          (setf (f2cl-lib:fref fv1 (jtw) ((1 30))) fval1)
          (setf (f2cl-lib:fref fv2 (jtw) ((1 30))) fval2)
          (setf fsum (+ fval1 fval2))
          (setf resg (+ resg (* (f2cl-lib:fref wg (j) ((1 15))) fsum)))
          (setf resk (+ resk (* (f2cl-lib:fref wgk (jtw) ((1 31))) fsum)))
          (setf resabs
                  (+ resabs
                     (* (f2cl-lib:fref wgk (jtw) ((1 31)))
                        (+ (f2cl-lib:dabs fval1) (f2cl-lib:dabs fval2)))))
         label10))
      (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                    ((> j 15) nil)
        (tagbody
          (setf jtwm1 (f2cl-lib:int-sub (f2cl-lib:int-mul j 2) 1))
          (setf dabsc (* hlgth (f2cl-lib:fref xgk (jtwm1) ((1 31)))))
          (setf fval1 (funcall f (- centr dabsc)))
          (setf fval2 (funcall f (+ centr dabsc)))
          (setf (f2cl-lib:fref fv1 (jtwm1) ((1 30))) fval1)
          (setf (f2cl-lib:fref fv2 (jtwm1) ((1 30))) fval2)
          (setf fsum (+ fval1 fval2))
          (setf resk (+ resk (* (f2cl-lib:fref wgk (jtwm1) ((1 31))) fsum)))
          (setf resabs
                  (+ resabs
                     (* (f2cl-lib:fref wgk (jtwm1) ((1 31)))
                        (+ (f2cl-lib:dabs fval1) (f2cl-lib:dabs fval2)))))
         label15))
      (setf reskh (* resk 0.5d0))
      (setf resasc
              (* (f2cl-lib:fref wgk (31) ((1 31)))
                 (f2cl-lib:dabs (- fc reskh))))
      (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                    ((> j 30) nil)
        (tagbody
          (setf resasc
                  (+ resasc
                     (* (f2cl-lib:fref wgk (j) ((1 31)))
                        (+
                         (f2cl-lib:dabs
                          (- (f2cl-lib:fref fv1 (j) ((1 30))) reskh))
                         (f2cl-lib:dabs
                          (- (f2cl-lib:fref fv2 (j) ((1 30))) reskh))))))
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
  (setf (gethash 'fortran-to-lisp::dqk61 fortran-to-lisp::*f2cl-function-info*)
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

