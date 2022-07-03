;;; Compiled by f2cl version:
;;; ("" "" "" "" "" "" "")

;;; Using Lisp SBCL 1.4.2
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing t) (:declare-common nil)
;;;           (:float-format single-float))

(in-package "FISHPACK")


(defun cosgen (n ijump fnum fden a)
  (declare (type (array single-float (*)) a)
           (type (single-float) fden fnum)
           (type (f2cl-lib:integer4) ijump n))
  (f2cl-lib:with-multi-array-data
      ((a single-float a-%data% a-%offset%))
    (prog ((y 0.0) (np1 0) (k2 0) (x 0.0) (i 0) (k5 0) (k1 0) (k 0) (pibyn 0.0)
           (k4 0) (k3 0) (dum 0.0) (pi$ 0.0))
      (declare (type (f2cl-lib:integer4) k3 k4 k k1 k5 i k2 np1)
               (type (single-float) pi$ dum pibyn x y))
      (setf pi$ (pimach dum))
      (if (= n 0)
          (go label105))
      (if (= ijump 1)
          (go label103))
      (setf k3 (+ (the f2cl-lib:integer4 (truncate n ijump)) 1))
      (setf k4 (f2cl-lib:int-sub k3 1))
      (setf pibyn (/ pi$ (f2cl-lib:ffloat (f2cl-lib:int-add n ijump))))
      (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                    ((> k ijump) nil)
        (tagbody
          (setf k1 (f2cl-lib:int-mul (f2cl-lib:int-sub k 1) k3))
          (setf k5 (f2cl-lib:int-mul (f2cl-lib:int-sub k 1) k4))
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i k4) nil)
            (tagbody
              (setf x
                      (coerce (the f2cl-lib:integer4 (f2cl-lib:int-add k1 i))
                              'single-float))
              (setf k2 (f2cl-lib:int-add k5 i))
              (setf (f2cl-lib:fref a-%data% (k2) ((1 1)) a-%offset%)
                      (* -2.0 (cos (* x pibyn))))
             label101))
         label102))
      (go label105)
     label103
      (setf np1 (f2cl-lib:int-add n 1))
      (setf y (/ pi$ (+ (f2cl-lib:ffloat n) fden)))
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i n) nil)
        (tagbody
          (setf x (- (f2cl-lib:ffloat (f2cl-lib:int-sub np1 i)) fnum))
          (setf (f2cl-lib:fref a-%data% (i) ((1 1)) a-%offset%)
                  (* 2.0 (cos (* x y))))
         label104))
     label105
      (go end_label)
     end_label
      (return (values nil nil nil nil nil)))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::cosgen
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo :arg-types
                                            '((fortran-to-lisp::integer4)
                                              (fortran-to-lisp::integer4)
                                              (single-float) (single-float)
                                              (array single-float (*)))
                                            :return-values
                                            '(nil nil nil nil nil) :calls
                                            '(fortran-to-lisp::pimach))))

