;;; Compiled by f2cl version:
;;; ("" "" "" "" "" "" "")

;;; Using Lisp SBCL 1.4.2
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing t) (:declare-common nil)
;;;           (:float-format single-float))

(in-package "LAPACK")


(let* ((zero 0.0d0) (half 0.5d0) (one 1.0d0) (two 2.0d0) (four 4.0d0))
  (declare (type (double-float 0.0d0 0.0d0) zero)
           (type (double-float 0.5d0 0.5d0) half)
           (type (double-float 1.0d0 1.0d0) one)
           (type (double-float 2.0d0 2.0d0) two)
           (type (double-float 4.0d0 4.0d0) four)
           (ignorable zero half one two four))
  (defun dlasv2 (f g h ssmin ssmax snr csr snl csl)
    (declare (type (double-float) csl snl csr snr ssmax ssmin h g f))
    (prog ((a 0.0d0) (clt 0.0d0) (crt 0.0d0) (d 0.0d0) (fa 0.0d0) (ft 0.0d0)
           (ga 0.0d0) (gt 0.0d0) (ha 0.0d0) (ht 0.0d0) (l 0.0d0) (m 0.0d0)
           (mm 0.0d0) (r 0.0d0) (s 0.0d0) (slt 0.0d0) (srt 0.0d0) (t$ 0.0d0)
           (temp 0.0d0) (tsign 0.0d0) (tt 0.0d0) (pmax 0) (gasmal nil)
           (swap nil))
      (declare
       (type (double-float) a clt crt d fa ft ga gt ha ht l m mm r s slt srt t$
        temp tsign tt)
       (type (f2cl-lib:integer4) pmax)
       (type f2cl-lib:logical gasmal swap))
      (setf ft f)
      (setf fa (abs ft))
      (setf ht h)
      (setf ha (abs h))
      (setf pmax 1)
      (setf swap (> ha fa))
      (cond
       (swap (setf pmax 3) (setf temp ft) (setf ft ht) (setf ht temp)
        (setf temp fa) (setf fa ha) (setf ha temp)))
      (setf gt g)
      (setf ga (abs gt))
      (cond
       ((= ga zero) (setf ssmin ha) (setf ssmax fa) (setf clt one)
        (setf crt one) (setf slt zero) (setf srt zero))
       (t (setf gasmal f2cl-lib:%true%)
        (cond
         ((> ga fa) (setf pmax 2)
          (cond
           ((< (f2cl-lib:f2cl/ fa ga) (dlamch "EPS"))
            (setf gasmal f2cl-lib:%false%) (setf ssmax ga)
            (cond ((> ha one) (setf ssmin (/ fa (/ ga ha))))
                  (t (setf ssmin (* (/ fa ga) ha))))
            (setf clt one) (setf slt (/ ht gt)) (setf srt one)
            (setf crt (/ ft gt))))))
        (cond
         (gasmal (setf d (- fa ha))
          (cond ((= d fa) (setf l one)) (t (setf l (/ d fa))))
          (setf m (/ gt ft)) (setf t$ (- two l)) (setf mm (* m m))
          (setf tt (* t$ t$)) (setf s (f2cl-lib:fsqrt (+ tt mm)))
          (cond ((= l zero) (setf r (abs m)))
                (t (setf r (f2cl-lib:fsqrt (+ (* l l) mm)))))
          (setf a (* half (+ s r))) (setf ssmin (/ ha a)) (setf ssmax (* fa a))
          (cond
           ((= mm zero)
            (cond
             ((= l zero)
              (setf t$ (* (f2cl-lib:sign two ft) (f2cl-lib:sign one gt))))
             (t (setf t$ (+ (/ gt (f2cl-lib:sign d ft)) (/ m t$))))))
           (t (setf t$ (* (+ (/ m (+ s t$)) (/ m (+ r l))) (+ one a)))))
          (setf l (f2cl-lib:fsqrt (+ (* t$ t$) four))) (setf crt (/ two l))
          (setf srt (/ t$ l)) (setf clt (/ (+ crt (* srt m)) a))
          (setf slt (/ (* (/ ht ft) srt) a))))))
      (cond (swap (setf csl srt) (setf snl crt) (setf csr slt) (setf snr clt))
            (t (setf csl clt) (setf snl slt) (setf csr crt) (setf snr srt)))
      (if (= pmax 1)
          (setf tsign
                  (* (f2cl-lib:sign one csr) (f2cl-lib:sign one csl)
                     (f2cl-lib:sign one f))))
      (if (= pmax 2)
          (setf tsign
                  (* (f2cl-lib:sign one snr) (f2cl-lib:sign one csl)
                     (f2cl-lib:sign one g))))
      (if (= pmax 3)
          (setf tsign
                  (* (f2cl-lib:sign one snr) (f2cl-lib:sign one snl)
                     (f2cl-lib:sign one h))))
      (setf ssmax (f2cl-lib:sign ssmax tsign))
      (setf ssmin
              (f2cl-lib:sign ssmin
                             (* tsign (f2cl-lib:sign one f)
                                (f2cl-lib:sign one h))))
      (go end_label)
     end_label
      (return (values nil nil nil ssmin ssmax snr csr snl csl)))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dlasv2
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo :arg-types
                                            '((double-float) (double-float)
                                              (double-float) (double-float)
                                              (double-float) (double-float)
                                              (double-float) (double-float)
                                              (double-float))
                                            :return-values
                                            '(nil nil nil
                                              fortran-to-lisp::ssmin
                                              fortran-to-lisp::ssmax
                                              fortran-to-lisp::snr
                                              fortran-to-lisp::csr
                                              fortran-to-lisp::snl
                                              fortran-to-lisp::csl)
                                            :calls '(fortran-to-lisp::dlamch))))

