;; package.lisp
;;
;; Copyright (c) 2021 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>


;; Permission to use, copy, modify, and/or distribute this software for any
;; purpose with or without fee is hereby granted, provided that the above
;; copyright notice and this permission notice appear in all copies.

;; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;; WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;; MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;; ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;; ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;; OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

(in-package :cl-user)
(defpackage :keyframe.test
  (:use :cl
   :fiveam
   :3d-vectors
        :alexandria
        :keyframe))

(in-package :keyframe.test)

(defgeneric near (a b))
(defmethod near ((a real) (b real))
  (< (abs (- b a)) 0.00001))

(defmethod near ((a vec3) (b vec3))
  (v< (vabs (v- b a)) (vec3 0.00001 0.00001 0.00001)))

(defmethod near ((a vec2) (b vec2))
  (v< (vabs (v- b a)) (vec2 0.00001 0.00001)))

(defmethod near ((a t) (b t))
  (= a b))

(test keyframe-sequence-create

  (let ((seq (kf:create-simple-keyframe-sequence
              (list (vec3  1.0  1.0 0.0)
                    (vec3  1.0 -1.0 0.0)
                    (vec3 -1.0 -1.0 0.0)
                    (vec3 -1.0  1.0 0.0)))))
    (is-true (= 4 (keyframe-count seq)))))

(test keyframe-sequence-empty
  (let ((seq (kf:create-simple-keyframe-sequence
              (list))))
    (is (= 0 (keyframe-count seq)))
    (signals error (value-at seq 0))
    (signals error (value-at seq 1.0))))

(test keyframe-sequence-single-value
  (let* ((v (vec3 0.0 1.0 0.0))
         (seq (kf:create-simple-keyframe-sequence
               (list v))))
    (is (= 1 (keyframe-count seq)))
    (is (near v (value-at seq -1.0)))
    (is (near v (value-at seq 0.0)))
    (is (near v (value-at seq 0.5)))
    (is (near v (value-at seq 1.0)))
    (is (near v (value-at seq 2.0)))))

(test keyframe-sequence-two-values
  (let* ((v1 (vec3 0.0 0.0 0.0))
         (v2 (vec3 1.0 0.0 0.0))
         (one-third (vlerp v1 v2 (/ 1.0 3.0)))
         (midpoint (vlerp v1 v2 0.5))
         (two-thirds (vlerp v1 v2 (/ 2.0 3.0)))
         (seq (kf:create-simple-keyframe-sequence
               (list v1 v2) :interpolator #'vlerp
                            :time-scale 2.0)))
    (is (= 2 (keyframe-count seq)))
    (is (near v1 (value-at seq -1.0)))
    (is (near v1 (value-at seq 0.0)))
    (is (near one-third (value-at seq (/ 2.0 3.0))))
    (is (near midpoint (value-at seq 1.0)))
    (is (near two-thirds (value-at seq (/ 4.0 3.0))))
    (is (near v2 (value-at seq 2.0)))
    (is (near v2 (value-at seq 3.0)))))


(test keyframe-sequence-multiple-values
  (let* ((v1 (vec3 0.0 0.0 0.0))
         (v2 (vec3 1.0 0.0 0.0))
         (v3 (vec3 1.0 1.0 0.0))
         (v4 (vec3 0.0 1.0 0.0))
         (seq (kf:create-keyframe-sequence (list
                                               (kf:create-keyframe v1 0.0 :interpolator #'vlerp)
                                               (kf:create-keyframe v2 1.0 :interpolator #'vlerp)
                                               (kf:create-keyframe v3 2.0 :interpolator #'vlerp)
                                               (kf:create-keyframe v4 4.0 :interpolator #'vlerp)
                                               (kf:create-keyframe v1 5.0 :interpolator #'vlerp)))))

    (is (= 5 (keyframe-count seq)))
    (is (near v1 (value-at seq -1.0)))
    (is (near v1 (value-at seq 0.0)))
    (is (near (vec3 0.5 0.0 0.0) (value-at seq 0.5)))
    (is (near v2 (value-at seq 1.0)))
    (is (near v3 (value-at seq 2.0)))
    (is (near (vec3 0.5 1.0 0.0) (value-at seq 3.0)))))

(test keyframe-sequence-repeating
  (let* ((v1 (vec3 0.0 0.0 0.0))
         (v2 (vec3 1.0 0.0 0.0))
         (seq (kf:create-keyframe-sequence (list
                                               (kf:create-keyframe v1 0.0 :interpolator #'vlerp)
                                               (kf:create-keyframe v2 1.0 :interpolator #'vlerp)
                                               (kf:create-keyframe v1 2.0 :interpolator #'vlerp))
                                              :before :repeat
                                              :after :repeat)))

    (is (= 3 (keyframe-count seq)))
    (is (near v2 (value-at seq -1.0)))
    (is (near v1 (value-at seq 0.0)))
    (is (near (vec3 0.5 0.0 0.0) (value-at seq 0.5)))
    (is (near (vec3 0.5 0.0 0.0) (value-at seq 1.5)))
    (is (near (vec3 0.5 0.0 0.0) (value-at seq 2.5)))
    (is (near (vec3 0.5 0.0 0.0) (value-at seq 3.5)))))