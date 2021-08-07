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

(defun test-vlerp (n a b)
  (3d-vectors:vlerp a b n))

(test keyframe-sequence-create
  (let ((seq (kf:create-simple-keyframe-sequence
              (list 1 2 3 4)
              :interpolator #'lerp)))
    (is-true (= 4 (keyframe-count seq)))))

(test keyframe-sequence-empty
  (let ((seq (kf:create-simple-keyframe-sequence
              (list))))
    (is (= 0 (keyframe-count seq)))
    (signals error (value-at seq 0))
    (signals error (value-at seq 1.0))))

(test keyframe-sequence-single-value
  (let* ((v 4)
         (seq (kf:create-simple-keyframe-sequence (list v))))
    (is (= 1 (keyframe-count seq)))
    (is (near v (value-at seq -1.0)))
    (is (near v (value-at seq 0.0)))
    (is (near v (value-at seq 0.5)))
    (is (near v (value-at seq 1.0)))
    (is (near v (value-at seq 2.0)))))

(test keyframe-sequence-single-value-vectors
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
  (let* ((v1 0.0)
         (v2 1.0)
         (one-third (lerp (/ 1.0 3.0) v1 v2))
         (midpoint (lerp 0.5 v1 v2))
         (two-thirds (lerp (/ 2.0 3.0) v1 v2))
         (seq (kf:create-simple-keyframe-sequence (list v1 v2)
                                                  :interpolator #'lerp
                                                  :time-scale 1.0)))
    (is (= 2 (keyframe-count seq)))
    (is (near v1 (value-at seq -1.0)))
    (is (near v1 (value-at seq 0.0)))
    (is (near one-third (value-at seq (/ 1.0 3.0))))
    (is (near midpoint (value-at seq 0.5)))
    (is (near two-thirds (value-at seq (/ 2.0 3.0))))
    (is (near v2 (value-at seq 2.0)))
    (is (near v2 (value-at seq 3.0)))))

(test keyframe-sequence-two-values-vectors
  (let* ((v1 (vec3 0.0 0.0 0.0))
         (v2 (vec3 1.0 0.0 0.0))
         (one-third (test-vlerp (/ 1.0 3.0) v1 v2))
         (midpoint (test-vlerp 0.5 v1 v2))
         (two-thirds (test-vlerp (/ 2.0 3.0) v1 v2))
         (seq (kf:create-simple-keyframe-sequence
               (list v1 v2) :interpolator #'test-vlerp
                            :time-scale 1.0)))
    (is (= 2 (keyframe-count seq)))
    (is (near v1 (value-at seq -1.0)))
    (is (near v1 (value-at seq 0.0)))
    (is (near one-third (value-at seq (/ 1.0 3.0))))
    (is (near midpoint (value-at seq (/ 1.0 2.0))))
    (is (near two-thirds (value-at seq (/ 2.0 3.0))))
    (is (near v2 (value-at seq 1.0)))
    (is (near v2 (value-at seq 3.0)))))

(test keyframe-sequence-multiple-values
  (let* ((v1 0.0)
         (v2 1.0)
         (v3 3.0)
         (v4 4.0)
         (seq (kf:create-keyframe-sequence (list
                                               (kf:create-keyframe v1 0.0 :interpolator #'lerp)
                                               (kf:create-keyframe v2 1.0 :interpolator #'lerp)
                                               (kf:create-keyframe v3 2.0 :interpolator #'lerp)
                                               (kf:create-keyframe v4 4.0 :interpolator #'lerp)
                                               (kf:create-keyframe v1 5.0 :interpolator #'lerp)))))

    (is (= 5 (keyframe-count seq)))
    (is (near v1 (value-at seq -1.0)))
    (is (near v1 (value-at seq 0.0)))
    (is (near 0.5 (value-at seq 0.5)))
    (is (near v2 (value-at seq 1.0)))
    (is (near v3 (value-at seq 2.0)))
    (is (near 3.5 (value-at seq 3.0)))))

(test keyframe-sequence-multiple-values-vectors
  (let* ((v1 (vec3 0.0 0.0 0.0))
         (v2 (vec3 1.0 0.0 0.0))
         (v3 (vec3 1.0 1.0 0.0))
         (v4 (vec3 0.0 1.0 0.0))
         (seq (kf:create-keyframe-sequence (list
                                               (kf:create-keyframe v1 0.0 :interpolator #'test-vlerp)
                                               (kf:create-keyframe v2 1.0 :interpolator #'test-vlerp)
                                               (kf:create-keyframe v3 2.0 :interpolator #'test-vlerp)
                                               (kf:create-keyframe v4 4.0 :interpolator #'test-vlerp)
                                               (kf:create-keyframe v1 5.0 :interpolator #'test-vlerp)))))

    (is (= 5 (keyframe-count seq)))
    (is (near v1 (value-at seq -1.0)))
    (is (near v1 (value-at seq 0.0)))
    (is (near (vec3 0.5 0.0 0.0) (value-at seq 0.5)))
    (is (near v2 (value-at seq 1.0)))
    (is (near v3 (value-at seq 2.0)))
    (is (near (vec3 0.5 1.0 0.0) (value-at seq 3.0)))))

(test keyframe-sequence-repeating
  (let* ((v1 0.0)
         (v2 1.0)
         (seq (kf:create-keyframe-sequence (list
                                               (kf:create-keyframe v1 0.0 :interpolator #'lerp)
                                               (kf:create-keyframe v2 1.0 :interpolator #'lerp))
                                              :before :repeat
                                              :after :repeat)))

    (is (= 2 (keyframe-count seq)))
    (is (near v2 (value-at seq -1.0)))
    (is (near v1 (value-at seq 0.0)))
    (is (near 0.5 (value-at seq 0.5)))
    (is (near 0.5 (value-at seq 1.5)))
    (is (near 0.5 (value-at seq 2.5)))
    (is (near 0.5 (value-at seq 3.5)))))

(test keyframe-sequence-repeating-vectors
  (let* ((v1 (vec3 0.0 0.0 0.0))
         (v2 (vec3 1.0 0.0 0.0))
         (seq (kf:create-keyframe-sequence (list
                                               (kf:create-keyframe v1 0.0 :interpolator #'test-vlerp)
                                               (kf:create-keyframe v2 1.0 :interpolator #'test-vlerp))
                                              :before :repeat
                                              :after :repeat)))

    (is (= 2 (keyframe-count seq)))
    (is (near v2 (value-at seq -1.0)))
    (is (near v1 (value-at seq 0.0)))
    (is (near (vec3 0.5 0.0 0.0) (value-at seq 0.5)))
    (is (near (vec3 0.5 0.0 0.0) (value-at seq 1.5)))
    (is (near (vec3 0.5 0.0 0.0) (value-at seq 2.5)))
    (is (near (vec3 0.5 0.0 0.0) (value-at seq 3.5)))))

