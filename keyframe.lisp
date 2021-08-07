;; keyframe.lisp
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

(in-package :keyframe)

(defclass keyframe-sequence ()
  ((frames :initarg :frames)
   (before-behavior :initarg :before :initform :clamp)
   (after-behavior :initarg :after :initform :clamp))
  (:documentation "A sequence of keyframes where values in between frames are calculated using interpolation.~
                   :before and :after can be :clamp or :repeat"))

(defclass keyframe ()
  ((value :initarg :value :accessor keyframe-value)
   (start-time :initarg :start-time :type number)
   (interpolator :initarg :interpolator :initform #'lerp))
  (:documentation "An item in a keyframe sequence.  The keyframe sequence has the specified value at time start-time.~
                   The sequence interpolates to this frame using the interpolator function."))


(defgeneric start-time (keyframe)
  (:documentation "Return the start time of a keyframe or keyframe-sequence."))

(defgeneric end-time (keyframe)
  (:documentation "Return the end time of a keyframe or keyframe-sequence."))

(defgeneric keyframe-count (sequence)
  (:documentation "Return the number of keyframes in sequence."))

(defgeneric value-at (sequence time)
  (:documentation "Return the value of keyframe-sequence at time t."))




(defmethod start-time ((keyframe keyframe))
  (slot-value keyframe 'start-time))

(defmethod start-time ((key-seq keyframe-sequence))
  (start-time (aref (slot-value key-seq 'frames) 0)))




(defmethod end-time ((keyframe keyframe))
  (slot-value keyframe 'start-time))

(defmethod end-time ((key-seq keyframe-sequence))
  (with-slots (frames) key-seq
      (end-time (aref frames (1- (length frames))))))



(defmethod keyframe-count ((sequence keyframe-sequence))
  (with-slots (frames) sequence
    (length frames)))




(defun compute-repeating-time (sequence real-time)
  "Compute the 'canonical' time of real-time for sequence.  The canonical time is the time taking~
   into account start and end behavior (clamping, repeating, etc.)."
  (with-slots (before-behavior after-behavior) sequence
    (let* ((start-time (start-time sequence))
           (end-time (end-time sequence))
           (remainder (mod (- real-time start-time) (- end-time start-time))))

      (cond
        ((= start-time end-time)
         start-time)
        ;; In range
        ((and (>= real-time start-time) (<= real-time end-time))
         real-time)

        ;; Before and clamping
        ((and (<= real-time start-time) (eq before-behavior :clamp))
         start-time)

        ;; After and clamping
        ((and (>= real-time end-time) (eq after-behavior :clamp))
         end-time)

        ;; Repeating on the end
        ((and (= 0.0 remainder) (> real-time end-time) (eq after-behavior :repeat))
         end-time)

        ((and (> real-time end-time) (eq after-behavior :repeat))
         (+ remainder start-time))

        ;; Repeating on the beginning
        ((and (= 0.0 remainder) (< real-time start-time) (eq before-behavior :repeat))
         end-time)

        ((and (< real-time start-time) (eq before-behavior :repeat))
         (+ remainder start-time))

        (t ;; "This shouldn't happen"
         (error "Unhandled case in compute-repeating-time real-time ~a start-time ~
                 ~a end-time ~a before-behavior ~a after-behavior ~a"
                real-time start-time end-time before-behavior after-behavior))))))

;; Non-keyframed objects return their own value.
(defmethod value-at ((obj t) time)
  (declare (ignorable time))
  obj)

;; keyframe objects return their value
(defmethod value-at ((obj keyframe) time)
  (slot-value obj 'value))

(defmethod value-at ((sequence keyframe-sequence) time)
  (with-slots (frames before-behavior after-behavior) sequence
    (let ((last-idx (- (length frames) 1)))
      (cond
        ;; value-at of an empty sequence is an error
        ((= -1 last-idx)
         (error "Trying to get value-at of empty sequence.")
         nil)

        ;; Single value sequences are always the value of the first (and only) frame.
        ((= 0 last-idx)
         (keyframe-value (aref frames 0)))

        ;; With clamping, all values before start time are the
        ;; value of the first keyframe
        ((and (<= time (start-time sequence))
              (eq before-behavior :clamp) )
         (keyframe-value (aref frames 0)))

        ;; With clamping, all values after end time are the
        ;; value of the last keyframe
        ((and (>= time (end-time sequence))
              (eq before-behavior :clamp))
         (keyframe-value (aref frames last-idx)))

        (t
         ;; (format t "Computing using compute-canonical-time~%")
           (let* ((rep-time (compute-repeating-time sequence time))
                  (first-frame-idx (position rep-time
                                             frames :test #'>=
                                                    :key #'start-time
                                                    :from-end t
                                                    ))
                  (second-frame-idx (mod (1+ first-frame-idx) (length frames)))
                  (first-frame (aref frames first-frame-idx))
                  (second-frame (aref frames second-frame-idx)))
             (with-slots (interpolator) second-frame
               (funcall interpolator
                        (/ (- rep-time (start-time first-frame))
                           (- (start-time second-frame) (start-time first-frame)))
                        (keyframe-value first-frame)
                        (keyframe-value second-frame)
                        ))))))))

(defun create-keyframe (value time &key (interpolator #'lerp))
  "Convenience function for constructing a keyframe."
  (make-instance 'keyframe :value value :start-time time :interpolator interpolator))

(defun create-keyframe-sequence (frames &key (before :clamp) (after :clamp))
  "Convenience function for constructing a keyframe-sequence from frames."
  (declare (type sequence frames))
  (make-instance 'keyframe-sequence
                 :frames (make-array (length frames)
                                     :element-type 'keyframe
                                     :initial-contents frames
                                     :adjustable t)
                 :before before
                 :after after))

(defun create-simple-keyframe-sequence (points &key (interpolator #'lerp) (time-scale 1.0) (before :clamp) (after :clamp))
  "Convenience function for constructing a keyframe-sequence from values spaced time-scale time units apart."
  (declare (type sequence points))
  (make-instance 'keyframe-sequence
                 :frames (make-array (length points)
                                     :element-type 'keyframe
                                     :initial-contents (loop for pt in points
                                                             for time = 0.0 then (1+ time)
                                                             collecting
                                                             (make-instance 'keyframe
                                                                            :value pt
                                                                            :interpolator interpolator
                                                                            :start-time (* time-scale time)))
                                     :adjustable t)
                 :before before
                 :after after))
