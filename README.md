# keyframe
### _Jeremiah LaRocco <jeremiah_larocco@fastmail.com>_


# Overview
`keyframe` is a Common Lisp package for interpolating values between keyframes.

# Example Usage

```common-lisp
(ql:quickload :keyframe)
(defparameter *kf* (kf:create-keyframe-sequence (list (kf:create-keyframe 0.0 0.0)
                                                      (kf:create-keyframe 100.0 1.0)
                                                      (kf:create-keyframe 120.0 3.0))
                                                :before :clamp
                                                :after :clamp))
(kf:value-at *kf* -1.0) ;; 0.0
(kf:value-at *kf* 0.0)  ;; 0.0
(kf:value-at *kf* 0.5)  ;; 50.0
(kf:value-at *kf* 1.0)  ;; 100.0
(kf:value-at *kf* 1.5)  ;; 105.0
(kf:value-at *kf* 1.75) ;; 107.5
(kf:value-at *kf* 2.0)  ;; 110.0
(kf:value-at *kf* 2.5)  ;; 115.0
(kf:value-at *kf* 3.0)  ;; 120.0
```

# Overview

A `keyframe-sequence` is a sequence of values that are interpolated between a number of `keyframes`, each of which indicates a specific values at a specific times.  In between keyframes are interpolated using a interpolation function (`alexandria:lerp` by default).

For example, in the `keyframe-sequence` below, the value of the sequence ranges between 0.0 at time 0.0 and 100.0 at time 1.0.  By default, time values outside of the range `(kf:start-time kf)` and `(kf:end-time kf)` are clamped to the value at `start-time` and `end-time`, respectively.

```common-lisp
(ql:quickload :keyframe)
(let ((kf (kf:create-keyframe-sequence (list (kf:create-keyframe 0.0 0.0)
                                       (kf:create-keyframe 100.0 1.0)))))
    (kf:value-at kf 0.0) ;; 0.0
    (kf:value-at kf 0.5) ;; 50.0
    (kf:value-at kf 1.0) ;; 100.0
```

# Background
I originally wrote most of this as part of the [newgl](https://github.com/jl2/newgl/) OpenGL library, but realized it could be useful on its own.

As part of `newgl`, the biggest use case was for simple camera animation, in the [`keyframe viewer`](https://github.com/jl2/newgl/blob/master/keyframe-viewer.lisp), where camera positions are interpolated between a list of points.
This is a project to do keyframe interpolation.

# License

ISC


Copyright (c) 2021 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>


