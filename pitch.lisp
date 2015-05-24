(in-package #:post-tonal)

(export '(pitch-class int-value pitch-class-mod pitch-class-from-other
          defpitch-class pitch-class-transpose pitch-class-invert
          pitch-class-ord-interval pitch-class-unord-interval
          pitch-class-12 dyn-pitch-class pitch pitch-ord-interval
          pitch-unord-interval))

(defclass pitch-class ()
  ((int-value :initarg :int-value
              :reader int-value
              :initform 0
              :type fixnum)))

(defmethod initialize-instance :after ((pc pitch-class) &key)
  (setf (int-value pc)
        (int-value pc)))

(defgeneric pitch-class-mod (pitch-class))
(defgeneric pitch-class-from-other (pitch-class n))

(defun (setf int-value) (value pc)
  (setf (slot-value pc 'int-value)
        (mod value (pitch-class-mod pc))))

(defmacro defpitch-class (name modulus)
  `(progn
     (defclass ,name (pitch-class) ())
     (defmethod pitch-class-mod ((pc ,name)) ,modulus)
     (defmethod pitch-class-from-other ((pc ,name) n)
       (make-instance ',name :int-value n))))

(defun pitch-class-transpose (pitch-class n)
  (pitch-class-from-other pitch-class (+ (int-value pitch-class)
                                         n)))

(defun pitch-class-invert (pitch-class)
  (pitch-class-from-other pitch-class (- (pitch-class-mod pitch-class)
                                         (int-value pitch-class))))

(defun pitch-class-ord-interval (pc1 pc2)
  (mod (- (int-value pc2)
          (int-value pc1))
       (pitch-class-mod pc1)))

(defun pitch-class-unord-interval (pc1 pc2)
  (min (pitch-class-ord-interval pc1 pc2)
       (pitch-class-ord-interval pc2 pc1)))

(defpitch-class pitch-class-12 12)

(defclass dyn-pitch-class (pitch-class)
  ((modulus :initarg :modulus
            :initform 12
            :type fixnum)))

(defmethod pitch-class-mod ((pc dyn-pitch-class))
  (slot-value pc 'modulus))

(defmethod pitch-class-from-other ((pc dyn-pitch-class) n)
  (make-instance 'dyn-pitch-class :int-value n
                 :modulus (pitch-class-mod)))

(defclass pitch ()
  ((pitch-class :initarg :pitch-class
                :accessor pitch-class
                :type pitch-class)
   (pitch-octave :initarg :octave
                 :initform 4
                 :accessor pitch-octave
                 :type fixnum)))

(defmethod pitch-class-mod ((p pitch))
  (pitch-class-mod (pitch-class p)))

(defmethod pitch-class-from-other ((p pitch) n)
  (make-instance 'pitch :pitch-class (pitch-class-from-other (pitch-class p) n)
                 :octave (pitch-octave p)))

(defun pitch-transpose (p n)
  (let ((pc (pitch-class-transpose (pitch-class p) n))
        (oct-diff (truncate n (pitch-class-mod p))))
    (make-instance 'pitch :pitch-class pc
                   :octave (+ (pitch-octave p) oct-diff))))

(defun pitch-invert (p)
  (pitch-class-invert p))

(defun pitch-ord-interval (pc1 pc2)
  (+ (* (pitch-class-mod pc1)
        (- (pitch-octave pc2)
           (pitch-octave pc1)))
     (- (int-value pc2)
        (int-value pc1))))

(defun pitch-unord-interval (pc1 pc2)
  (abs (pitch-ord-interval pc1 pc2)))
