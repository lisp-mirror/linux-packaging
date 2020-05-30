(in-package :linux-packaging)

(defclass deb (static-program-op) ())
(setf (find-class 'asdf::deb) (find-class 'deb))
