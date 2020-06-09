(uiop:define-package :linux-packaging/pacman
    (:use :cl :linux-packaging/package)
  (:import-from :uiop #:run-program)
  (:import-from :cl-ppcre #:split)
  (:export #:pacman))

(in-package :linux-packaging/pacman)

(defclass pacman (linux-package)
  ((package-type :initform "pacman")))

(defmethod system-dependencies ((s pacman))
  #+sbcl '("glibc" "zlib"))

(defmethod path->package ((s pacman) path)
  ;; output is: "<file> is owned by <package> <version>"
  (fifth (split " " (run-program `("pacman" "-Qo" ,path) :output '(:string :stripped t)))))
