(uiop:define-package :linux-packaging/deb
    (:use :cl :linux-packaging/package)
  (:import-from :uiop #:run-program)
  (:import-from :cl-ppcre #:split))

(in-package :linux-packaging/deb)

(defclass deb (linux-package)
  ((package-type :initform "deb")))

(defmethod system-dependencies ((o deb))
  #+sbcl '("libc6" "zlib1g"))

(defmethod path->package ((o deb) path)
  ;; output is: "<package>:amd64: <file>"
  (first (split
	  ":"
	  (run-program `("dpkg" "-S" ,path)
		       :output '(:string :stripped t)))))

(setf (find-class 'asdf::deb) (find-class 'deb))
