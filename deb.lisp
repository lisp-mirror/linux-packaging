(uiop:define-package :linux-packaging/deb
    (:use :cl :linux-packaging/package)
  (:import-from :uiop #:run-program #:truenamize)
  (:import-from :cl-ppcre #:split)
  (:export #:deb))

(in-package :linux-packaging/deb)

(defclass deb (linux-package)
  ((package-type :initform "deb")))

(defmethod system-dependencies ((s deb))
  #+sbcl '("libc6" "zlib1g"))

(defmethod path->package ((s deb) path)
  ;; output is: "<package>:amd64: <file>"
  (first (split
	  ":"
	  (run-program `("dpkg" "-S" ,(namestring (truenamize path))
		       :output '(:string :stripped t)))))
