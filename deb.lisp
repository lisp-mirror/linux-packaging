(uiop:define-package :linux-packaging/deb
    (:use :cl :linux-packaging/package)
  (:import-from :uiop #:run-program)
  (:import-from :cl-ppcre #:split)
  (:import-from :sb-posix #:s-islnk #:stat-mode #:lstat #:readlink)
  (:export #:deb))

(in-package :linux-packaging/deb)

(defclass deb (linux-package)
  ((package-type :initform "deb")))

(defmethod system-dependencies ((s deb))
  #+sbcl '("libc6" "zlib1g"))

(defun symlinkp (pathname)
  (s-islnk (stat-mode (lstat pathname))))

(defmethod path->package ((s deb) path)
  ;; output is: "<package>:amd64: <file>"
  (first (split
	  ":"
	  (run-program `("dpkg" "-S" ,(if (symlinkp path) (namestring (merge-pathnames (readlink path) path)) path))
		       :output '(:string :stripped t)))))
