(uiop:define-package :linux-packaging/rpm
    (:use :cl :linux-packaging/package)
  (:import-from :uiop #:run-program))

(in-package :linux-packaging/rpm)

(defclass rpm (linux-package)
  ((package-type :initform "rpm")))

(defmethod system-dependencies ((o rpm))
  #+sbcl '("glibc" "zlib"))

(defmethod path->package ((o rpm) path)
  (run-program `("rpm" "-q" "--whatprovides" ,path "--qf" "%{NAME}") :output '(:string)))

(setf (find-class 'asdf::rpm) (find-class 'rpm))
