(uiop:define-package :linux-packaging/rpm
    (:use :cl :linux-packaging/package)
  (:import-from :uiop #:run-program)
  (:export #:rpm))

(in-package :linux-packaging/rpm)

(defclass rpm (linux-package)
  ((package-type :initform "rpm")))

(defmethod system-dependencies ((s rpm))
  #+sbcl '("glibc" "zlib"))

(defmethod path->package ((s rpm) path)
  (run-program `("rpm" "-q" "--whatprovides" ,path "--qf" "%{NAME}") :output '(:string)))
