(uiop:define-package :linux-packaging-tests/t/main
    (:use :cl)
  (:import-from :sqlite #:connect)
  (:import-from :uiop #:*temporary-directory*)
  (:export #:main))

(in-package :linux-packaging-tests/t/main)

(defun main ()
  (let ((db (merge-pathnames #p"test.db" *temporary-directory*)))
    ;; this goes through the shared library
    (connect db)))
