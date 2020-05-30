(uiop:define-package :linux-packaging
    (:use :cl)
  (:import-from :asdf
                #:component-name
                #:perform
                #:system
                #:system-author
                #:system-license)
  (:import-from :asdf/system #:component-build-pathname)
  (:import-from :cffi
                #:close-foreign-library
		#:foreign-library-name
                #:foreign-library-pathname
                #:foreign-library-type
                #:list-foreign-libraries)
  (:import-from :cffi-toolchain #:static-program-op)
  (:import-from :ppcre #:split)
  (:import-from :uiop
                #:getenv
                #:register-image-dump-hook
                #:run-program
		#:subprocess-error))
