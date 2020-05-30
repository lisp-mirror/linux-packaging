(in-package :linux-packaging)

;;; Make sure we close statically linked libraries.
;;; Remove when this or similar is done in cffi: https://github.com/cffi/cffi/pull/163
(register-image-dump-hook
 (lambda ()
   (loop for library in (list-foreign-libraries)
      when (eq (foreign-library-type library) :grovel-wrapper)
      do (close-foreign-library library))))
