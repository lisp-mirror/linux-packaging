(uiop:define-package :linux-packaging/package
    (:use :cl)
  (:import-from :asdf
                #:system
                #:component-name
                #:perform
                #:system
                #:system-author
                #:system-description
                #:system-license
                #:system-version)
  (:import-from :asdf/system #:component-build-pathname)
  (:import-from :cffi
                #:close-foreign-library
                #:foreign-library-load-state
                #:foreign-library-name
                #:foreign-library-pathname
                #:foreign-library-type
                #:list-foreign-libraries)
  (:import-from :cffi-toolchain #:static-program-op)
  (:import-from :uiop
                #:getenv
                #:register-image-dump-hook
                #:run-program)
  (:import-from :uiop #:run-program)
  (:import-from :cl-ppcre #:split)
  (:export #:linux-package
           #:system-dependencies
           #:package-type
           #:path->package
           #:build-op))

(in-package :linux-packaging/package)

(defmacro d (system &rest args)
  `(when (verbose ,system)
     (funcall #'format t ,@args)))

(defun cat (&rest args)
  (apply #'concatenate 'string args))

(defun last-elt (sequence)
  (subseq sequence (1- (length sequence))))

(defun right-pad (char haystack)
  (check-type char string)
  (check-type haystack string)
  (cat haystack
       (unless (string= char (last-elt haystack))
         char)))

(defun additional-file->argument (additional-file)
  (format nil "~a=~a"
          (let ((src (first additional-file)))
            (if (pathnamep src)
                (namestring src)
                src))
          (let ((dst (rest additional-file)))
            (if (pathnamep dst)
                (namestring dst)
                (right-pad "/" dst)))))

(defclass linux-package (system)
  ((package-name :initarg :package-name :initform nil :reader pkg-name)
   (ignored-libraries :initarg :ignored-libraries :initform nil :reader ignored-libraries)
   (additional-files :initarg :additional-files :initform nil :reader additional-files)
   (additional-dependencies :initarg :additional-dependencies :initform nil :reader additional-dependencies)
   (verbose :initarg :verbose :initform nil :reader verbose)
   (package-type :reader package-type)))

(defmethod make-instance :after ((s linux-package) &key &allow-other-keys)
  (setf (slot-value s 'ignored-libraries)
        (mapcar #'string-downcase (ignored-libraries s)))

  ;;; Make sure we close statically linked libraries.
  ;;; Remove when this or similar is done in cffi: https://github.com/cffi/cffi/pull/163
  (register-image-dump-hook
   (lambda ()
     (loop for library in (list-foreign-libraries)
        when (eq (foreign-library-type library) :grovel-wrapper)
        do (close-foreign-library library)))))

(defgeneric system-dependencies (linux-package)
  (:documentation "Returns the dependencies that every Lisp image relies on."))

(defgeneric path->package (linux-package path)
  (:documentation "Returns the package a file belongs to."))

(defmethod find-dependencies ((system linux-package))
  (let ((libraries-to-paths (ldconfig)))
    (remove-duplicates
     (append
      (additional-dependencies system)
      (let ((system-deps (system-dependencies system)))
	(d system "System dependencies: ~a~%" system-deps)
	system-deps)
      (reduce (lambda (packages library)
                (append
                 packages
                 (unless (or (is-ignored system library)
                             (eq (foreign-library-type library) :grovel-wrapper))
                   (list
		    (let* ((path (or (gethash (namestring (foreign-library-pathname library))
					      libraries-to-paths)
				     (error "Unable to find the library for ~a"
					    (foreign-library-name library))))
			   (package (path->package system path)))
		      (d system "Package for ~a: ~a~%" path package)
		      (or package
			  (error "Unable to find a package for ~a"
				 (foreign-library-name library))))))))
              (list-foreign-libraries)
              :initial-value nil)))))

(defclass build-op (static-program-op) ())

(defmethod perform ((o build-op) (s system))
  (call-next-method o s)

  (let* ((deps (find-dependencies s))
         (command (delete nil
                          `("fpm" "-s" "dir"
                                  "-t" ,(package-type s)
                                  ,(let ((maintainer (system-author s)))
                                     (when maintainer (cat "--maintainer=" maintainer)))
                                  ,(let ((license (system-license s)))
                                     (when license (cat "--license=" license)))
                                  ,(let ((description (system-description s)))
                                     (when description (cat "--description=" description)))
                                  ,@(mapcar (lambda (dep)
                                              (cat "--depends=" dep))
                                            deps)
                                  "-n" ,(or (pkg-name s) (component-name s))
                                  "-v" ,(or (system-version s) (getenv "VERSION") "1.0.0")
                                  ,(format nil "~a=/usr/bin/" (component-build-pathname s))
                                  ,@(mapcar #'additional-file->argument
                                            (additional-files s))))))
    (d s "Running command: ~a~%" command)
    (run-program command :output :interactive :error-output :interactive)))

(defun ldconfig ()
  (let ((libraries->paths (make-hash-table :test #'equal)))
    (with-input-from-string (s (run-program '("ldconfig" "-p") :output '(:string)))
      ;; first line is pointless
      (read-line s)

      (loop
         (let ((line (read-line s nil 'eof)))
           (when (eq line 'eof)
             (return-from ldconfig libraries->paths))

           (let ((parts (split " " (subseq line 1))))
             (setf (gethash (first parts) libraries->paths)
                   (first (last parts)))))))))

(defun is-ignored (system library)
  (member (string-downcase (symbol-name (foreign-library-name library)))
          (ignored-libraries system)
          :test #'string=))
