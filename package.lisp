(uiop:define-package :linux-packaging/package
    (:use :cl)
  (:import-from :asdf
		#:system
                #:component-name
                #:perform
                #:system
                #:system-author
                #:system-license)
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
          (first additional-file)
          (right-pad "/" (rest additional-file))))

(defclass linux-package (system)
  ((package-name :initarg :package-name :initform nil :reader pkg-name)
   (package-version :initarg :package-version :initform nil :reader version)
   (ignored-libraries :initarg :ignored-libraries :initform nil :reader ignored-libraries)
   (additional-files :initarg :additional-files :initform nil :reader additional-files)
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
      (system-dependencies system)
      (reduce (lambda (packages library)
		(append
		 packages
		 (unless (or (is-ignored system library)
			     (eq (foreign-library-type library) :grovel-wrapper))
		   (or
		    (list
		     (path->package
		      system
		      (or (gethash (namestring (foreign-library-pathname library))
				   libraries-to-paths)
			  (error "Unable to find the library for ~a"
				 (foreign-library-name library)))))
		    (error "Unable to find a package for ~a"
			   (foreign-library-name library))))))
	      (list-foreign-libraries)
	      :initial-value nil)))))

(defclass build-op (static-program-op) ())

(defmethod perform ((o build-op) (s system))
  (call-next-method o s)

  (let ((deps (find-dependencies s)))
    (run-program
     (delete nil
	     `("fpm" "-s" "dir"
		     "-t" ,(package-type s)
		     ,(let ((maintainer (system-author s)))
			(when maintainer (cat "--maintainer=" maintainer)))
		     ,(let ((license (system-license s)))
			(when license (cat "--license=" license)))
		     ,@(mapcar (lambda (dep)
				 (cat "--depends=" dep))
			       deps)
		     "-n" ,(or (pkg-name s) (component-name s))
		     "-v" ,(or (version s) (getenv "VERSION") "1.0.0")
		     ,(format nil "~a=/usr/bin/" (component-build-pathname s))
		     ,@(mapcar #'additional-file->argument (additional-files s))))
     :output :interactive
     :error-output :interactive)))

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
