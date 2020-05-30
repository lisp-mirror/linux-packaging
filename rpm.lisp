(in-package :linux-packaging)

(defun path->rpm-package (path)
  (handler-case
      (run-program `("rpm" "-q" "--whatprovides" ,path "--qf" "%{NAME}") :output '(:string))
    (subprocess-error ()
      nil)))

(defun find-rpm-dependencies (op)
  (let ((libraries-to-paths (ldconfig)))
    (remove-duplicates
     (append
      #+sbcl '("glibc" "zlib")
      (reduce (lambda (packages library)
		(append
		 packages
		 (unless (or (is-ignored op library)
			     (eq (foreign-library-type library) :grovel-wrapper))
		   (or
		    (path->rpm-package
		     (or (gethash (foreign-library-pathname library) libraries-to-paths)
			 (error "Unable to find a package for ~a"
				(foreign-library-name library))))
		    (error "Unable to find a package for ~a"
			   (foreign-library-name library))))))
	      (list-foreign-libraries)
	      :initial-value nil)))))

(defclass rpm (linux-package) ())

(defmethod perform ((o rpm) (s system))
  (call-next-method o s)

  (let ((deps (find-rpm-dependencies o)))
    (run-program `("fpm" "-s" "dir"
                         "-t" "rpm"
                         "-n" ,(or (name o) (component-name s))
                         "-v" ,(or (version o) (getenv "VERSION") "1.0.0")
                         ,@(let ((maintainer (system-author s)))
                             (if maintainer (cat "--maintainer=" maintainer) ""))
                         ,@(let ((license (system-license s)))
                             (if license (cat "--license=" license) ""))
                         ,@(mapcar (lambda (dep)
                                     (cat "--depends=" dep))
                                   deps)
                         (format nil "~a=/usr/bin/" (component-build-pathname s))
                         ,@(mapcar #'additional-file->argument (additional-files o))))))

(setf (find-class 'asdf::rpm) (find-class 'rpm))
