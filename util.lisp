(in-package :linux-packaging)

(defun cat (&rest args)
  (apply #'concatenate 'string args))

(defun right-pad (char haystack)
  (check-type char string)
  (check-type haystack string)
  (cat haystack
       (unless (string= char (subseq haystack (1- (length haystack))))
	 char)))

(defun additional-file->argument (additional-file)
  (format nil "~a=~a"
          (first additional-file)
          (right-pad "/" (rest additional-file))))

(defclass linux-package (static-program-op)
  ((name :initarg :name :initform nil :reader name)
   (version :initarg :version :initform nil :reader version)
   (ignored-libraries :initarg :ignored-libraries :initform nil :reader ignored-libraries)
   (additional-files :initarg :additional-files :initform nil :reader additional-files)))

(defmethod make-instance :after ((op linux-package) &allow-other-keys)
  (setf (slot-value op 'ignored-libraries)
	(mapcar #'string-downcase (ignored-libraries op))))

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

(defun is-ignored (op library)
  (member (string-downcase (symbol-name (foreign-library-name library)))
	  (ignored-libraries op)
	  :test #'string=))
