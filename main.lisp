(uiop:define-package :linux-packaging/main
    (:use :linux-packaging/package
	  :linux-packaging/rpm
	  :linux-packaging/deb
	  :linux-packaging/pacman)
  (:nicknames :linux-packaging)
  (:export #:build-op #:rpm #:deb #:pacman))
