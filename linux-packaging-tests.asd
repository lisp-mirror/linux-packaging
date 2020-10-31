(defsystem "linux-packaging-tests"
  :description "A minimal system to test the packaging."
  :defsystem-depends-on ("wild-package-inferred-system")
  :class "winfer:wild-package-inferred-system"
  :depends-on ("linux-packaging-tests/t/*"))

(defsystem "linux-packaging-tests/rpm"
  :defsystem-depends-on ("linux-packaging")
  :class "linux-packaging:rpm"
  :depends-on ("linux-packaging-tests")
  :build-operation "linux-packaging:build-op"
  :package-name "random-name"
  :verbose t
  :version #.(format nil "1.2.3")
  :additional-dependencies ("emacs")
  :build-pathname "other-random-name"
  :entry-point "linux-packaging-tests/t/main:main")

(defsystem "linux-packaging-tests/deb"
  :defsystem-depends-on ("linux-packaging")
  :class "linux-packaging:deb"
  :depends-on ("linux-packaging-tests")
  :build-operation "linux-packaging:build-op"
  :package-name "random-name"
  :verbose t
  :version #.(format nil "1.2.3")
  :additional-dependencies ("emacs")
  :build-pathname "other-random-name"
  :entry-point "linux-packaging-tests/t/main:main")

(defsystem "linux-packaging-tests/pacman"
  :defsystem-depends-on ("linux-packaging")
  :class "linux-packaging:pacman"
  :depends-on ("linux-packaging-tests")
  :build-operation "linux-packaging:build-op"
  :package-name "random-name"
  :verbose t
  :version #.(format nil "1.2.3")
  :additional-dependencies ("emacs")
  :build-pathname "other-random-name"
  :entry-point "linux-packaging-tests/t/main:main")
