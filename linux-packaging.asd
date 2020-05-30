(defsystem "linux-packaging"
  :author "Florian Margaine <florian@margaine.com>"
  :description "ASDF extension to generate linux packages."
  :license "MIT"
  :depends-on ("asdf"
               "cffi"
               "cffi-toolchain"
	       "cl-ppcre"
               "uiop")
  :serial t
  :components ((:file "package")
               (:file "util")
               (:file "rpm")
               (:file "main")))
