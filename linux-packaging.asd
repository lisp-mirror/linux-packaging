(defsystem "linux-packaging"
  :author "Florian Margaine <florian@margaine.com>"
  :description "ASDF extension to generate linux packages."
  :license "MIT"
  :defsystem-depends-on ("wild-package-inferred-system")
  :class "winfer:wild-package-inferred-system"
  :depends-on ("linux-packaging/*"))
