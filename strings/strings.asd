(defpackage :com.bwestbro.strings-system
  (:use :asdf :cl))

(in-package :com.bwestbro.strings-system)

(defsystem "strings"
  :description "a string library"
  :version "0.1"
  :author "Brent R. Westbrook <brentrwestbrook@gmail.com>"
  :components ((:file "strings")))
