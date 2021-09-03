(defpackage :com.bwestbro.stats-system
  (:use :asdf :cl))

(in-package :com.bwestbro.stats-system)

(defsystem "stats"
  :description "statistics library for interactive data exploration"
  :version "0.1"
  :author "Brent R. Westbrook <brentrwestbrook@gmail.com>"
  :components ((:file "stats")))
