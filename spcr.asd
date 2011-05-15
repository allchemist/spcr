(defpackage :spcr-system
    (:use :cl :asdf))

(in-package :spcr-system)

(defsystem spcr
  :description "Chemical compounds identifier through IR spectra"
  :author "Khokhlov Ivan"
  :licence "BSD"
  :depends-on (split-sequence gplt cl-openbabel cl-pubchem cl-store sb-math annil s-dot ltk)
  :serial t
  :components
  ((:file "package")
   (:module system
	    :serial t
	    :components
	    ((:file "utils")))
   (:module formats
	    :serial t
	    :components
	    ((:file "internal")
	     (:file "jdx")
	     (:file "gmdb")))
   (:module database
	    :serial t
	    :components
	    ((:file "db-engine")))
   (:module ident
	    :serial t
	    :components
	    ((:file "groups-table")
	     (:file "patterns")
	     (:file "classifier")
	     (:file "task")
	     (:file "visual")))))
