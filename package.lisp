(defpackage :spcr
    (:use :cl :cl-openbabel :cl-pubchem :gplt :split-sequence :cl-store :sb-math :annil :ltk))

(in-package :spcr)

(defparameter *root-path*
  (let ((path (namestring (asdf:component-relative-pathname (asdf:find-system :spcr)))))
    (if (search ".asd" path)
	(subseq path 0 (1+ (position #\/ path :from-end t)))
	path)))
