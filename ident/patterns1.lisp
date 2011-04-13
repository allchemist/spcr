(in-package :spcr)
#|
(defstruct (group-patterns
	     (:conc-name pat-)
	     (:constructor init-group-patterns))
  patterns smarts min-out max-out)

(defun make-group-patterns (smarts data &optional
			    (smiles-map (cl-store:restore
					 "/data/progr/spcr/data/sp-smiles-map.dat")))
  (let* ((patterns (make-patterns smarts smiles-map data))
	 (outputs (mapcar #'(lambda (p) (aref (second p) 0)) patterns)))
    (init-group-patterns
     :patterns patterns
     :smarts smarts
     :min-out (apply #'min outputs)
     :max-out (apply #'max outputs))))

(defun make-full-group-patterns (smarts &key
				 (data (list *sp-data*))
				 (smiles-map (cl-store:restore
					      "/data/progr/spcr/data/sp-smiles-map.dat")))
  (mapcar #'(lambda (d)
	      (progn
		(info "~%") (info "CREATING NEW PATTERNS SET~%") (info "~%")
		(make-group-patterns smarts d smiles-map)))
	  data))
			  
(defun make-plot-data (pattern)
  (let ((arr (make-matrix (list (length pattern) 2))))
    (setf (col arr 0)
	  (linspace 450 3966 (length pattern)))
    (setf (col arr 1) pattern)
    arr))


|#

