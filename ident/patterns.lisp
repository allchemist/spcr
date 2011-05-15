(in-package :spcr)

(defstruct (group-patterns
	     (:conc-name pat-)
	     (:constructor %make-group-patterns))
  patterns smarts min-out max-out)

(defun gr-encode (codec group-patterns)
  (%make-group-patterns :patterns (encode codec (pat-patterns group-patterns))
			:smarts (pat-smarts group-patterns)
			:min-out (pat-min-out group-patterns)
			:max-out (pat-max-out group-patterns)))

(defun gr-decode (codec group-patterns)
  (%make-group-patterns :patterns (decode codec (pat-patterns group-patterns))
			:smarts (pat-smarts group-patterns)
			:min-out (pat-min-out group-patterns)
			:max-out (pat-max-out group-patterns)))

#|
(defun make-group-patterns (smarts data &optional
			    (smiles-map (cl-store:restore
					 "/data/progr/spcr/data/sp-smiles-map.dat")))
  (let* ((patterns (make-patterns smarts smiles-map data))
	 (outputs (map 'list #'(lambda (p) (aref (second p) 0)) patterns)))
    (%make-group-patterns
     :patterns patterns
     :smarts smarts
     :min-out (apply #'min outputs)
     :max-out (apply #'max outputs))))|#

(defun reduce-indexes (indexes smarts &optional
			     (smiles-map (cl-store:restore
					  "/data/progr/spcr/data/sp-smiles-map.dat")))
	(let ((grep (obgrep-string smarts (mapcar #'second smiles-map))))
	  (remove-if #'(lambda (x)
			 (or (and (not (null indexes))
				  (not (member x indexes)))
			     (not (elt grep x))))
		     (loop for i from 0 below 5228 collect i))))

(defun inputs-with-indexes (inputs indexes)
  (coerce (loop for i in indexes collect (svref inputs i)) 'simple-vector))

(defun make-group-patterns (smarts data &optional indexes
			    (smiles-map (cl-store:restore
					 "/data/progr/spcr/data/sp-smiles-map.dat")))
  (let* ((patterns (make-patterns smarts
				  (if indexes
				      (remove-if #'(lambda (x) (not (member (car x) indexes)))
						 smiles-map)
				      smiles-map)
				  (if indexes
				      (inputs-with-indexes data indexes)
				      data)))
	 (outputs (map 'list #'(lambda (p) (aref (second p) 0)) patterns)))
    (%make-group-patterns
     :patterns patterns
     :smarts smarts
     :min-out (apply #'min outputs)
     :max-out (apply #'max outputs))))

;;;

(defun adapt-to-pattern (spectrum)
  (let* ((data (coerce-matrix (sp-data spectrum) 'double-float))
	 (spl (make-csplines (coerce-matrix (linspace (sp-xmin spectrum) (sp-xmax spectrum) (dim0 data))
					    'double-float)
			     data)))
    (map '(simple-array single-float (*)) 
	 #'(lambda (x) (eval-splines x spl))
	 (linspace 450.0 3966.0 880))))

(defun plot-pattern (pattern &rest keys &key &allow-other-keys)
  (let ((arr (make-matrix (list (dim0 pattern) 2))))
    (setf (col arr 0) (linspace 450.0 3966.0 (dim0 pattern)))
    (setf (col arr 1) pattern)
    (apply #'make-plot arr :draw-with 'lines keys)))
