(in-package :spcr)

(defstruct gmdb
  name molform resolution
  x-data y-data)

(defun read-gmdb (file)
  (flet ((parse-value-string (string)
	   (let ((val (split-sequence #\: string)))
	     (when (string= (subseq string 0 1) "$")
	       (list (subseq (first val) 1) (second val))))))
    (let ((gmdb (make-gmdb)))
      (with-open-file (s file)
	(file-position
	 s
	 (loop
	   (let ((str (read-line s)))
	     (if (and (>= (length str) 5) (string= (subseq str 0 5) "$DATA"))
		 (return (file-position s))
		 (let ((val (parse-value-string str)))
		   (when val
		     (let ((sec (second val)))
		       (case (read-from-string (first val))
			 (title (setf (gmdb-name gmdb) sec))
			 (formula (setf (gmdb-molform gmdb) sec))
			 (resolution (setf (gmdb-resolution gmdb) (parse-integer sec)))))))))))
	(let (x-data y-data)
	  (loop for str = (read-line s) while (char/= (char str 0) #\$) do
	    (let ((delim (position #\; str)))
	      (push (read-from-string str nil nil :end delim) x-data)
	      (push (read-from-string str nil nil :start (1+ delim)) y-data)))
	  (setf (gmdb-x-data gmdb) (make-matrix (length x-data) :initial-contents (nreverse x-data)))
	  (setf (gmdb-y-data gmdb) (make-matrix (length y-data) :initial-contents (nreverse y-data))))
	gmdb))))

(defmethod parse-spectrum-format ((data gmdb))
  (let ((x-data (gmdb-x-data data)))
    (make-spectrum
     :name (gmdb-name data)
     :xmin (mmin x-data)
     :xmax (mmax x-data)
     :dx   (/ (- (mmax x-data) (mmin x-data)) (1- (dim0 x-data)))
     :xunits '1/CM
     :yunits 'absorabnce
     :data (m+c (transmittance-to-absorbance (gmdb-y-data data)) 2.0))))
