(in-package :spcr)

(defstruct jdx
  "JCAMP-DX spctrum representation"
  name smiles cas molform state
  xunits yunits yfactor deltax firstx lastx npoints
  data)

(defun parse-value-string (string)
  "Extract field and its value from JDX string"
  (when (string= (subseq string 0 2) "##") ; if proper value string
    (let ((val (split-sequence #\= string)))
      (list (subseq (first val) 2) (second val))))) ; list field and its value

(defun read-jdx (file)
  (flet ((parse-value-string (string)
	   (when (string= (subseq string 0 2) "##") ; if proper value string
	     (let ((val (split-sequence #\= string)))
	       (list (subseq (first val) 2) (second val)))))) ; list field and its value
    (let ((jdx (make-jdx)))
      (with-open-file (s file)
	;; read fields till 'XYDATA', then stop and remember file position
	(file-position
	 s
	 (loop
	   (let ((str (read-line s))) ; for each parameter line
	     (if (and (>= (length str) 8) (string= (subseq str 0 8) "##XYDATA")) ; check if data section rreached
		 (return (file-position s)) ; return data section position
		 (let ((val (parse-value-string str)))
		   (when val ; when proper value string is proper (not null)
		     (let ((sec (second val)))
		       (case (intern (string (read-from-string (first val))) :keyword)
			 (:title (setf (jdx-name jdx) sec))
			 (:cas (setf (jdx-cas jdx) sec))
			 (:molform (setf (jdx-molform jdx) sec))
			 (:state (setf (jdx-state jdx) sec))
			 (:smiles (setf (jdx-smiles jdx) sec))

			 (:xunits (setf (jdx-xunits jdx) sec))
			 (:yunits (setf (jdx-yunits jdx) sec))
			 (:yfactor (setf (jdx-yfactor jdx) sec))
			 (:deltax (setf (jdx-deltax jdx) sec))
			 (:firstx (setf (jdx-firstx jdx) sec))
			 (:lastx (setf (jdx-lastx jdx) sec))
			 (:npoints (setf (jdx-npoints jdx) sec))
			 ))))))))
;			 ((:xunits :yunits :yfactor :deltax :firstx :lastx :npoints)
;			    (eval `(setf (,(read-from-string (concatenate 'string "jdx-" (first val)))
;					   ,jdx)
;					 ,sec)))))))))))
	;; some slots should be converted from string to number
	(dolist (slot '(deltax yfactor firstx lastx npoints))
	  (when (null (slot-value jdx slot)) ; some files miss some fields
	    (case  slot ; calculate them automatically
	      (deltax (setf (slot-value jdx slot)
			    (write-to-string
			     (/ (abs (- (read-from-string (jdx-lastx jdx))
					(read-from-string (jdx-firstx jdx))))
				(1- (read-from-string (jdx-npoints jdx)))))))
	      ;; may be add other slots
	      ))
	  ;; set number value in place of string
	  (setf (slot-value jdx slot) (read-from-string (slot-value jdx slot))))
	;; read spectral data from 'XYDATA' till 'END='
	(let ((data (list nil)))
	  (loop for str = (read-line s) while (string/= (subseq str 0 6) "##END=")
		;; ignore X-coords, take only Y-coord (cdr of each data line)
		do (nconc data (mapcar #'read-from-string (cdr (remove "" (split-sequence #\Space str) :test #'string=)))))
	  ;; (cdr data) is a small cheat to be able to use nconc.
	  ;; nconc can modify only non-null strings, so first value of 'data' is '(nil)
	  (setf (jdx-data jdx) (cdr data)))
	;; check if 'npoints' value is equal to 'data' length
	(assert (= (length (jdx-data jdx)) (jdx-npoints jdx))
		nil "Number of readen points is not equal to declared 'npoints' value")
	;; finally, return
	jdx))))

(defmethod parse-spectrum-format ((data jdx))
  (make-spectrum
   :name   (jdx-name data)
   :cas    (parse-cas (jdx-cas data))
   :smiles (or (jdx-smiles data) nil)
   :xmin   (jdx-firstx data)
   :xmax   (jdx-lastx data)
   :dx     (jdx-deltax data)
   :xunits (parse-xunits (jdx-xunits data))
   :yunits (parse-yunits (jdx-yunits data))
   :data   (map '(simple-array single-float (*)) #'(lambda (x) (float (* x (jdx-yfactor data)))) (jdx-data data))))
