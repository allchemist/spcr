(in-package :spcr)

(defun strings (&rest things)
  "Concatenate things as one string"
  (apply #'concatenate 'string
	 (mapcar #'(lambda (x)
		     (cond ((stringp x) x)
			   ((symbolp x) (string x))
			   (t (write-to-string x))))
		 things)))

(defun vector-to-file (vector path)
  (with-open-file (s path
		     :direction :output
		     :if-exists :supersede
		     :if-does-not-exist :create)
    (dotimes (i (length vector))
      (princ (elt vector i) s) (princ " " s)))
  path)

(defun vector-from-file (path &optional (vec-type '(simple-array single-float (*))))
  (with-open-file (s path)
    (coerce
     (loop for elem = (read s nil nil) while elem collect elem)
     vec-type)))

(defun cas->smiles-list (lst)
(let ((*standard-output* (make-string-output-stream)))
  (pct::id->smiles-list
   (pct::cas->id-list
    lst))))

(defun gc () (sb-ext:gc :full t))
