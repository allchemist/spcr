(in-package :spcr)

(defstruct (spec-db
	     (:conc-name db-))
  xmin xmax dx xunits yunits path
  name-col cas-col smiles-col)

(defun db-search (db field value)
  (gethash value
	   (ecase field
	     (:cas (db-cas-col db))
	     (:smiles (db-smiles-col db))
	     (:name (db-name-col db)))))

(defun (setf db-search) (new db field value)
  (setf (gethash value
		 (ecase field
		   (:cas (db-cas-col db))
		   (:smiles (db-smiles-col db))
		   (:name (db-name-col db))))
	new))
	
(defun db-get (db field value)
  (vector-from-file (strings (db-path db) (db-search db field value) ".dat")))

(defun cas->smiles (cas)
  (let ((*standard-output* (make-string-output-stream))
	(cid (pct::cas->id-list `(,cas))))
    (when cid (cdar (pct::ids->param (first cid))))))

(defun db-create (dir source &key (if-data-exists T))
  (let ((db (make-spec-db
	     :name-col   (make-hash-table :test 'equal)
	     :cas-col    (make-hash-table :test 'equal)
	     :smiles-col (make-hash-table :test 'equal)))
	(count 0))
    (do-spec-dir (sp source)
      (loop for slot in '(xmin xmax dx xunits yunits) do
	(if (not (slot-value db slot))
	    (setf (slot-value db slot)
		  (slot-value sp slot))
	    (assert (equal (slot-value db slot) (slot-value sp slot))
		    nil "Irregular parameter: ~A at spectrum of ~A" slot (slot-value sp 'name))))
      (setf (gethash (slot-value sp 'cas) (db-cas-col db))       count)
      (setf (gethash (slot-value sp 'smiles) (db-smiles-col db)) count)
      (setf (gethash (slot-value sp 'name) (db-name-col db))     count)
      (unless if-data-exists (vector-to-file (slot-value sp 'data) (strings dir count ".dat")))
      (incf count)
      (when (zerop (mod count 10)) (princ ".")))
    (setf (slot-value db 'path) dir)
    db))

(defun simple-get-elem (id map)
  (let ((elem (elt map id)))
    (unless (= id (second elem))
      (setf elem (find id map :key #'second)))
    elem))

(defun simple-get-cas (id map)
  (third (simple-get-elem id map)))

(defun simple-get-data (id map &optional (dir "/data/progr/spcr/test-db/"))
  (vector-from-file (strings dir id ".dat")))

(defun make-patterns-from-scratch (smarts smiles-map &optional (range '(-1 1)))
  (let (patterns (pos 0) (neg 0))
    (dotimes (id (length smiles-map))
      (let ((elem (elt smiles-map id)))
	(push (list (simple-get-data (first elem) smiles-map)
		    (make-array 1 :element-type 'single-float
				:initial-element
				(coerce
				 (if (obgrep-string smarts (second elem))
				     (progn (incf pos) (second range))
				     (progn (incf neg) (first range)))
				 'single-float)))
	      patterns))
      (when (zerop (mod id 10)) (princ ".")))
    (dolist (p patterns)
      (let ((val (aref (second p) 0)))
	(setf (aref (second p) 0)
	      (/ (aref (second p) 0)
		 (if (= val (first range)) (/ neg pos) 1)))))
    (nreverse patterns)))

(defun make-patterns (smarts smiles-map data &optional (range '(-1 1)))
  (let ((patterns (make-list (length smiles-map)))
	(grep (obgrep-string smarts (mapcar #'second smiles-map))))
    (dotimes (id (length patterns))
      (setf (elt patterns id)
	    (list (get-pattern data id)
		  (make-array 1 :element-type 'single-float
			      :initial-element
			      (coerce (if (elt grep id)
					  (second range)
					  (first range))
				      'single-float)))))
    (dolist (p patterns)
      (let ((val (aref (second p) 0)))
	(setf (aref (second p) 0)
	      (/ (aref (second p) 0)
		 (if (= val (first range)) (/ (count nil grep) (count nil grep :test-not #'eql)) 1)))))
    (coerce patterns 'simple-vector)))
