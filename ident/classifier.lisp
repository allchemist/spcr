(in-package :spcr)

(defun make-simple-group-classifier (patterns test-part params cc-params)
  (make-cascade-classifier (pat-patterns patterns) test-part 'tanh-fn
			   (* 0.75 (/ (- (pat-max-out patterns) (pat-min-out patterns)) 2))
			   (param cc-params :hidden-num) params cc-params))						   

(defun make-preproc-group-classifier (patterns pcac test-part params cc-params)
  (change-class
   (make-simple-group-classifier (gr-encode pcac patterns) test-part params cc-params)
   'simple-preproc-classifier :codec pcac))

(defun make-group-classifier (patterns pcacs test-part params cc-params)
  (let (classifiers)
    (dolist (pcac pcacs)
      (info "~%")
      (info "Next codec~%")
      (push (make-preproc-group-classifier patterns pcac test-part (copy-tree params) (copy-tree cc-params))
	    classifiers))
    (make-instance 'boost-classifier
		   :output-ranges (list (pat-min-out patterns) (pat-max-out patterns))
		   :classifiers (nreverse classifiers))))

(defun %make-subtree-classifier (subtree data pcacs params cc-params idx-list)
  (cond ((null subtree) nil)
	((atom subtree)
	 (gc)
	 (let ((smarts (tree-smarts-idx subtree)))
	   (info "~%")
	   (make-group-classifier
	    (prog2
		(info "Preparing patterns for smarts ~A and ~A data samples.~%"
		      smarts (if idx-list (length idx-list) "all"))
		(make-group-patterns smarts data idx-list)
	      (info "Training classifier for smarts ~A.~%~%" smarts))
	    pcacs :auto params cc-params)))
	(t (cons (progn
		   (gc)
		   (let ((smarts (tree-smarts-idx (car subtree))))
		     (info "~%")
		     (make-group-classifier
		      (prog2
			  (info "Preparing patterns for smarts ~A and ~A data samples.~%"
				smarts (if idx-list (length idx-list) "all"))
			  (make-group-patterns smarts data idx-list)
			(info "Training classifier for smarts ~A.~%~%" smarts))
		      pcacs :auto params cc-params)))
		 (mapcar #'(lambda (x)
			     (%make-subtree-classifier x data pcacs params
						       (setf (param (copy-tree cc-params) :hidden-num)
							     (floor (* (param cc-params :hidden-num)
								       0.75)))
						       (reduce-indexes idx-list (tree-smarts-idx (car subtree)))))
			 (cdr subtree))))))

(defun %classify-group (classifier tree input)
  (cond ((null classifier) nil)
	((atom classifier)
	 (let ((val (aref (eval-network classifier input) 0)))
	   (vector (tree-smarts-idx tree)
		   val
		   (cond ((< (abs (- (second (boost-output-ranges classifier)) val))
			     (classifier-range classifier))
			  'true)
			 ((< (abs (- val (first (boost-output-ranges classifier))))
			     (classifier-range classifier))
			  'false)
			 (t 'unshure)))))
	(t (cons (%classify-group (car classifier) (car tree) input)
		 (mapcar #'(lambda (-c -t) (%classify-group -c -t input))
			 (cdr classifier) (cdr tree))))))

(defun %check-classify-group (result)
  (cond ((null result) nil)
	((atom result)
	 (when (eq (elt result 2) 'true)
	   (elt result 0)))
	(t (when (eq (elt (car result) 2) 'true)
	     (cons (%check-classify-group (car result))
		   (remove nil (mapcar #'%check-classify-group (cdr result))))))))

(defclass subtree-classifier ()
  ((classifiers :initarg :classifiers :accessor classifiers-tree)
   (smarts :initarg :smarts :accessor smarts-tree)))

(defun make-subtree-classifier (subtree data pcacs params cc-params)
  (make-instance 'subtree-classifier
		 :classifiers (%make-subtree-classifier subtree data pcacs params cc-params nil)
		 :smarts subtree))

(defun classify-subtree (classifier input)
  (%check-classify-group
   (%classify-group (classifiers-tree classifier) (smarts-tree classifier) input)))

(defclass tree-classifier ()
  ((classifiers :initarg :classifiers :accessor classifiers-list)))

(defun make-tree-classifier (&rest classifiers)
  (make-instance 'tree-classifier :classifiers classifiers))
