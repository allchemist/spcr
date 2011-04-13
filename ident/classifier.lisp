(in-package :spcr)

(defun make-simple-group-classifier (patterns test-part params cc-params)
  (make-cascor-classifier (pat-patterns patterns) test-part 'tanh-fn
			  (* 0.8 (/ (- (pat-max-out patterns) (pat-min-out patterns)) 2))
			  (param cc-params :hidden-num) params cc-params))

(defun make-preproc-group-classifier (patterns pcac test-part params cc-params)
  (change-class
   (make-simple-group-classifier (gr-encode pcac patterns) test-part params cc-params)
   'simple-preproc-classifier :codec pcac))

(defun make-group-classifier (patterns pcacs test-part params cc-params)
  (let (classifiers)
    (dolist (pcac pcacs)
      (push (make-preproc-group-classifier patterns pcacs test-part params cc-params)
	    classifiers))
    (make-instance 'boost-classifier
		   :output-ranges (list (pat-min-out patterns) (pat-max-out patterns))
		   :classifiers (nreverse classifiers))))

(defun make-subtree-classifier (subtree data pcacs params cc-params)
  (cond ((null subtree) nil)
	((atom subtree)
	 (gc)
	 (let ((smarts (tree-smarts-idx subtree)))
	   (make-group-classifier
	    (prog2
		(info "Building patterns for SMARTS: ~A~%" smarts)
		(progn (gc) (make-group-patterns smarts data))
	      (info "Training classifier for SMARTS: ~A~%" smarts))
	    pcacs nil params cc-params)))
	(t (cons (make-subtree-classifier (car subtree) data pcacs params cc-params)
		 (mapcar #'(lambda (x) (progn (gc) (make-subtree-classifier x data pcacs params cc-params)))
			 (cdr subtree))))))

 `((:eps . 0.2) (:mu . 2.0) (:epochs . 200)
   (:restarts . 2) (:recompute . 10) (:thr . 1.e-5) (:verbosity . 4))
 `((:method . :rprop) (:d0 . 0.01) (:dmin . 1.e-7) (:dmax . 2.0) (:epochs . 100)
   (:hidden-num . 15) (:candidates . 4) (:restarts . 1) (:recompute . 20) (:thr . 1.e-5) (:verbosity . 6))
