(in-package :spcr)

(defun make-simple-subtree-classifier (subtree params cc-params)
  (cond ((null subtree) nil)
	((atom subtree)
	 (make-simple-group-classifier (make-group-patterns (tree-smiles-idx subtree) *sp-data*)
				       nil params cc-params))
	(t (cons (make-simple-subtree-classifier (car subtree) params cc-params)
		 (mapcar #'(lambda (x) (make-simple-subtree-classifier x params cc-params)) (cdr subtree))))))
