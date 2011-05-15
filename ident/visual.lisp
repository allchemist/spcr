(in-package :spcr)

(defun parse-graph (tree)
  (let (nodes edges)
    (labels ((%parse-graph (subtree prev)
	       (cond ((null subtree) nil)
		     ((atom subtree)
		      (push subtree nodes)
		      (push (list prev subtree) edges))
		     (t (progn
			  (push (car subtree) nodes)
			  (push (list prev (car subtree)) edges)
			  (mapcar #'(lambda (s) (%parse-graph s (car subtree)))
				  (cdr subtree)))))))
      (%parse-graph tree nil))
    (values (nreverse nodes) (nreverse edges))))

(defun make-dot (tree)
  (let ((font `((s-dot::fontsize "9") (s-dot::fontname "Courier") (s-dot::shape "rectangle"))))
    (multiple-value-bind (nodes edges)
	(parse-graph tree)
      `(s-dot::graph ((s-dot::rankdir "TB"))
		     ,@(loop for n in nodes collect
			     `(s-dot::node ((s-dot::id ,(write-to-string n))
					    (s-dot::label ,(tree-smarts-idx n))
					    ,@font)))
		     ,@(loop for e in edges collect
			     `(s-dot::edge ((s-dot::from ,(write-to-string (first e)))
					    (s-dot::to ,(write-to-string (second e))))))))))

(defun display-graph (tree)
  (s-dot:render-s-dot "/tmp/tmp.png" "png"
		      (make-dot tree))
  (asdf:run-shell-command "sh -c \"feh -Z /tmp/tmp.png&\"")) 

(defun x-offset (name)
  (+ 20 (* (length name) 8)))

(defun y-offset (num)
  (* num 30))

(defun display-classify-group (result frame y-offset)
  (let (objs)
    (labels ((%display (result x-pos y-pos)
	       (cond ((null result) nil)
		     ((atom result) (print result)
		      (let ((decision (elt result 2))
			    (name (elt result 0)))
			(when (or (eq decision 'true) (eq decision 'unsure))
			  (push (ltk:place (make-instance 'ltk:label
							  :master frame
							  :text name
							  :borderwidth 3 :relief :sunken
							  :background (case decision (true "red") (unsure "white")))
					   y-pos x-pos :width (x-offset name) :height 25)
				objs))))
		     (t (progn (%display (car result)
					 (+ x-pos 50);(x-offset (elt (car result) 0)))
					 (+ y-pos (y-offset 1)))
			       (when (eq (elt (car result) 2) 'true)
				 (dotimes (i (length (cdr result)))
				   (let ((r (elt (cdr result) i)))
				     (unless r
				       (%display (cdr result)
						 (+ x-pos 50);(x-offset (elt r 0)))
						 (+ y-pos (y-offset i))))))))))))
      (%display result y-offset 0))))
	     

(defun display-classify-results (classifier frame entry)
  (let ((path (ltk:text entry))
	sp
	results)
    (unless (zerop (length path))
      (setf sp (adapt-to-pattern (read-spectrum path)))
      (setf results
	    (mapcar #'(lambda (c) (%classify-group (classifiers-tree c) (smarts-tree c) sp))
		    (classifiers-list classifier)))
      (dotimes (i (length results))
	(let ((r (elt results i)))
	  (display-classify-group r frame (* i 50)))))))
      


(defun display-classifier (classifier)
  (let* ((s *standard-output*))
    (sb-thread:make-thread
     (lambda ()
       (let ((*standard-output* s))
	 (ltk:with-ltk ()
	   (let* ((f (make-instance 'ltk:frame :width 500 :height 500))
		  (control (make-instance 'ltk:frame
					  :master f
					  :borderwidth 1
					  :relief :ridge))
		  (path (make-instance 'ltk:entry
				       :master control
				       :text "/data/botva/diplom/result-db/1.jdx"
				       :width 50
				       :borderwidth 3
				       :relief :sunken))
		  (start (make-instance 'ltk:button
					:text "Analyse"
					:master control
					:command (lambda () (display-classify-results classifier f path)))))
	     (ltk:pack f)
	     (ltk:place control 0 0 :width 500 :height 40)
	     (ltk:pack path :side :left)
	     (ltk:pack start :side :right)
	     )))))))
