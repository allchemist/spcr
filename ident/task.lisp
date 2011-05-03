(in-package :spcr)

(defun build-pcacs (pca-components-list &key (data-path "/data/progr/spcr/data/"))
  (let ((start (get-internal-run-time)) orig-data preproc preproc-data orig-pcac pcacs)
    (flet ((relative-path (name)
	     (strings data-path name))
	   (time-stamp ()
	     (prog1
		 (info "~A seconds passed~%"
		       (float (/ (- (get-internal-run-time) start) internal-time-units-per-second)))
	       (setf start (get-internal-run-time)))))
      (info "1: Loading original vectors~%")
      (setf orig-data (restore (relative-path "sp-source-data.dat")))
      (time-stamp) (gc)
      (info "2: Creating centering codec and storing to disc~%")
      (setf preproc (make-centering-codec orig-data))
      (store preproc (relative-path "preproc.dat"))
      (time-stamp) (gc)
      (info "3: Encoding data and storing to disc~%")
      (setf preproc-data (encode preproc (copy-patterns orig-data)))
      (store preproc-data (relative-path "sp-preproc-data.dat"))
      (setf orig-data nil)
      (time-stamp) (gc)
      (info "4: Creating source pca codec and storing to disc~%")
      (setf orig-pcac (make-pca-codec preproc-data '((:method . :svd))))
      (gc) (store orig-pcac (relative-path "orig-pcac.dat"))
      (time-stamp) (gc)
      (info "5: Creating pca codecs for ~A components and storing to disc~%" pca-components-list)
      (setf pcacs (mapcar #'(lambda (dim) (reduce-pca-codec orig-pcac dim)) pca-components-list))
      (gc) (store pcacs (relative-path "pcacs.dat"))
      (time-stamp) (gc)
      pcacs)))

(defun load-data ()
  (defparameter *sp-data* (restore "/data/progr/spcr/data/sp-preproc-data.dat"))
  (defparameter *pcacs* (restore "/data/progr/spcr/data/pcacs.dat")))


(defun build-OH-subtree-classifier (data pca-codecs)
  (make-subtree-classifier *oh-groups-tree* data pca-codecs
			   `((:eps . 0.35) (:mu . 2.0) (:epochs . 500)
			     (:recompute . 15) (:thr . 1.e-5) (:verbosity . 2) (:test-set-mix-ratio . 4)
			     (:classify-range . 0.75) (:max-err-bits . 0.1))
			   `((:eps . 1.0) (:mu . 2.0) (:epochs . 500) (:epochs-handicap . 45) (:test-set-mix-ratio . 2)
			     (:recompute . 20) (:thr . 1.e-4) (:verbosity . 0)
			     (:candidates . 4) (:hidden-num . 15))))

(defun adapt-gmdb-spectrum (path)
  (encode *preproc* (adapt-to-pattern (parse-spectrum-format (read-gmdb (strings "/data/botva/diplom/db/from_gm/data/" path))))))
