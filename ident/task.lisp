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
  (princ ".")
  (defparameter *sp-data* (restore "/data/progr/spcr/data/sp-preproc-data.dat"))
  (princ ".")
  (defparameter *pcacs* (restore "/data/progr/spcr/data/pcacs.dat"))
  (princ ".")
  (defparameter *orig-pcac* (restore "/data/progr/spcr/data/orig-pcac.dat"))
  (princ ".")
  (defparameter *preproc* (restore "/data/progr/spcr/data/preproc.dat")))


(defun build-OH-subtree-classifier (data pca-codecs)
  (make-subtree-classifier *oh-groups-tree* data pca-codecs
			   `((:eps . 0.35) (:mu . 2.0) (:epochs . 500)
			     (:recompute . 15) (:thr . 1.e-5) (:verbosity . 2) (:test-set-mix-ratio . 4)
			     (:classify-range . 0.75) (:max-err-bits . 0.1))
			   `((:eps . 1.0) (:mu . 2.0) (:epochs . 500) (:epochs-handicap . 45) (:test-set-mix-ratio . 2)
			     (:recompute . 20) (:thr . 1.e-4) (:verbosity . 0)
			     (:candidates . 4) (:hidden-num . 15))))

(defun build-subtree-classifier (subtree data pca-codecs)
  (make-subtree-classifier subtree data pca-codecs
			   `((:eps . 0.35) (:mu . 2.0) (:epochs . 500)
			     (:recompute . 15) (:thr . 1.e-5) (:verbosity . 2) (:test-set-mix-ratio . 10)
			     (:classify-range . 0.75) (:max-err-bits . 0.05))
			   `((:eps . 1.0) (:mu . 2.0) (:epochs . 500) (:epochs-handicap . 45) (:test-set-mix-ratio . 5)
			     (:recompute . 20) (:thr . 1.e-4) (:verbosity . 0)
			     (:candidates . 3) (:hidden-num . 10))))

(defun adapt-gmdb-spectrum (path preproc)
  (encode preproc (adapt-to-pattern (parse-spectrum-format (read-gmdb (strings "/data/botva/diplom/db/from_gm/data/" path))))))

(defun display-spectrum-transforms (spectrum preproc orig-pcac pcacs)
  (let ((sp (parse-spectrum-format spectrum)))
    (when (/= (dim0 (sp-data sp)) 880)
      (setf (sp-data sp) (adapt-to-pattern sp)))
    (plot-spectrum sp
		   :out "/tmp/spec-orig.png")
    (sleep 2)
    (plot-spectrum sp
		   :codec (make-chain-codec preproc orig-pcac)
		   :subname "autoscaled, encoded"
		   :out "/tmp/spec-enc.png")
    (sleep 2)
    (plot-spectrum sp
		   :codec (make-chain-codec preproc (first pcacs))
		   :op :diff
		   :subname "difference from 300 components"
		   :out "/tmp/spec-diff300.png")
    (sleep 2)
    (plot-spectrum sp
		   :codec (make-chain-codec preproc (first pcacs))
		   :op :decode
		   :subname "decoded from 300 components" 
		   :out "/tmp/spec-dec300.png")
    (sleep 2)
    (plot-spectrum sp
		   :codec (make-chain-codec preproc (second pcacs))
		   :op :diff
		   :subname "difference from 250 components"
		   :out "/tmp/spec-diff250.png")
    (sleep 2)
    (plot-spectrum sp
		   :codec (make-chain-codec preproc (second pcacs))
		   :op :decode
		   :subname "decoded from 250 components"
		   :out "/tmp/spec-dec250.png")
    (sleep 2)
    (plot-spectrum sp
		   :codec (make-chain-codec preproc (third pcacs))
		   :op :diff
		   :subname "difference from 200 components"
		   :out "/tmp/spec-diff200.png")
    (sleep 2)
    (plot-spectrum sp
		   :codec (make-chain-codec preproc (third pcacs))
		   :op :decode
		   :subname "decode from 200 components"
		   :out "/tmp/spec-dec200.png")))
