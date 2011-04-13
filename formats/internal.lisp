(in-package :spcr)

(defstruct (spectrum (:conc-name sp-))
  name cas smiles
  xmin xmax dx xunits yunits
  data)

;; xunits: 1/cm or micrometers => 1/cm
;; yunits: transmittance or absorbance => transmittance
;; relative transmittance: max peak has unit intensity

(defun absorbance-to-transmittance (data)
  (map-matrix data #'(lambda (x) (float (expt 10 (- x))))))

(defun transmittance-to-absorbance (data)
  (map-matrix data #'(lambda (x) (- (log x 10)))))

(defun transmittance-to-relative-transmittance (data)
  (let ((amp (- 1.0 (mmin data))))
    (map-matrix data #'(lambda (x) (- 1.0 (/ (- 1.0 x) amp))))))

(defun set-relative-transmittance (sp)
  (setf (sp-data sp) (transmittance-to-relative-transmittance (sp-data sp)))
  sp)

;; wavelength unit conversion

(defun convert-wavelength (wl) (/ 10000 wl))

;; convert spectra

(defgeneric parse-spectrum-format (spectrum))

(defun parse-cas (cas)
  (if (find #\- cas)
      cas
      (let ((rev-cas (reverse cas)))
	(nreverse
	 (concatenate 'string
		      (string (char rev-cas 0)) "-"
		      (subseq rev-cas 1 3) "-"
		      (subseq rev-cas 3))))))

(defun parse-xunits (xunits) (intern (string-upcase xunits) :spcr))
(defun parse-yunits (yunits) (intern (string-upcase yunits) :spcr))

(defun plot-spectrum (spectrum)
  (let* ((data (sp-data spectrum))
	 (arr (make-matrix (list (dim0 data) 2))))
    (setf (col arr 0) (linspace (sp-xmin spectrum) (sp-xmax spectrum) (dim0 data)))
    (setf (col arr 1) data)
    (make-plot arr :title (sp-name spectrum) :draw-with 'lines)))
