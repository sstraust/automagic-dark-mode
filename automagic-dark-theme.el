(defvar automagic-dark--invert-face-cookies nil)

(defun automagic-dark--color-invert-face (face inversion-function &optional background-p)
  "Invert the luminance (L) of FACE’s effective color.
If BACKGROUND-P is non‑nil, operate on :background instead of :foreground.
Returns the remap cookie."
  (let* ((attr     (if background-p :background :foreground))
         (orig-col (face-attribute face attr nil t))
	 (new-col (apply inversion-function (list orig-col))))
    (face-remap-add-relative face (list attr new-col))))

(defun  automagic-dark--invert-all-faces (inversion-function &optional background-inversion-function)
  (let ((background-inversion-function (or background-inversion-function inversion-function)))
    (setq automagic-dark--invert-face-cookies nil)
    (dolist (f (face-list))
      (progn 
	(let* ((orig (face-attribute f :foreground nil t)))
	  (when (and orig (not (eq orig 'unspecified)))
            (let ((cookie (automagic-dark--color-invert-face f inversion-function)))
              (push cookie automagic-dark--invert-face-cookies))))
	(let* ((orig (face-attribute f :background nil t)))
	  (when (and orig (not (eq orig 'unspecified)))
            (let ((cookie (automagic-dark--color-invert-face f background-inversion-function :background)))
              (push cookie automagic-dark--invert-face-cookies))))))))
    
(defun automagic-dark--remove-all-invert-remaps ()
  "Undo all face‑luminance‑invert remappings in this buffer."
  (interactive)
  (dolist (cookie my/invert-face-cookies)
    (face-remap-remove-relative cookie))
  (setq my/invert-face-cookies nil))




(defun automagic-dark-invert-color-cielab (color)
  "Invert COLOR, which can be a name or hex-string, via CIE L*a*b*."
  (let* (;; 1. From hex string to normalized RGB floats [0,1]
         (rgb      (color-name-to-rgb color))
         (r        (nth 0 rgb))
         (g        (nth 1 rgb))
         (b        (nth 2 rgb))
         ;; 2. Convert to CIELAB
         (lab      (apply #'color-srgb-to-lab rgb))
         (L        (nth 0 lab))
         (a        (nth 1 lab))
         (bb       (nth 2 lab))
         ;; 3. Invert the lightness channel
         (L-new    (- 100 L))
         ;; 4. Back to XYZ then RGB
         (xyz-new  (apply #'color-lab-to-xyz (list L-new a bb)))
         (rgb-new  (apply #'color-xyz-to-srgb xyz-new)))
    ;; 5. Format back to hex string
    (apply #'color-rgb-to-hex rgb-new)))

(defun automagic-dark-pure-invert (color)
  "Invert COLOR, which can be a name or hex string."
  (let* ((rgb (color-values color)) ;; returns (R G B) with 0..65535 values
         (r (car rgb))
         (g (cadr rgb))
         (b (caddr rgb)))
    (color-rgb-to-hex
     (/ (- 65535 r) 65535.0)
     (/ (- 65535 g) 65535.0)
     (/ (- 65535 b) 65535.0))))

(defun automagic-dark-luminance-invert (color)
  "Invert the lightness of COLOR while preserving hue and saturation."
  (let* ((rgb (color-name-to-rgb color)) ;; RGB 0..1 floats
         (hsl (apply #'color-rgb-to-hsl rgb))
         (h (nth 0 hsl))
         (s (nth 1 hsl))
         (l (nth 2 hsl))
         (inv-l (- 1.0 l))
         (new-rgb (color-hsl-to-rgb h s inv-l)))
    (apply #'color-rgb-to-hex new-rgb)))

(defun automagic-dark-add-luminance (color inc)
  "Invert the lightness of COLOR while preserving hue and saturation."
  (let* ((rgb (color-name-to-rgb color)) ;; RGB 0..1 floats
         (hsl (apply #'color-rgb-to-hsl rgb))
         (h (nth 0 hsl))
         (s (nth 1 hsl))
         (l (nth 2 hsl))
         (new-l (+ l inc))
         (new-rgb (color-hsl-to-rgb h s new-l)))
    (apply #'color-rgb-to-hex new-rgb)))


(defun automagic-dark-scaled-luminance-invert (color)
  "Invert the lightness of COLOR while preserving hue and saturation."
  (let* ((rgb (color-name-to-rgb color)) ;; RGB 0..1 floats
         (hsl (apply #'color-rgb-to-hsl rgb))
         (h (nth 0 hsl))
         (s (nth 1 hsl))
         (l (nth 2 hsl))
	 (inv-l (- 1 (expt l 0.8)))
         (new-rgb (color-hsl-to-rgb h s inv-l)))
    (apply #'color-rgb-to-hex new-rgb)))


(defun automagic-dark--get-luminance (color)
  "Invert the lightness of COLOR while preserving hue and saturation."
  (let* ((rgb (color-name-to-rgb color)) ;; RGB 0..1 floats
         (hsl (apply #'color-rgb-to-hsl rgb))
         (l (nth 2 hsl)))
    l))

(defun automagic-dark--wcag-luminance (color)
  "Compute the relative luminance of COLOR, a string like \"#rrggbb\"."
  (let* ((rgb (color-name-to-rgb color))
         (linear (mapcar (lambda (c)
                           (if (<= c 0.03928)
                               (/ c 12.92)
                             (expt (/ (+ c 0.055) 1.055) 2.4)))
                         rgb)))
    (+ (* 0.2126 (nth 0 linear))
       (* 0.7152 (nth 1 linear))
       (* 0.0722 (nth 2 linear)))))


(defun automagic-dark--wcag-contrast-ratio (color1 color2)
  "Compute the WCAG contrast ratio between COLOR1 and COLOR2."
  (let* ((l1 (automagic-dark--wcag-luminance color1))
         (l2 (automagic-dark--wcag-luminance color2))
         (lighter (max l1 l2))
         (darker (min l1 l2)))
    (/ (+ lighter 0.05) (+ darker 0.05))))


(defun automagic-dark-invert-luminance-with-wcag-contrast (fg)
  (let* ((old-bg (face-attribute 'default :background))
	 (new-bg (scaled-luminance-invert (face-attribute 'default :background)))
	 (new-fg (luminance-invert fg))
	 (old-ratio (wcag-contrast-ratio fg old-bg))
	 (step (if (> (wcag-luminance old-bg)
		     (wcag-luminance new-bg))
		  0.05
		 -0.05)))
    (let ((curr-fg new-fg))
      (while (and (> (wcag-contrast-ratio curr-fg new-bg) (* old-ratio 0.9))
		  (> (- (get-luminance curr-fg) step) 0)
		  (< (- (get-luminance curr-fg) step) 1))
	(setq curr-fg (add-luminance curr-fg (- 0 step))))
      (while (and (< (wcag-contrast-ratio curr-fg new-bg) (/ old-ratio 3))
		  (> (+ (get-luminance curr-fg) step) 0)
		  (< (+ (get-luminance curr-fg) step) 1))
	(setq curr-fg (add-luminance curr-fg step)))
      (message "new ratio")
      (message (number-to-string (wcag-contrast-ratio curr-fg new-bg)))
      curr-fg)))


(defcustom automagic-dark-color-inverter #'automagic-dark-invert-luminance-with-wcag-contrast
  "Choose the tool to use invert a color."
  :type '(choice
          (const :tag "wcag-contrast" automagic-dark-invert-luminance-with-wcag-contrast)
          (const :tag "pure invert" automagic-dark-pure-invert)
          (const :tag "luminance invert" automagic-dark-luminance-invert)
          (const :tag "cielab invert" automagic-dark-invert-color-cielab))
  :group 'my-custom-group)


(define-minor-mode automagic-dark-mode
  "A simple minor mode that runs code on enable/disable."
  :lighter "Dark"
  :global 't
  (if automagic-dark-mode
      (invert-all-faces automagic-dark-color-inverter)
    (remove-all-invert-remaps)))
