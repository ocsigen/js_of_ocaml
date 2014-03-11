;;; File: emacs-format-file
;;; Stan Warford
;;; 17 May 2006

(setq js-indent-level 2)
(custom-set-variables
 '(indent-tabs-mode nil)
 '(tab-width 2))

(defun emacs-format-js ()
  "Format the whole buffer."
  (js-mode)
  (indent-region (point-min) (point-max) nil)
  (delete-trailing-whitespace)
  (save-buffer)
  )

(defun emacs-format-js-ident ()
  "Format the whole buffer."
  (js-mode)
  (indent-region (point-min) (point-max) nil)
  (save-buffer)
  )

(defun emacs-format-js-clean ()
  "Format the whole buffer."
  (js-mode)
  (delete-trailing-whitespace)
  (save-buffer)
)
