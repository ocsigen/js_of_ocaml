;;; File: emacs-format-file
;;; Stan Warford
;;; 17 May 2006

(defun emacs-format-function ()
  "Format the whole buffer."
  (tuareg-mode)
  (indent-region (point-min) (point-max) nil)
  (delete-trailing-whitespace)
  (save-buffer)
  )

(defun emacs-format-function-ident ()
  "Format the whole buffer."
  (tuareg-mode)
  (indent-region (point-min) (point-max) nil)
  (save-buffer)
  )

(defun emacs-format-function-clean ()
  "Format the whole buffer."
  (delete-trailing-whitespace)
  (save-buffer)
  )
