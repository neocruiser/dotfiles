;; Since Nov 6 2014. No issue with any code
;; Hit C-_ to set Master file in latex-mode
;; org-mode and knitr work with latex
;; ! Before compiling knitr, open R with M-x (important)
;; helm-bibtex works too

;; Keep track of loading time
(defconst emacs-start-time (current-time))
;; initalize all ELPA packages
(require 'package)

(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)
(setq package-enable-at-startup nil)
(let ((elapsed (float-time (time-subtract (current-time)
                                          emacs-start-time))))
  (message "Loaded packages in %.3fs" elapsed))

;; Dvorak nicety, regardless of loading settings
;(define-key key-translation-map "\C-t" "\C-x")
;; Load use-package, used for loading packages
(require 'use-package)

;; keep customize settings in their own file
(setq custom-file "~/.emacs.d/custom.el")
(when (file-exists-p custom-file)
  (load custom-file))

(load "~/.emacs.d/settings.el")
;(require 'org)
;(org-babel-load-file
; (expand-file-name "settings.org"
;                   user-emacs-directory))

;; Message how long it took to load everything (minus packages)
(let ((elapsed (float-time (time-subtract (current-time)
                                          emacs-start-time))))
  (message "Loading settings...done (%.3fs)" elapsed))



