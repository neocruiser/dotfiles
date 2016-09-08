;; Since Nov 6 2014. No issue with any code
;; Hit C-_ to set Master file in latex-mode
;; org-mode and knitr work with latex
;; ! Before compiling knitr, open R with M-x (important)
;; helm-bibtex works too

;; initalize all ELPA packages
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)

;; Dvorak nicety, regardless of loading settings
;(define-key key-translation-map "\C-t" "\C-x")
;; Load use-package, used for loading packages
(require 'use-package)

;; keep customize settings in their own file
(setq custom-file "~/.emacs.d/custom.el")
(when (file-exists-p custom-file)
  (load custom-file))

;(load "~/.emacs.d/settings.el")
;(require 'org)
(org-babel-load-file "~/.emacs.d/settings.org")

;; helm and async from github
(add-to-list 'load-path "~/.emacs.d/async")

(add-to-list 'load-path "~/.emacs.d/helm")
(require 'helm-config)

;; ESS
(add-to-list 'load-path "~/.emacs.d/ess/lisp/")
(load "ess-site")

;; MAgit for git
(add-to-list 'load-path "~/.emacs.d/magit/lisp")
(require 'magit)

(with-eval-after-load 'info
  (info-initialize)
  (add-to-list 'Info-directory-list
	       "~/.emacs.d/magit/Documentation/"))

;; Powerline
(add-to-list 'load-path "~/.emacs.d/powerline")
(require 'powerline)
(require 'cl)
(setq powerline-arrow-shape 'arrow14)
(custom-set-faces
 '(mode-line ((t (:foreground "#030303" :background "#bdbdbd" :box nil))))
 '(mode-line-inactive ((t (:foreground "#f9f9f9" :background "#666666" :box nil)))))
