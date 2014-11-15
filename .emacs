;; Since Nov 6 2014. No issue with any code
;; Hit C-_ to set Master file in latex-mode
;; org-mode and knitr work with latex
;; ! Before compiling knitr, open R with M-x (important)
;; helm-bibtex works too

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-delay 0.7)
 '(blink-cursor-mode nil)
 '(custom-safe-themes (quote ("756597b162f1be60a12dbd52bab71d40d6a2845a3e3c2584c6573ee9c332a66e" "6a37be365d1d95fad2f4d185e51928c789ef7a4ccf17e7ca13ad63a8bf5b922f" default)))
 '(global-aggressive-indent-mode t)
 '(helm-adaptive-history-length 250)
 '(helm-bibtex-format-citation-functions (quote ((org-mode . helm-bibtex-format-citation-org-link-to-PDF) (latex-mode . helm-bibtex-format-citation-cite) (markdown-mode . helm-bibtex-format-citation-pandoc-citeproc) (default . helm-bibtex-format-citation-default))))
 '(helm-mode t)
 '(inhibit-startup-echo-area-message nil)
 '(inhibit-startup-screen t)
 '(markdown-command "/home/neocruiser/Markdown_1.0.1/Markdown.pl")
 '(menu-bar-mode nil)
 '(save-place t nil (saveplace))
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(uniquify-buffer-name-style (quote post-forward) nil (uniquify)))

 
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Inconsolata" :foundry "unknown" :slant normal :weight normal :height 109 :width normal))))
 '(cursor ((t (:background "tan" :foreground "black"))))
 '(flyspell-duplicate ((t (:foreground "Gold3"))))
 '(flyspell-incorrect ((t (:foreground "OrangeRed"))))
 '(sml/filename ((t (:inherit sml/global :foreground "pale goldenrod" :weight bold))))
 '(sml/global ((t (:foreground "#93a1a1"))))
 '(sml/modes ((t (:inherit sml/global))))
 '(sml/position-percentage ((t (:weight normal))))
 '(sml/prefix ((t (:inherit sml/global :foreground "peru"))))
 '(sml/read-only ((t (:inherit sml/global :foreground "medium aquamarine"))))
 '(sml/vc-edited ((t (:inherit sml/modified :foreground "light salmon"))))
 '(sml/warning ((t (:inherit sml/global :foreground "light coral" :weight bold))) t)
 '(writegood-duplicates-face ((t (:inherit font-lock-warning-face :foreground "dark orange"))))
 '(writegood-passive-voice-face ((t (:inherit font-lock-warning-face :foreground "dark turquoise"))))
 '(writegood-weasels-face ((t (:inherit font-lock-warning-face :foreground "magenta")))))
 

;;;; Desktop Session Managment
;==============================
;; This code is very usefull
;; It wont work though with *Rnw files
;; It is thus better to close all knitr files before session-save
;; sessions can be manually saved before exiting emacs
;; If problems occure with PID being already loaded
;; delete all .emacs.desktop saves in ~/.emacs.d/

(desktop-save-mode -1)			;; Save state of the desktop
(setq history-length 250)
    (add-to-list 'desktop-globals-to-save 'file-name-history)
;; use only one desktop
(setq desktop-path '("~/.emacs.d"))
(setq desktop-dirname "~/.emacs.d")
(setq desktop-base-file-name ".emacs.desktop")
;; use session-save to save the desktop manually
(defun saved-session ()
  (file-exists-p (concat desktop-dirname "/" desktop-base-file-name)))
(defun session-save ()
  "Save an emacs session."
  (interactive)
  (if (saved-session)
      (if (y-or-n-p "Overwrite existing desktop? ")
	  (desktop-save-in-desktop-dir)
	(message "Session not saved."))
  (desktop-save-in-desktop-dir)))
;; ask user whether to restore desktop at start-up 'Mx session-save'
(add-hook 'after-init-hook
	  '(lambda ()
	     (if (saved-session)
		 (if (y-or-n-p "Restore desktop? ")
		     (session-restore)))))
;; use session-restore to restore the desktop manually 'Mx session-restore'
(defun session-restore ()
  "Restore a saved emacs session."
  (interactive)
  (if (saved-session)
      (desktop-read)
    (message "No desktop found.")))

;;;; LOAD MELPA
;==============
(when (>= emacs-major-version 24)	;; Load MELPA packages
(require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
  )
	
;;;; Custom themes
;==================
(add-to-list 'custom-theme-load-path (expand-file-name "~/.emacs.d/themes/"))
(load-theme 'zenburn t) 
;(load-theme 'monokai t) 
;(load-theme 'gotham t)
;(load-theme 'leuven t) ;; best for org-mode
;(load-theme 'spacegray t)
;(load-theme 'molokai t)
;(load-theme 'gruvbox t)

;;;; Miscelanious
;=================
(setq debug-on-error t) ;; debug-on-error
(setq make-backup-files nil) ;; Get rid of backup files
(fset `yes-or-no-p `y-or-n-p) ;; change yes to y and no to n
(iswitchb-mode -1)  ; Inactivate iswitch to use HELM Cx-b and Cc-m 
(setq-default transient-mark-mode t) ; highligh the marked region
(set-face-attribute 'region nil :background "#666")
(setq-default visible-bell t) ; flash the sreen dont beep
(require 'saveplace) ; save cursor position for every buffer
(setq save-place-file "~/.emacs.d/saved-places")
(require 'uniquify) ; change title buffer
(require 'ansi-color) ; translates ANSI SGR into emacs faces
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)


;;;; Window resize in linux
;============================
(global-set-key (kbd "M-s-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "M-s-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "M-s-<down>") 'shrink-window)
(global-set-key (kbd "M-s-<up>") 'enlarge-window)


;;;; Move line up or down (global)
;================================
(defun move-line (n)
  "Move the current line up or down by N lines."
  (interactive "p")
  (setq col (current-column))
  (beginning-of-line) (setq start (point))
  (end-of-line) (forward-char) (setq end (point))
  (let ((line-text (delete-and-extract-region start end)))
    (forward-line n)
    (insert line-text)
    ;; restore point to original column in moved line
    (forward-line -1)
    (forward-char col)))

(defun move-line-up (n)
  "Move the current line up by N lines."
  (interactive "p")
  (move-line (if (null n) -1 (- n))))

(defun move-line-down (n)
  "Move the current line down by N lines."
  (interactive "p")
  (move-line (if (null n) 1 n)))

 
;;;; Replace under the selection
;==============================
(pending-delete-mode t)

;;;; GOD MODE (remove the need to hold Ctrl or Meta keys)
;=========================================================
(require 'god-mode)
(define-key god-local-mode-map (kbd "z") 'repeat)
(define-key god-local-mode-map (kbd "i") 'god-local-mode)
(add-to-list 'god-exempt-major-modes 'dired-mode)

; change cursor style between God-mode and regular
(defun my-update-cursor ()
  (setq cursor-type (if (or god-local-mode buffer-read-only)
                        'bar
                      'box)))

(add-hook 'god-mode-enabled-hook 'my-update-cursor)
(add-hook 'god-mode-disabled-hook 'my-update-cursor)
(blink-cursor-mode -1)

;;;; undo tree
;==============
(require 'undo-tree)
(global-undo-tree-mode)

;;;; Autoindentation
;==================
;; Autoindentation plugins are numerous
;; either electric indent
;; or aggressive indent with add-hooks
;; Both need further parametrization
;; issues with the comment section


(setq auto-indent-key-for-end-of-line-then-newline "<M-return>")
(setq auto-indent-key-for-end-of-line-insert-char-then-newline "<M-S-return>")
(setq auto-indent-indent-style 'aggressive)
(require 'auto-indent-mode)


;;;; ace jump mode
;==================
(autoload
  'ace-jump-mode
  "ace-jump-mode"
  "Emacs quick move minor mode"
  t)
;; you can select the key you prefer to
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

;; enable a more powerful jump back function from ace jump mode
;;
(autoload
  'ace-jump-mode-pop-mark
  "ace-jump-mode"
  "Ace jump back:-)"
  t)
(eval-after-load "ace-jump-mode"
  '(ace-jump-mode-enable-mark-sync))
(define-key global-map (kbd "C-x SPC") 'ace-jump-mode-pop-mark)

;;;; Powerline
;=========================
  (require 'powerline)
   (powerline-center-evil-theme)
  
;;;; Smart mode line
;=======================
(require 'smart-mode-line)
   (sml/setup)
    (sml/apply-theme 'dark)
;(sml/apply-theme 'light)
;(sml/apply-theme 'respectful)
;(sml/apply-theme 'automatic)

(add-to-list 'sml/replacer-regexp-list '("^~/.emacs.d/configs/" ":ED:") t)
(add-to-list 'sml/replacer-regexp-list '("^/media/Data/Bibliography" ":bib:") t)
(add-to-list 'sml/replacer-regexp-list '("^/media/Data/Dropbox/Latex" ":LaTeX:") t)
(add-to-list 'sml/replacer-regexp-list '("^/media/Data/Dropbox/R" ":R:") t)
(add-to-list 'sml/replacer-regexp-list '("^/media/Data/Dropbox/Private/org" ":org:") t)
(add-to-list 'sml/replacer-regexp-list '("^/media/Data/Dropbox" ":dropbox:") t)

;;;; Samartparens
;==============================
(smartparens-global-mode t)

;; different colors for parenthesis highlights
(require 'highlight-parentheses) 
(setq hl-paren-colors '("gold" "IndianRed" "cyan" "green" "orange" 
"magenta")) 
(defun hpm-on () 
  (highlight-parentheses-mode t)) 
(add-hook 'ess-mode-hook 'hpm-on) 
(add-hook 'inferior-ess-mode-hook 'hpm-on) 
(add-hook 'latex-mode-hook 'hpm-on) 

;;;; WriteGood
;================
(require 'writegood-mode)
;(writegood-mode 1)
(global-set-key "\C-cs" 'writegood-mode)
(global-set-key "\C-c\C-gg" 'writegood-grade-level)
(global-set-key "\C-c\C-ge" 'writegood-reading-ease)

;;;; Style-check
;================ 
; source http://www.cs.umd.edu/~nspring/software/style-check-readme.html
(defun my-action/style-check-file ()
     (interactive)
    (compile (format "style-check.rb -v %s" (buffer-file-name))))
(global-set-key "\C-c\C-gs" 'my-action/style-check-file)

;;;; Discover my major mode keybindings
;=======================================
(global-set-key (kbd "C-h C-m") 'discover-my-major)

;;;; MAKE READONLY FILES WRITABLE FOR A FULL ROOT PERMISSION
;============================================================
(make-variable-buffer-local
 (defvar my-override-mode-on-save nil
   "Can be set to automatically ignore read-only mode of a file when saving."))

(defadvice file-writable-p (around my-overide-file-writeable-p act)
  "override file-writable-p if `my-override-mode-on-save' is set."
  (setq ad-return-value (or
                         my-override-mode-on-save
                         ad-do-it)))

(defun my-override-toggle-read-only ()
  "Toggle buffer's read-only status, keeping `my-override-mode-on-save' in sync."
  (interactive)
  (setq my-override-mode-on-save (not my-override-mode-on-save))
  (toggle-read-only))

(defadvice save-buffer (around save-buffer-as-root-around activate)
  "Use sudo to save the current buffer."
  (interactive "p")
  (if (and (buffer-file-name) (not (file-writable-p (buffer-file-name))))
      (let ((buffer-file-name (format "/sudo::%s" buffer-file-name)))
    ad-do-it)
    ad-do-it))


;;;; HELM
;=========
;; guide http://tuhdo.github.io/helm-intro.html
;; Customize helm-bibtex with M-x customize-option

(require 'helm-config)    ; M-x helm-mini
(require 'helm-R)
(require 'helm-anything)

(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-!") 'helm-anything-resume)
(global-set-key (kbd "M-x") 'helm-M-x)

;; HELM-bibtex
(global-set-key (kbd "C-c x") 'helm-bibtex)
(autoload 'helm-bibtex "helm-bibtex" "" t)

(setq 
  helm-bibtex-bibliography '(
   "/media/Data/Bibliography/deeplearninggpu2014.bib"
   "/media/Data/Bibliography/humanGenetics.bib"
))

(setq 
  helm-bibtex-library-path "/media/Data/Bibliography/Bibliography2017/"
  helm-bibtex-notes-path "/media/Data/Bibliography/notes/"
  helm-bibtex-pdf-symbol "P"
)

	(setq helm-bibtex-pdf-open-function    ;; Open PDF in Evince
      (lambda (fpath) (shell-command-to-string
                       (concat "/usr/bin/evince " fpath " &"))))

;; Helm with ag
(global-set-key (kbd "C-c g") 'helm-do-ag)
(global-set-key (kbd "C-c G") 'helm-ag-this-file)


;;;; Line numbering
;==============================
(add-hook 'prog-mode-hook 'linum-mode)
(defun linum-off-mode ()
  "Toggles the line numbers as well as the fringe. This allows me
to maximize the screen estate."
  (interactive)
  (if linum-mode
      (progn
        (fringe-mode '(0 . 0))
        (linum-mode -1))
    (fringe-mode '(8 . 0))
    (linum-mode 1)))

;;;; Flyspell dictionnary and auto-dictionary
;=============================================
; Build a dictionary file
(setq ispell-personal-dictionary
    (concat (getenv "HOME") "/.dictionary.txt"))

;; remove/activate spelling in orgmode
(dolist (hook '(text-mode-hook org-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))
(dolist (hook '(change-log-mode-hook log-edit-mode-hook org-agenda-mode-hook))
  (add-hook hook (lambda () (flyspell-mode -1))))

;;;; ORG mode
;==============================
;(setq org-startup-indented t)

;; setting up the staging area with DROPBOX
(setq org-mobile-directory "/media/Data/Dropbox/Private/org/")
;    (global-set-key "\C-cc" 'org-capture)

(setq org-todo-keywords
           '((sequence "TODO(t)" "CODE(o)" "IDEA(i)" "REMINDER(r@)" "|" "CANCELED(c@)" "DONE(d!)")))
(setq org-todo-keyword-faces
           '(("TODO" . org-warning) ("IDEA" . "yellow") ("CODE" . "blue")
             ("CANCELED" . (:foreground "brown" :weight bold))))
;; TODO ORG mode customization http://tinyurl.com/m3fs7kt

(setq org-clock-persist 'history)
     (org-clock-persistence-insinuate)
(setq org-log-done 'time)
(setq org-log-done 'note)
;; SAve the clock history across Emacs sessions

;; Turns on word wrapping (ONLY in org mode
(define-key org-mode-map "\M-q" 'toggle-truncate-lines)


(setq org-agenda-files (list "/media/Data/Dropbox/Private/org/grymoire.org"))

;; tags
(setq org-tag-alist '(
  ("@work" . ?w) ("@home" . ?h) ("perl" . ?p) 
  ("sysadmin" . ?y) ("R" . ?r) ("latex" . ?l)
  ("ML" . ?m) ("rstat" ?t) ("loisir" . ?i)
  ("emacs" . ?c) ("Table" . ?s)
))

;; PDFs visited in Org-mode are opened in Evince (and not in the default choice) http://stackoverflow.com/a/8836108/789593
(add-hook 'org-mode-hook
      '(lambda ()
         (delete '("\\.pdf\\'" . default) org-file-apps)
         (add-to-list 'org-file-apps '("\\.pdf\\'" . "evince %s"))))


;;;; ESS (emacs and R)
;=====================
(add-hook 'ess-mode-hook	;; flyspell disable
          (lambda ()
            (flyspell-mode -1)
;    		(ispell-kill-ispell 1)
;    		(flyspell-prog-mode -1)
          ))

;; R change between similar executed scripts
(require 'ess-site)
	(put 'upcase-region 'disabled nil)
(add-hook 'inferior-ess-mode-hook
'(lambda nil
          (define-key inferior-ess-mode-map [\C-up]
              'comint-previous-matching-input-from-input)
          (define-key inferior-ess-mode-map [\C-down]
              'comint-next-matching-input-from-input)
          (define-key inferior-ess-mode-map [\C-x \t]
              'comint-dynamic-complete-filename)
     )
 )


;;;; LaTeX mode AucTex
;======================
;; Everything is working nicely
;; No problem with any package
;; Master files will be introduced in each nested tex files
;; Be carefull to chose correctly the master file
;; Once set, a master file can be corrected with C-_
;; Compile using Latex
;; sometimes Tex is chosen automatically.It doesn't work

(setq TeX-parse-self t) ; Enable parse on load.
(setq TeX-auto-save t) ; Enable parse on save.    
(setq TeX-auto-untabify t) ; convert tab to spaces (Parsing Files section of the manual)

(setq-default TeX-master nil)

(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)

    (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
    (setq reftex-plug-into-AUCTeX t)
	(setq tex-dvi-view-command "xdvi")
;	(latex-preview-pane-enable) ; preview pane of latex compilation doc
	



(add-hook 'LaTeX-mode-hook
	(lambda ()
;		(setq TeX-master (guess-TeX-master (buffer-file-name)))
		(auto-fill-mode -1)
		(setq fill-column 2000)
;		(flyspell-mode -1)
))

; Table of content activation in menubar
(add-hook 'reftex-load-hook 'imenu-add-menubar-index)
  (add-hook 'reftex-mode-hook 'imenu-add-menubar-index)


;;;; SCRATCH buffer
;==================
;; The scratch buffer can be initialized with a major mode of choice
(setq initial-major-mode 'r-mode)
(setq initial-scratch-message "\
# This buffer is for notes you don't want to save, and for R code.
# If you want to create an *.Rnw file, run ~/perls/knitr.pl
# then enter the file's and project's name.

")

;;;; Openning large files
;=========================
(require 'vlf-integrate)
(eval-after-load "vlf"
  '(define-key vlf-prefix-map "\C-xv" vlf-mode-map))

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)


;;;; Yasnippets
;==============================
(require 'yasnippet)
(yas-global-mode 1)
(setq 
  yas-snippet-dirs "~/.emacs.d/snippets/"
  yas-load-directory "~/.emacs.d/elpa/"
  yas-use-menu nil
)

;(define-key yas-minor-mode-map (kbd "TAB") nil) ; remove Tab completion with yas (if failed try <tab>)

;;;; Expand Region
;=================
(require 'expand-region)
    (global-set-key (kbd "C-=") 'er/expand-region)

;;;; Autocompletion scripts
;==============================
;; source (http://tinyurl.com/c7enart)
(require 'ac-math)
(require 'auto-complete)
(require 'auto-complete-config)

(add-to-list 'ac-modes 'latex-mode) ; make auto-complete aware of {{{latex-mode}}}
(ac-flyspell-workaround)		; use flyspell workaround
;(define-key ac-mode-map (kbd "TAB") 'auto-complete)			;; define a key
(add-to-list 'load-path "~/.emacs.d/elpa/")
(add-to-list 'ac-dictionary-directories "~/.emacs.d/elpa/")
(ac-config-default)

(setq
 ac-auto-show-menu nil
 ac-auto-start 4
; ac-menu-height 20
; ac-math-unicode-in-math-p t
; ac-auto-start nil		;; disable autoatic autcompletion
)

; ac with ispell
   (eval-after-load "auto-complete"
     '(progn
         (ac-ispell-setup)))

; ac with latex
(add-hook 'LaTeX-mode-hook 'ac-latex-mode-setup) 

; remove source-math-latex to increase speed
(defun ac-latex-mode-setup ()         
  (setq ac-sources
     (append '(ac-source-math-unicode ac-source-latex-commands)
               ac-sources)))

; popup fix
;(setq popup-use-optimized-column-computation nil)




;; Some configuration of keybindings
;; some are global
;; others are hooked to minor or major modes
;; Variables are declared earlier
;; Modes are declared throughout the file
;; This section holds a collection of keybindings grouped together

(global-set-key "\C-cc" 'reftex-citation)
(global-set-key "\C-cl" 'org-store-link)	; used in combination w/ Cc Cl 
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(global-set-key (kbd "C-c C-l") 'linum-off-mode)  ;; For Linux
(global-set-key (kbd "M-<up>") 'move-line-up)
(global-set-key (kbd "M-<down>") 'move-line-down)
(global-set-key [f11] 'fullscreen)
(global-set-key (kbd "C-s") 'isearch-forward-regexp) ;; activate regular expression in isearch
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)
(global-set-key [down-mouse-3] 'imenu)  ; TOC activation right-mouse click
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "<escape>") 'god-mode-all)
(global-set-key (kbd "C-x C-1") 'delete-other-windows)
(global-set-key (kbd "C-x C-2") 'split-window-below)
(global-set-key (kbd "C-x C-3") 'split-window-right)
(global-set-key (kbd "C-x C-0") 'delete-window)


;; From here forth, scripts are not important for the workflow
;; Lisp code are that of coding languages
;; They are not essential for the running of previous major modes

;; Markdown Mode
;==============================
(add-hook 'markdown-mode-hook
            (lambda ()
              (when buffer-file-name
                (add-hook 'after-save-hook
                          'check-parens
                          nil t))))

 (autoload 'markdown-mode "markdown-mode"
       "Major mode for editing Markdown files" t)
    (add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
    (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
    (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))


;; PERL
;==========
;;; Use cperl-mode instead of the default perl-mode
(add-to-list 'auto-mode-alist '("\\.\\([pP][Llm]\\|al\\)\\'" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("perl" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("perl5" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("miniperl" . cperl-mode))

;; special Perl indentation
(defun n-cperl-mode-hook ()
  (setq cperl-indent-level 4)
  (setq cperl-continued-statement-offset 4)
  (setq cperl-extra-newline-before-brace t)
  (setq cperl-close-paren-offset -4)
  (setq cperl-indent-parens-as-block t)
  (setq cperl-tab-always-indent t)
  (set-face-background 'cperl-array-face "#5c888b")
  (set-face-foreground 'cperl-array-face "#656555")
  (set-face-background 'cperl-hash-face "#9c6363")
  (set-face-foreground 'cperl-hash-face "#656555")
  )
(add-hook 'cperl-mode-hook 'n-cperl-mode-hook t)

;; Python
;============
(setq py-install-directory "~/.emacs.d/python-mode.el-6.1.3/")
  (add-to-list 'load-path py-install-directory)
  (require 'python-mode)

;; Browse the Python Documentation using Info (C-h S)
(require 'info-look)

(info-lookup-add-help
 :mode 'python-mode
 :regexp "[[:alnum:]_]+"
 :doc-spec
 '(("(python)Index" nil "")))

;; enable code autocompletion
  (setq py-load-pymacs-p t)

; command 
(defun py-next-block ()
   "go to the next block.  Cf. `forward-sexp' for lisp-mode"
   (interactive)
   (py-mark-block nil 't)
   (back-to-indentation))

;; eldoc for org and python
;==========================
; activate it
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
     (add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
     (add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)

(autoload 'eldoc-in-minibuffer-mode "eldoc-eval")
   (eldoc-in-minibuffer-mode 1)

(defun ted-frob-eldoc-argument-list (string)
   "Upcase and fontify STRING for use with `eldoc-mode'."
   (propertize (upcase string)
               'face 'font-lock-variable-name-face))
 (setq eldoc-argument-case 'ted-frob-eldoc-argument-list)

(org-eldoc-hook-setup) ;; have org-eldoc add itself to `org-mode-hook'
; for python
(add-hook 'python-mode-hook
          '(lambda () (eldoc-mode 1)) t)

 (set (make-local-variable 'eldoc-documentation-function)
        'tal-eldoc-function)




;;----------------------------------------------------------
;; ---- BEGIN Email client ----
;;----------------------------------------------------------
(add-to-list 'load-path "~/.emacs.d/mu4e")
(require 'mu4e)

;; default
(setq mu4e-maildir "~/Maildir")
(setq mu4e-drafts-folder "/[Gmail].Drafts")
(setq mu4e-sent-folder   "/[Gmail].Sent Mail")
(setq mu4e-trash-folder  "/[Gmail].Trash")

;; don't save message to Sent Messages, Gmail/IMAP takes care of this
(setq mu4e-sent-messages-behavior 'delete)

;; setup some handy shortcuts
;; you can quickly switch to your Inbox -- press ``ji''
;; then, when you want archive some messages, move them to
;; the 'All Mail' folder by pressing ``ma''.

(setq mu4e-maildir-shortcuts
      '( ("/INBOX"               . ?i)
         ("/[Gmail].Sent Mail"   . ?s)
         ("/[Gmail].Trash"       . ?t)
         ("/[Gmail].All Mail"    . ?a)))

;; allow for updating mail using 'U' in the main view:
(setq mu4e-get-mail-command "offlineimap")

;; something about ourselves
(setq
 user-mail-address "slei.bass@gmail.com"
 user-full-name "Sleiman Bassim"
 message-signature
 (concat
  "Sleiman Bassim, PhD\n"
  "Microbiology & Molecular Genetics\n"
  "University of Vermont\n"
  "Hills Building, 105 Carrigan Drive\n"
  "Burlington, VT 05405\n"
   "\n"))

;; sending mail -- replace USERNAME with your gmail username
;; also, make sure the gnutls command line utils are installed
;; package 'gnutls-bin' in Debian/Ubuntu

(require 'smtpmail)
;; (setq message-send-mail-function 'smtpmail-send-it
;;       starttls-use-gnutls t
;;       smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
;;       smtpmail-auth-credentials
;;       '(("smtp.gmail.com" 587 "renws1990@gmail.com" nil))
;;       smtpmail-default-smtp-server "smtp.gmail.com"
;;       smtpmail-smtp-server "smtp.gmail.com"
;;       smtpmail-smtp-service 587)

;; alternatively, for emacs-24 you can use:
(setq message-send-mail-function 'smtpmail-send-it
    smtpmail-stream-type 'starttls
    smtpmail-default-smtp-server "smtp.gmail.com"
    smtpmail-smtp-server "smtp.gmail.com"
    smtpmail-smtp-service 587)

;; don't keep message buffers around
(setq message-kill-buffer-on-exit t)

;;----------------------------------------------------------
;; ---- END Email client ----
;;----------------------------------------------------------
