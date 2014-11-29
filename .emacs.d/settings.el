
(setq user-full-name "Sleiman Bassim"
      user-mail-address "slei.bass@gmail.com")

(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)

(global-font-lock-mode t)

(setq gc-cons-threshold 20000000)

(setq jit-lock-stealth-time 1
      jit-lock-stealth-load 100
      jit-lock-chunk-size 1000
      jit-lock-defer-time 0.01)

(transient-mark-mode t)

(when (functionp 'menu-bar-mode)
  (menu-bar-mode -1))
(when (functionp 'set-scroll-bar-mode)
  (set-scroll-bar-mode 'nil))
(when (functionp 'mouse-wheel-mode)
  (mouse-wheel-mode -1))
(when (functionp 'tooltip-mode)
  (tooltip-mode -1))
(when (functionp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (functionp 'blink-cursor-mode)
  (blink-cursor-mode -1))

(setq ring-bell-function (lambda()))
(setq inhibit-startup-message t
      initial-major-mode 'fundamental-mode)

(line-number-mode 1)
(column-number-mode 1)

(defalias 'yes-or-no-p 'y-or-n-p)

(setq read-file-name-completion-ignore-case t)

(setq line-move-visual t)

(pending-delete-mode t)

(setq make-pointer-invisible t)

(setq-default fill-column 80)
(setq-default default-tab-width 2)
(setq-default indent-tabs-mode nil)

(setq-default find-file-visit-truename nil)

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets))

(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "M-%") 'query-replace-regexp)

(setq split-width-threshold 180)

(whitespace-mode t)

(set-default 'indicate-empty-lines t)
(setq show-trailing-whitespace t)

(random t)

(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'latex-mode-hook 'turn-on-auto-fill)

(use-package diminish
  :init
  (progn
    (diminish 'auto-fill-function "AF")))

(setq sentence-end-double-space nil)

(setq diff-switches "-u")

(setq initial-major-mode 'r-mode
      initial-scratch-message "\
# This buffer is for notes you don't want to save, and for R code.
# If you want to create an *.Rnw file, run ~/perls/knitr.pl
# then enter the file's and project's name.
")
;;; Burry the scratch buffer but dont kill it
(defadvice kill-buffer (around kill-buffer-around-advice activate)
  (let ((buffer-to-kill (ad-get-arg 0)))
    (if (equal buffer-to-kill "*scratch*")
        (bury-buffer)
      ad-do-it)))

(use-package vlf-integrate
  :bind ("C-c v" . vlf))
;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

(when (functionp 'set-fringe-style)
  (set-fringe-style 0))

(global-auto-revert-mode 1)

(when (window-system)
  (setenv "EMACS_GUI" "t"))

;; savehist
(setq savehist-additional-variables
      ;; also save my search entries
      '(search-ring regexp-search-ring)
      savehist-file "~/.emacs.d/savehist")
(savehist-mode t)
(setq-default save-place t)
;; delete-auto-save-files
(setq delete-auto-save-files t)
(setq backup-directory-alist
      '(("." . "~/.emacs_backups")))

(setq ispell-personal-dictionary "~/.dictionary.txt")
;; flyspell
(use-package flyspell
  :config
  (define-key flyspell-mode-map (kbd "M-n") 'flyspell-goto-next-error)
  (define-key flyspell-mode-map (kbd "M-p") 'ispell-word))

(use-package diminish
  :init
  (progn
    (diminish 'flyspell-mode "FS")))

(dolist (hook '(text-mode-hook org-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))
(dolist (hook '(change-log-mode-hook log-edit-mode-hook org-agenda-mode-hook))
  (add-hook hook (lambda () (flyspell-mode -1))))

(add-hook 'ess-mode-hook        ;; flyspell disable
          (lambda ()
            (flyspell-mode -1)
;               (ispell-kill-ispell 1)
;               (flyspell-prog-mode -1)
          ))

(use-package view
  :bind
  (("C-x v" . view-mode))
  :config
  (progn
    ;; When in view-mode, the buffer is read-only:
    (setq view-read-only t)

    (defun View-goto-line-last (&optional line)
      "goto last line"
      (interactive "P")
      (goto-line (line-number-at-pos (point-max))))

    ;; less like
    (define-key view-mode-map (kbd "N") 'View-search-last-regexp-backward)
    (define-key view-mode-map (kbd "?") 'View-search-regexp-backward?)
    (define-key view-mode-map (kbd "g") 'View-goto-line)
    (define-key view-mode-map (kbd "G") 'View-goto-line-last)
    (define-key view-mode-map (kbd "b") 'View-scroll-page-backward)
    (define-key view-mode-map (kbd "f") 'View-scroll-page-forward)
    ;; vi/w3m like
    (define-key view-mode-map (kbd "h") 'backward-char)
    (define-key view-mode-map (kbd "j") 'next-line)
    (define-key view-mode-map (kbd "k") 'previous-line)
    (define-key view-mode-map (kbd "l") 'forward-char)
    (define-key view-mode-map (kbd "[") 'backward-paragraph)
    (define-key view-mode-map (kbd "]") 'forward-paragraph)
    (define-key view-mode-map (kbd "J") 'View-scroll-line-forward)
    (define-key view-mode-map (kbd "K") 'View-scroll-line-backward)))

(use-package doc-view
  :config
  (define-key doc-view-mode-map (kbd "j")
    'doc-view-next-line-or-next-page)
  (define-key doc-view-mode-map (kbd "k")
    'doc-view-previous-line-or-previous-page))

(use-package god-mode
  :config
  (define-key god-local-mode-map (kbd "z") 
    'repeat)
  (define-key god-local-mode-map (kbd "i") 
    'god-local-mode)
  :bind ("<escape>" . god-mode-all))

;(add-to-list 'god-exempt-major-modes 'dired-mode)
(add-hook 'god-mode-enabled-hook 'my-update-cursor)
(add-hook 'god-mode-disabled-hook 'my-update-cursor)
; change cursor style between God-mode and regular
(defun my-update-cursor ()
  (setq cursor-type 
        (if (or god-local-mode buffer-read-only) 'bar 'box)))

(defun my/dired-mode-hook ()
  (hl-line-mode t)
  (toggle-truncate-lines 1))

(use-package dired
  :bind ("C-x C-j" . dired-jump)
  :config
  (progn
    (use-package dired-x
      :init (setq-default dired-omit-files-p t)
      :config
      (when (eq system-type 'darwin)
        (add-to-list 'dired-omit-extensions ".DS_STORE")))
    (use-package dired-imenu)
    (customize-set-variable 'diredp-hide-details-initially-flag nil)
    (use-package dired+)
    (put 'dired-find-alternate-file 'disabled nil)
    (setq ls-lisp-dirs-first t
          dired-recursive-copies 'always
          dired-recursive-deletes 'always
          dired-dwim-target t
          delete-by-moving-to-trash t
          wdired-allow-to-change-permissions t)
    (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)
    (define-key dired-mode-map (kbd "C-M-u") 'dired-up-directory)
    (define-key dired-mode-map (kbd "C-x C-q") 'wdired-change-to-wdired-mode)
    (add-hook 'dired-mode-hook 'my/dired-mode-hook)))

(use-package saveplace
  :init
  (setq-default save-place t)
  (setq save-place-file (expand-file-name "saved-places" user-emacs-directory)))

(use-package recentf
  :init
  (progn
    (setq recentf-max-saved-items 300
          recentf-exclude '("/auto-install/" ".recentf" "/repos/" "/elpa/"
                            "\\.mime-example" "\\.ido.last" "COMMIT_EDITMSG"
                            ".gz"
                            "~$" "/tmp/" "/ssh:" "/sudo:" "/scp:")
          recentf-auto-cleanup 600)
    (when (not noninteractive) (recentf-mode 1))

    (defun recentf-save-list ()
      "Save the recent list.
Load the list from the file specified by `recentf-save-file',
merge the changes of your current session, and save it back to
the file."
      (interactive)
      (let ((instance-list (copy-list recentf-list)))
        (recentf-load-list)
        (recentf-merge-with-default-list instance-list)
        (recentf-write-list-to-file)))

    (defun recentf-merge-with-default-list (other-list)
      "Add all items from `other-list' to `recentf-list'."
      (dolist (oitem other-list)
        ;; add-to-list already checks for equal'ity
        (add-to-list 'recentf-list oitem)))

    (defun recentf-write-list-to-file ()
      "Write the recent files list to file.
Uses `recentf-list' as the list and `recentf-save-file' as the
file to write to."
      (condition-case error
          (with-temp-buffer
            (erase-buffer)
            (set-buffer-file-coding-system recentf-save-file-coding-system)
            (insert (format recentf-save-file-header (current-time-string)))
            (recentf-dump-variable 'recentf-list recentf-max-saved-items)
            (recentf-dump-variable 'recentf-filter-changer-current)
            (insert "\n \n;;; Local Variables:\n"
                    (format ";;; coding: %s\n" recentf-save-file-coding-system)
                    ";;; End:\n")
            (write-file (expand-file-name recentf-save-file))
            (when recentf-save-file-modes
              (set-file-modes recentf-save-file recentf-save-file-modes))
            nil)
        (error
         (warn "recentf mode: %s" (error-message-string error)))))))

(add-hook 'prog-mode-hook
          (lambda ()
            (use-package idle-highlight-mode
              :init (idle-highlight-mode t))
            (setq show-trailing-whitespace t)
            (hl-line-mode 1)
            (subword-mode t)))

(use-package eldoc
  :config
  (progn
    (use-package diminish
      :init
      (progn (diminish 'eldoc-mode "ed")))
    (setq eldoc-idle-delay 0.2)
    (set-face-attribute 'eldoc-highlight-function-argument nil
                        :underline t :foreground "green"
                        :weight 'bold)))
;;; elisp regex grouping
(set-face-foreground 'font-lock-regexp-grouping-backslash "#ff1493")
(set-face-foreground 'font-lock-regexp-grouping-construct "#ff8c00")
;;; ielm buffer
(defun ielm-other-window ()
  "Run ielm on other window"
  (interactive)
  (switch-to-buffer-other-window
   (get-buffer-create "*ielm*"))
  (call-interactively 'ielm))

(define-key emacs-lisp-mode-map (kbd "C-c C-z") 'ielm-other-window)
(define-key lisp-interaction-mode-map (kbd "C-c C-z") 'ielm-other-window)

(use-package org
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c b" . org-iswitchb)
         ("C-c c" . org-capture)
         ("C-c M-p" . org-babel-previous-src-block)
         ("C-c M-n" . org-babel-next-src-block)
         ("C-c S" . org-babel-previous-src-block)
         ("C-c s" . org-babel-next-src-block))
  :config
  (progn
    (use-package org-install)
    ;; load github-flavored-markdown
    (add-hook 'org-mode-hook 'turn-on-auto-fill)
    (use-package org-latex)
    (unless (boundp 'org-export-latex-classes)
      (setq org-export-latex-classes nil))
    ;; Koma-class
    ;; initialize with #+LaTeX_CLASS: koma-article
    (add-to-list 'org-export-latex-classes
          '("koma-article"
             "\\documentclass{scrartcl}"
             ("\\section{%s}" . "\\section*{%s}")
             ("\\subsection{%s}" . "\\subsection*{%s}")
             ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
             ("\\paragraph{%s}" . "\\paragraph*{%s}")
             ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
    (setq org-directory "/media/Data/Dropbox/Private/org"
          org-startup-indented t
          org-return-follows-link t
          ;; allow changing between todo stats directly by hotkey
          org-use-fast-todo-selection t
          org-src-fontify-natively t
          org-fontify-whole-heading-line t
          org-completion-use-ido t
          org-edit-src-content-indentation 0
          ;; Imenu should use 3 depth instead of 2
          org-imenu-depth 3
          org-agenda-start-on-weekday nil
          ;; Use sticky agenda's so they persist
          org-agenda-sticky t
          ;; show 4 agenda days
          org-agenda-span 4
          org-special-ctrl-a/e t
          org-special-ctrl-k t
          org-yank-adjusted-subtrees nil
          org-src-window-setup 'current-window
          ;; Overwrite the current window with the agenda
          org-agenda-window-setup 'current-window
          ;; Use full outline paths for refile targets - we file directly with IDO
          org-refile-use-outline-path t
          ;; Targets complete directly with IDO
          org-outline-path-complete-in-steps nil
          ;; Allow refile to create parent tasks with confirmation
          org-refile-allow-creating-parent-nodes (quote confirm)
          ;; Use IDO for both buffer and file completion and ido-everywhere to t
          ido-everywhere t
          ido-max-directory-size 100000
          ;; always enable noweb, results as code and exporting both
          org-babel-default-header-args
          (cons '(:noweb . "yes")
                (assq-delete-all :noweb org-babel-default-header-args))
          org-babel-default-header-args
          (cons '(:exports . "both")
                (assq-delete-all :exports org-babel-default-header-args))
          ;; I don't want to be prompted on every code block evaluation
          org-confirm-babel-evaluate nil
          ;; Compact the block agenda view
          org-agenda-compact-blocks t
          ;; Mark entries as done when archiving
          org-archive-mark-done nil
          org-archive-location "%s_archive::* Archived Tasks"
          ;; Sorting order for tasks on the agenda
          org-agenda-sorting-strategy
          (quote ((agenda habit-down
                          time-up
                          priority-down
                          user-defined-up
                          effort-up
                          category-keep)
                  (todo priority-down category-up effort-up)
                  (tags priority-down category-up effort-up)
                  (search priority-down category-up)))

          ;; Enable display of the time grid so we can see the marker for the current time
          org-agenda-time-grid (quote ((daily today remove-match)
                                       #("----------------" 0 16 (org-heading t))
                                       (0900 1100 1300 1500 1700)))
          org-agenda-include-diary t
          org-agenda-insert-diary-extract-time t
          org-agenda-repeating-timestamp-show-all t
          ;; Show all agenda dates - even if they are empty
          org-agenda-show-all-dates t)

    ;; PDFs visited in Org-mode are opened in Evince (and not in the default choice) http://stackoverflow.com/a/8836108/789593
    (add-hook 'org-mode-hook
              '(lambda ()
                 (delete '("\\.pdf\\'" . default) org-file-apps)
                 (add-to-list 'org-file-apps '("\\.pdf\\'" . "evince %s"))))

    (use-package org-toc
      :disabled t
      :init (add-hook 'org-mode-hook 'org-toc-enable))

    ;; Agenda org-mode files
    (setq org-agenda-files
          '("/media/Data/Dropbox/Private/org/grymoire.org"
            "/media/Data/Dropbox/Private/org/human/human.org"
            ))
    ;; Org todo keywords
    (setq org-todo-keywords
          (quote
           ((sequence "SOMEDAY(s)" "TODO(t)" "INPROGRESS(i)" "WAITING(w@/!)" "NEEDSREVIEW(n@/!)"
                      "|" "DONE(d)")
            (sequence "WAITING(w@/!)" "HOLD(h@/!)"
                      "|" "CANCELLED(c@/!)"))))
    ;; Org faces
    (setq org-todo-keyword-faces
          (quote (("TODO" :foreground "brown" :weight bold)
                  ("INPROGRESS" :foreground "deep sky blue" :weight bold)
                  ("SOMEDAY" :foreground "purple" :weight bold)
                  ("NEEDSREVIEW" :foreground "#edd400" :weight bold)
                  ("DONE" :foreground "forest green" :weight bold)
                  ("WAITING" :foreground "orange" :weight bold)
                  ("HOLD" :foreground "magenta" :weight bold)
                  ("CANCELLED" :foreground "forest green" :weight bold))))
    ;; add or remove tags on state change
    (setq org-todo-state-tags-triggers
          (quote (("CANCELLED" ("CANCELLED" . t))
                  ("WAITING" ("WAITING" . t))
                  ("HOLD" ("WAITING") ("HOLD" . t))
                  (done ("WAITING") ("HOLD"))
                  ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
                  ("INPROGRESS" ("WAITING") ("CANCELLED") ("HOLD"))
                  ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))))
    ;; refile targets all level 1 headers in todo.org and notes.org
    (setq org-refile-targets '((nil :maxlevel . 2)
                               (org-agenda-files :maxlevel . 2)))
    ;; quick access to common tags
    (setq org-tag-alist
          '(("@WORK" . ?w)
            ("@HOME" . ?h)
            ("PERL" . ?p)
            ("SYSADMIN" . ?s)
            ("LATEX" . ?l)
            ("ML" . ?m)
            ("RSTAT" ?r)
            ("TABLE" . ?t)
            ("export" . ?e)
            ("noexport" . ?n)))
    ;; Custom agenda command definitions
    (setq org-agenda-custom-commands
          (quote
           (("N" "Notes" tags "NOTE"
             ((org-agenda-overriding-header "Notes")
              (org-tags-match-list-sublevels t)))
            (" " "Agenda"
             ((agenda "" nil)
              ;; All items with the "REFILE" tag, everything in refile.org
              ;; automatically gets that applied
              (tags "REFILE"
                    ((org-agenda-overriding-header "Tasks to Refile")
                     (org-tags-match-list-sublevels nil)))
              ;; All "INPROGRESS" todo items
              (todo "INPROGRESS"
                    ((org-agenda-overriding-header "Current work")))
              ;; All headings with the "support" tag
              (tags "support/!"
                    ((org-agenda-overriding-header "Support cases")))
              ;; All "NEESREVIEW" todo items
              (todo "NEEDSREVIEW"
                    ((org-agenda-overriding-header "Waiting on reviews")))
              ;; All "WAITING" items without a "support" tag
              (tags "WAITING-support"
                    ((org-agenda-overriding-header "Waiting for feedback")))
              ;; All TODO items
              (todo "TODO"
                    ((org-agenda-overriding-header "Task list")
                     (org-agenda-sorting-strategy '(category-keep))))
              ;; Everything on hold
              (todo "HOLD"
                    ((org-agenda-overriding-header "On-hold")))
              ;; Everything that's done and archivable
              (todo "DONE"
                    ((org-agenda-overriding-header "Tasks for archive")
                     (org-agenda-skip-function 'my/skip-non-archivable-tasks))))
             nil))))

    (ido-mode (quote both))

    ;; Exclude DONE state tasks from refile targets
    (defun my/verify-refile-target ()
      "Exclude todo keywords with a done state from refile targets"
      (not (member (nth 2 (org-heading-components)) org-done-keywords)))
    (setq org-refile-target-verify-function 'my/verify-refile-target)

    (define-key org-mode-map "\M-q" 'toggle-truncate-lines)
    (define-key org-mode-map (kbd "C-M-<return>") 'org-insert-todo-heading)
    (define-key org-mode-map (kbd "C-c t") 'org-todo)
    (define-key org-mode-map (kbd "M-G") 'org-plot/gnuplot)
    (local-unset-key (kbd "M-S-<return>"))

    (add-hook 'org-mode-hook
              (lambda ()
                (turn-on-flyspell)
                (define-key org-mode-map [C-tab] 'other-window)
                (define-key org-mode-map [C-S-tab]
                  (lambda ()
                    (interactive)
                    (other-window -1)))
                (define-key org-mode-map (kbd "C-'")
                  'eyebrowse-next-window-config)))

    ;; org-babel stuff
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((emacs-lisp . t)
       (dot . t)
       (sh . t)
       (R . t)
       (perl . t)
       (gnuplot . t)
       (latex . t)))

    ;; Use org.css from the :wq website for export document stylesheets
    (setq org-html-head-extra
          "<link rel=\"stylesheet\" href=\"http://dakrone.github.io/org.css\" type=\"text/css\" />")
    (setq org-html-head-include-default-style nil)


    ;; ensure this variable is defined
    (unless (boundp 'org-babel-default-header-args:sh)
      (setq org-babel-default-header-args:sh '()))

    ;; add a default shebang header argument shell scripts
    (add-to-list 'org-babel-default-header-args:sh
                 '(:shebang . "#!/usr/bin/env zsh"))


    ;; Function declarations
    (defun my/skip-non-archivable-tasks ()
      "Skip trees that are not available for archiving"
      (save-restriction
        (widen)
        ;; Consider only tasks with done todo headings as archivable candidates
        (let ((next-headline (save-excursion
                               (or (outline-next-heading) (point-max))))
              (subtree-end (save-excursion (org-end-of-subtree t))))
          (if (member (org-get-todo-state) org-todo-keywords-1)
              (if (member (org-get-todo-state) org-done-keywords)
                  (let* ((daynr (string-to-int
                                 (format-time-string "%d" (current-time))))
                         (a-month-ago (* 60 60 24 (+ daynr 1)))
                         (this-month
                          (format-time-string "%Y-%m-" (current-time)))
                         (subtree-is-current
                          (save-excursion
                            (forward-line 1)
                            (and (< (point) subtree-end)
                                 (re-search-forward this-month
                                                    subtree-end t)))))
                    (if subtree-is-current
                        subtree-end     ; Has a date in this month, skip it
                      nil))             ; available to archive
                (or subtree-end (point-max)))
            next-headline))))

    (defun my/save-all-agenda-buffers ()
      "Function used to save all agenda buffers that are
currently open, based on `org-agenda-files'."
      (interactive)
      (save-current-buffer
        (dolist (buffer (buffer-list t))
          (set-buffer buffer)
          (when (member (buffer-file-name)
                        (mapcar 'expand-file-name (org-agenda-files t)))
            (save-buffer)))))

    ;; save all the agenda files after each capture
    (add-hook 'org-capture-after-finalize-hook 'my/save-all-agenda-buffers)

    (use-package org-id
      :config
      (progn
        (setq org-id-link-to-org-use-id t)

        (defun my/org-custom-id-get (&optional pom create prefix)
          "Get the CUSTOM_ID property of the entry at point-or-marker POM.
If POM is nil, refer to the entry at point. If the entry does not
have an CUSTOM_ID, the function returns nil. However, when CREATE
is non nil, create a CUSTOM_ID if none is present already. PREFIX
will be passed through to `org-id-new'. In any case, the
CUSTOM_ID of the entry is returned."
          (interactive)
          (org-with-point-at pom
            (let ((id (org-entry-get nil "CUSTOM_ID")))
              (cond
               ((and id (stringp id) (string-match "\\S-" id))
                id)
               (create
                (setq id (org-id-new prefix))
                (org-entry-put pom "CUSTOM_ID" id)
                (org-id-add-location id (buffer-file-name (buffer-base-buffer)))
                id)))))

        (defun my/org-add-ids-to-headlines-in-file ()
          "Add CUSTOM_ID properties to all headlines in the
current file which do not already have one."
          (interactive)
          (org-map-entries (lambda () (my/org-custom-id-get (point) 'create))))

        ;; automatically add ids to captured headlines
        (add-hook 'org-capture-prepare-finalize-hook
                  (lambda () (my/org-custom-id-get (point) 'create)))))

    (defun my/org-inline-css-hook (exporter)
      "Insert custom inline css to automatically set the
background of code to whatever theme I'm using's background"
      (when (eq exporter 'html)
        (let* ((my-pre-bg (face-background 'default))
               (my-pre-fg (face-foreground 'default)))
          ;;(setq org-html-head-include-default-style nil)
          (setq
           org-html-head-extra
           (concat
            org-html-head-extra
            (format "<style type=\"text/css\">\n pre.src {background-color: %s; color: %s;}</style>\n"
                    my-pre-bg my-pre-fg))))))

    (add-hook 'org-export-before-processing-hook 'my/org-inline-css-hook)

    ))

(defun my/terminal-notifier-notify (title message)
  "Show a message with `terminal-notifier-command`."
  (interactive)
  (start-process "terminal-notifier"
                 "*terminal-notifier*"
                 "terminal-notifier"
                 "-title" title
                 "-message" message))

(use-package alert
  :config
  (progn
    (alert-define-style 'terminal-notifier
                        :title "terminal-notifier"
                        :notifier
                        (lambda (info)
                          (my/terminal-notifier-notify
                           (plist-get info :title)
                           (plist-get info :message))
                          ;; The :category of the alert
                          (plist-get info :category)
                          ;; The major-mode this alert relates to
                          (plist-get info :mode)
                          ;; The buffer the alert relates to
                          (plist-get info :buffer)
                          ;; Severity of the alert.  It is one of:
                          ;;   `urgent'
                          ;;   `high'
                          ;;   `moderate'
                          ;;   `normal'
                          ;;   `low'
                          ;;   `trivial'
                          (plist-get info :severity)
                          ;; Whether this alert should persist, or fade away
                          (plist-get info :persistent)
                          ;; Data which was passed to `alert'.  Can be
                          ;; anything.
                          (plist-get info :data))

                        ;; Removers are optional.  Their job is to remove
                        ;; the visual or auditory effect of the alert.
                        :remover
                        (lambda (info)
                          ;; It is the same property list that was passed to
                          ;; the notifier function.
                          ))
    (if (eq (window-system) 'ns)
        (setq alert-default-style 'terminal-notifier))))

(desktop-save-mode -1)                  ;; Save state of the desktop
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

(add-to-list 'custom-theme-load-path (expand-file-name "~/.emacs.d/themes/"))
(load-theme 'zenburn t) 
;(load-theme 'monokai t) 
;(load-theme 'gotham t)
;(load-theme 'leuven t) ;; best for org-mode
;(load-theme 'spacegray t)
;(load-theme 'molokai t)
;(load-theme 'gruvbox t)

;(setq debug-on-error t) ;; debug-on-error
;(iswitchb-mode -1)  ; Inactivate iswitch to use HELM Cx-b and Cc-m 
;(setq-default transient-mark-mode t) ; highligh the marked region
;(set-face-attribute 'region nil :background "#666")
;(require 'uniquify) ; change title buffer
;(require 'ansi-color) ; translates ANSI SGR into emacs faces
;(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(global-set-key (kbd "M-S-s-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "M-S-s-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "M-S-s-<down>") 'shrink-window)
(global-set-key (kbd "M-S-s-<up>") 'enlarge-window)

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

(use-package undo-tree
  :idle (global-undo-tree-mode t)
  :diminish ""
  :config
  (progn
    (define-key undo-tree-map (kbd "C-x u") 'undo-tree-visualize)
    (define-key undo-tree-map (kbd "C-/") 'undo-tree-undo)))

(use-package auto-indent-mode
  :config
  (progn
    (setq auto-indent-key-for-end-of-line-then-newline "<M-return>"
        auto-indent-key-for-end-of-line-insert-char-then-newline "<M-S-return>"
        auto-indent-indent-style 'aggressive)))

(use-package ace-jump-mode
  :bind (("C-c SPC" . ace-jump-mode)
         ("C-c M-SPC" . ace-jump-line-mode)
         ("C-x SPC" . ace-jump-mode-pop-mark)))

(use-package keyfreq
  :config
  (progn
    (setq keyfreq-mode 1
          keyfreq-autosave-mode 1)))

(use-package bookmark+
  :config
  (progn
    (setq bookmark-version-control t
          ;; auto-save bookmarks
          bookmark-save-flag 1)))

(use-package yasnippet
  :diminish ""
  :idle (yas-reload-all)
  :config
  (setq yas-snippet-dirs "~/.emacs.d/snippets/"
        yas-load-directory "~/.emacs.d/elpa/"
        yas-use-menu nil)
  (yas-global-mode 1))
;;; Chose snippets using Helm
(progn
      (defun my-yas/prompt (prompt choices &optional display-fn)
      (let* ((names (loop for choice in choices
                          collect (or (and display-fn
                                           (funcall display-fn choice))
                                      coice)))
             (selected (helm-other-buffer
                        `(((name . ,(format "%s" prompt))
                           (candidates . names)
                           (action . (("Insert snippet" . (lambda (arg)
                                                            arg))))))
                        "*helm yas/prompt*")))
        (if selected
            (let ((n (position selected names :test 'equal)))
              (nth n choices))
          (signal 'quit "user quit!"))))
      (custom-set-variables '(yas/prompt-functions '(my-yas/prompt))))

(global-set-key (kbd "C-!") 'yas-insert-snippet)  ;; yas + helm

(smartparens-global-mode t)
;; different colors for parenthesis highlights
(use-package highlight-parentheses
  :init
  (setq hl-paren-colors '("gold" "IndianRed" "cyan" "green" "orange" "magenta")))

(defun hpm-on ()
  (highlight-parentheses-mode t))
(add-hook 'ess-mode-hook 'hpm-on)
(add-hook 'inferior-ess-mode-hook 'hpm-on)
(add-hook 'latex-mode-hook 'hpm-on)

;;; darken parentheses
(use-package paren-face
  :init (global-paren-face-mode))

(use-package indent-guide)

(use-package smart-mode-line
  :init
  (sml/setup)
  (sml/apply-theme 'dark))
;(sml/apply-theme 'light)
;(sml/apply-theme 'respectful)
;(sml/apply-theme 'automatic)
(add-to-list 'sml/replacer-regexp-list '("^~/.emacs.d/configs/" ":ED:") t)
(add-to-list 'sml/replacer-regexp-list '("^/media/Data/Bibliography" ":bib:") t)
(add-to-list 'sml/replacer-regexp-list '("^/media/Data/Dropbox/Latex" ":LaTeX:") t)
(add-to-list 'sml/replacer-regexp-list '("^/media/Data/Dropbox/R" ":R:") t)
(add-to-list 'sml/replacer-regexp-list '("^/media/Data/Dropbox/Private/org" ":org:") t)
(add-to-list 'sml/replacer-regexp-list '("^/media/Data/Dropbox" ":dropbox:") t)

(use-package company
  :config
  (progn
    (add-hook 'after-init-hook 'global-company-mode)))

(use-package company-auctex
  :init (company-auctex-init)
  :config
  (progn
    (add-to-list 'company-backend 'company-math-symbols-unicode)
    (setq company-tooltip-align-annotations t)))

(use-package writegood-mode
  :config
  (progn
    (global-set-key "\C-cs" 'writegood-mode)
    (global-set-key "\C-c\C-gg" 'writegood-grade-level)
    (global-set-key "\C-c\C-ge" 'writegood-reading-ease)))
;(writegood-mode 1)
;;; Style-check
; source http://www.cs.umd.edu/~nspring/software/style-check-readme.html
(defun my-action/style-check-file ()
     (interactive)
    (compile (format "style-check.rb -v %s" (buffer-file-name))))
(global-set-key "\C-c\C-gs" 'my-action/style-check-file)

(global-set-key (kbd "C-h C-m") 'discover-my-major)

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

(quietly-read-abbrev-file)
(setq-default abbrev-mode t)
(setq save-abbrevs t)
(add-hook 'text-mode-hook (lambda () (abbrev-mode 1)))

(use-package flycheck
  :bind (("M-g M-n" . flycheck-next-error)
         ("M-g M-p" . flycheck-previous-error)
         ("M-g M-=" . flycheck-list-errors))
  :idle (global-flycheck-mode)
  :diminish "fc"
  :config
  (progn
    (setq-default flycheck-disabled-checkers
                  '(emacs-lisp-checkdoc))
    (use-package flycheck-tip
      :config
      (add-hook 'flycheck-mode-hook
                (lambda ()
                  (global-set-key (kbd "C-c C-n") 'flycheck-tip-cycle)
                  (global-set-key (kbd "C-c C-p") 'flycheck-tip-cycle-reverse))))))

(use-package highlight-symbol
  :bind (("M-n" . highlight-symbol-next)
         ("M-p" . highlight-symbol-prev)))
;;; highlight numbers
(use-package highlight-numbers
  :init (add-hook 'prog-mode-hook 'highlight-numbers-mode))
;;; highight escape sequences
(use-package highlight-escape-sequences
  :init (add-to-list 'prog-mode-hook (lambda () (hes-mode t))))

(use-package easy-kill
  :init (global-set-key [remap kill-ring-save] 'easy-kill))

(use-package helm
  :bind
  (("C-M-z" . helm-resume)
   ("C-c x" . helm-bibtex)
   ("M-y" . helm-show-kill-ring)
   ("C-h b" . helm-descbinds)
   ;;("C-x C-r" . helm-mini)
   ;;("C-x M-o" . helm-occur)
   ("C-x C-o" . helm-occur)
   ("C-h a" . helm-apropos)
   ("C-h m" . helm-man-woman)
   ("M-g >" . helm-ag-this-file)
   ("M-g ," . helm-ag-pop-stack)
   ("M-g ." . helm-do-grep)
   ("C-c g" . helm-do-ag)
   ("C-x C-i" . helm-semantic-or-imenu)
   ("M-x" . helm-M-x)
   ;;("C-x C-b" . helm-buffers-list)
   ;;("C-x b" . helm-buffers-list)
   ("C-x C-f" . helm-find-files)
   ("C-x b" . helm-mini)
   ("C-h t" . helm-world-time)
   ("C-h s" . helm-simple-call-tree))
  ;;:idle (helm-mode 1)
  :config
  (progn
    (use-package helm-config)
    (use-package helm-files)
    (use-package helm-R)
    (use-package helm-bibtex)
    (use-package helm-anything)
    (use-package helm-grep)
    (use-package helm-man)
    (use-package helm-misc)
    (use-package helm-aliases)
    (use-package helm-elisp)
    (use-package helm-imenu)
    (use-package helm-semantic)
    (use-package helm-ring)
    (use-package helm-bookmark
      :bind (("C-x M-b" . helm-bookmarks)))
    (use-package helm-projectile
      :bind (("C-x f" . helm-projectile)
             ;("C-c p f" . helm-projectile-find-file)
             ))
    (use-package helm-eshell
      :init (add-hook 'eshell-mode-hook
                      (lambda ()
                        (define-key eshell-mode-map (kbd "M-l")
                          'helm-eshell-history))))
    (setq helm-idle-delay 0.1
          helm-exit-idle-delay 0.1
          helm-input-idle-delay 0
          helm-candidate-number-limit 500
          helm-buffers-fuzzy-matching t
          helm-truncate-lines t
          helm-grep-default-command
          ;;"ggrep -a -d skip %e -n%cH -e %p %f"
          "grep -a -d skip %e -n%cH -e %p %f"
          helm-grep-default-recurse-command
          ;;"ggrep -a -d recurse %e -n%cH -e %p %f"
          "grep -a -d recurse %e -n%cH -e %p %f"
          )
    (setq helm-bibtex-bibliography '(
                                     "/media/Data/Bibliography/deeplearninggpu2014.bib"
                                     "/media/Data/Bibliography/humanGenetics.bib"))
    (setq helm-bibtex-library-path "/media/Data/Bibliography/Bibliography2017/"
          helm-bibtex-notes-path "/media/Data/Bibliography/notes/"
          helm-bibtex-pdf-symbol "P")
    (setq helm-bibtex-pdf-open-function    ;; Open PDF in Evince
      (lambda (fpath) (shell-command-to-string
                       (concat "/usr/bin/evince " fpath " &"))))
    (setq display-time-world-list '(("America/Vermont" "Vermont")
                                    ("America/Denver" "Denver")
                                    ("EST5EDT" "Boston")
                                    ("Europe/France" "France")
                                    ("Europe/Germany" "Denver")
                                    ("Europe/London" "London")
                                    ("Europe/Amsterdam" "Amsterdam")
                                    ("Asia/Tokyo" "Tokyo")
                                    ("Australia/Sydney" "Sydney")))
    (define-key helm-map (kbd "C-p")   'helm-previous-line)
    (define-key helm-map (kbd "C-n")   'helm-next-line)
    (define-key helm-map (kbd "C-M-n") 'helm-next-source)
    (define-key helm-map (kbd "C-M-p") 'helm-previous-source)

    ;; Start of not-my-config stuff
    ;; taken from https://tuhdo.github.io/helm-intro.html#sec-6
    (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to do persistent action
    (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
    (define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

    (define-key helm-grep-mode-map (kbd "<return>")  'helm-grep-mode-jump-other-window)
    (define-key helm-grep-mode-map (kbd "n")  'helm-grep-mode-jump-other-window-forward)
    (define-key helm-grep-mode-map (kbd "p")  'helm-grep-mode-jump-other-window-backward)

    (setq helm-google-suggest-use-curl-p t
          helm-scroll-amount 4 ; scroll 4 lines other window using M-<next>/M-<prior>
          helm-quick-update t ; do not display invisible candidates
          helm-idle-delay 0.01 ; be idle for this many seconds, before updating in delayed sources.
          helm-input-idle-delay 0.01 ; be idle for this many seconds, before updating candidate buffer
          helm-ff-search-library-in-sexp t ; search for library in `require' and `declare-function' sexp.

          ;; you can customize helm-do-grep to execute ack-grep
          ;; helm-grep-default-command "ack-grep -Hn --smart-case --no-group --no-color %e %p %f"
          ;; helm-grep-default-recurse-command "ack-grep -H --smart-case --no-group --no-color %e %p %f"
          helm-split-window-default-side 'other ;; open helm buffer in another window
          helm-split-window-in-side-p t ;; open helm buffer inside current window, not occupy whole other window
          helm-buffers-favorite-modes (append helm-buffers-favorite-modes
                                              '(picture-mode artist-mode))
          helm-candidate-number-limit 200 ; limit the number of displayed canidates
          helm-M-x-requires-pattern 0     ; show all candidates when set to 0
          helm-boring-file-regexp-list
          '("\\.git$" "\\.hg$" "\\.svn$" "\\.CVS$" "\\._darcs$" "\\.la$" "\\.o$" "\\.i$") ; do not show these files in helm buffer
          helm-ff-file-name-history-use-recentf t
          helm-move-to-line-cycle-in-source t ; move to end or beginning of source
                                        ; when reaching top or bottom of source.
          ido-use-virtual-buffers t     ; Needed in helm-buffers-list
          helm-buffers-fuzzy-matching t ; fuzzy matching buffer names when non--nil
                                        ; useful in helm-mini that lists buffers
          )

    (define-key helm-map (kbd "C-x 2") 'helm-select-2nd-action)
    (define-key helm-map (kbd "C-x 3") 'helm-select-3rd-action)
    (define-key helm-map (kbd "C-x 4") 'helm-select-4rd-action)

    ;; helm-mini instead of recentf
    (global-set-key (kbd "C-c h s") 'helm-semantic-or-imenu)
    (global-set-key (kbd "C-c h m") 'helm-man-woman)
    (global-set-key (kbd "C-c h g") 'helm-do-grep)
    (global-set-key (kbd "C-c h f") 'helm-find)
    (global-set-key (kbd "C-c h l") 'helm-locate)
    (global-set-key (kbd "C-c h o") 'helm-occur)
    (global-set-key (kbd "C-c h r") 'helm-resume)
    (define-key 'help-command (kbd "C-f") 'helm-apropos)
    (define-key 'help-command (kbd "r") 'helm-info-emacs)

    ;; use helm to list eshell history
    (add-hook 'eshell-mode-hook
              #'(lambda ()
                  (define-key eshell-mode-map (kbd "M-l")  'helm-eshell-history)))

    ;; Save current position to mark ring
    (add-hook 'helm-goto-line-before-hook 'helm-save-current-pos-to-mark-ring)

    (defvar helm-httpstatus-source
      '((name . "HTTP STATUS")
        (candidates . (("100 Continue") ("101 Switching Protocols")
                       ("102 Processing") ("200 OK")
                       ("201 Created") ("202 Accepted")
                       ("203 Non-Authoritative Information") ("204 No Content")
                       ("205 Reset Content") ("206 Partial Content")
                       ("207 Multi-Status") ("208 Already Reported")
                       ("300 Multiple Choices") ("301 Moved Permanently")
                       ("302 Found") ("303 See Other")
                       ("304 Not Modified") ("305 Use Proxy")
                       ("307 Temporary Redirect") ("400 Bad Request")
                       ("401 Unauthorized") ("402 Payment Required")
                       ("403 Forbidden") ("404 Not Found")
                       ("405 Method Not Allowed") ("406 Not Acceptable")
                       ("407 Proxy Authentication Required") ("408 Request Timeout")
                       ("409 Conflict") ("410 Gone")
                       ("411 Length Required") ("412 Precondition Failed")
                       ("413 Request Entity Too Large")
                       ("414 Request-URI Too Large")
                       ("415 Unsupported Media Type")
                       ("416 Request Range Not Satisfiable")
                       ("417 Expectation Failed") ("418 I'm a teapot")
                       ("422 Unprocessable Entity") ("423 Locked")
                       ("424 Failed Dependency") ("425 No code")
                       ("426 Upgrade Required") ("428 Precondition Required")
                       ("429 Too Many Requests")
                       ("431 Request Header Fields Too Large")
                       ("449 Retry with") ("500 Internal Server Error")
                       ("501 Not Implemented") ("502 Bad Gateway")
                       ("503 Service Unavailable") ("504 Gateway Timeout")
                       ("505 HTTP Version Not Supported")
                       ("506 Variant Also Negotiates")
                       ("507 Insufficient Storage") ("509 Bandwidth Limit Exceeded")
                       ("510 Not Extended")
                       ("511 Network Authentication Required")))
        (action . message)))

    (defvar helm-clj-http-source
      '((name . "clj-http options")
        (candidates
         .
         ((":accept - keyword for content type to accept")
          (":as - output coercion: :json, :json-string-keys, :clojure, :stream, :auto or string")
          (":basic-auth - string or vector of basic auth creds")
          (":body - body of request")
          (":body-encoding - encoding type for body string")
          (":client-params - apache http client params")
          (":coerce - when to coerce response body: :always, :unexceptional, :exceptional")
          (":conn-timeout - timeout for connection")
          (":connection-manager - connection pooling manager")
          (":content-type - content-type for request")
          (":cookie-store - CookieStore object to store/retrieve cookies")
          (":cookies - map of cookie name to cookie map")
          (":debug - boolean to print info to stdout")
          (":debug-body - boolean to print body debug info to stdout")
          (":decode-body-headers - automatically decode body headers")
          (":decompress-body - whether to decompress body automatically")
          (":digest-auth - vector of digest authentication")
          (":follow-redirects - boolean whether to follow HTTP redirects")
          (":form-params - map of form parameters to send")
          (":headers - map of headers")
          (":ignore-unknown-host? - whether to ignore inability to resolve host")
          (":insecure? - boolean whether to accept invalid SSL certs")
          (":json-opts - map of json options to be used for form params")
          (":keystore - file path to SSL keystore")
          (":keystore-pass - password for keystore")
          (":keystore-type - type of SSL keystore")
          (":length - manually specified length of body")
          (":max-redirects - maximum number of redirects to follow")
          (":multipart - vector of multipart options")
          (":oauth-token - oauth token")
          (":proxy-host - hostname of proxy server")
          (":proxy-ignore-hosts - set of hosts to ignore for proxy")
          (":proxy-post - port for proxy server")
          (":query-params - map of query parameters")
          (":raw-headers - boolean whether to return raw headers with response")
          (":response-interceptor - function called for each redirect")
          (":retry-handler - function to handle HTTP retries on IOException")
          (":save-request? - boolean to return original request with response")
          (":socket-timeout - timeout for establishing socket")
          (":throw-entire-message? - whether to throw the entire response on errors")
          (":throw-exceptions - boolean whether to throw exceptions on 5xx & 4xx")
          (":trust-store - file path to trust store")
          (":trust-store-pass - password for trust store")
          (":trust-store-type - type of trust store")))
        (action . message)))

    (defun helm-httpstatus ()
      (interactive)
      (helm-other-buffer '(helm-httpstatus-source) "*helm httpstatus*"))

    (defun helm-clj-http ()
      (interactive)
      (helm-other-buffer '(helm-clj-http-source) "*helm clj-http flags*"))

    (global-set-key (kbd "C-c M-C-h") 'helm-httpstatus)
    (global-set-key (kbd "C-c M-h") 'helm-clj-http)

    (use-package helm-descbinds
      :init (helm-descbinds-mode t))
    (use-package helm-ag
      :bind ("C-M-s" . helm-ag-this-file))
    (use-package helm-swoop
      :bind (("M-i" . helm-swoop)
             ("M-I" . helm-swoop-back-to-last-point)
             ("C-c M-i" . helm-multi-swoop))
      :config
      (progn
        ;; When doing isearch, hand the word over to helm-swoop
        (define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
        ;; From helm-swoop to helm-multi-swoop-all
        (define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)
        ;; Save buffer when helm-multi-swoop-edit complete
        (setq helm-multi-swoop-edit-save t
              ;; If this value is t, split window inside the current window
              helm-swoop-split-with-multiple-windows nil
              ;; Split direcion. 'split-window-vertically or 'split-window-horizontally
              helm-swoop-split-direction 'split-window-vertically
              ;; If nil, you can slightly boost invoke speed in exchange for text color
              helm-swoop-speed-or-color nil)))))

(use-package golden-ratio
  :diminish golden-ratio-mode
  :defer t)

(use-package ess-site
  :config
  (progn
   (put 'upcase-region 'disabled nil)
   (add-hook 'inferior-ess-mode-hook
             '(lambda nil
                (define-key inferior-ess-mode-map [\C-up]
                  'comint-previous-matching-input-from-input)
                (define-key inferior-ess-mode-map [\C-down]
                  'comint-next-matching-input-from-input)
                (define-key inferior-ess-mode-map [\C-x \t]
                  'comint-dynamic-complete-filename)
     ))))

(use-package AUCTeX
  :config
  (progn
    (setq TeX-parse-self t ; Enable parse on load.
          TeX-auto-save t ; Enable parse on save
          TeX-auto-untabify t ; convert tab to spaces (Parsing Files section of the manual)
          tex-dvi-view-command "xdvi"
          reftex-plug-into-AUCTeX t)
    (setq-default TeX-master nil)
    (add-hook 'LaTeX-mode-hook 'visual-line-mode)
    (add-hook 'LaTeX-mode-hook 'flyspell-mode)
    (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
    (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
    ; Table of content activation in menubar
    (add-hook 'reftex-load-hook 'imenu-add-menubar-index)
    (add-hook 'reftex-mode-hook 'imenu-add-menubar-index)
    ))

;(add-hook 'LaTeX-mode-hook
;       (lambda ()
;               (setq TeX-master (guess-TeX-master (buffer-file-name)))
;               (auto-fill-mode -1)
;               (setq fill-column 2000)
;               (flyspell-mode -1)
;))

(use-package expand-region
  :bind (("C-=" . er/expand-region)
         ("C-M-=" . er/contract-region)))

(global-set-key "\C-xrs" 'bookmark-save)
(global-set-key "\C-cc" 'reftex-citation)
(global-set-key "\C-cl" 'org-store-link)        ; used in combination w/ Cc Cl 
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
;;(global-set-key (kbd "C-c m") 'mu4e)  ;; email
(global-set-key (kbd "C-c p") 'speedbar)  ;; speedbar
(global-set-key (kbd "C-c e") 'ecb-activate)  ;; ECB
(global-set-key (kbd "M-<up>") 'move-line-up)
(global-set-key (kbd "M-<down>") 'move-line-down)
(global-set-key [f11] 'fullscreen)

(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)
(global-set-key [down-mouse-3] 'imenu)  ; TOC activation right-mouse click
(global-set-key (kbd "C-=") 'er/expand-region)
;(global-set-key (kbd "<escape>") 'god-mode-all)
(global-set-key (kbd "C-x C-1") 'delete-other-windows)
(global-set-key (kbd "C-x C-2") 'split-window-below)
(global-set-key (kbd "C-x C-3") 'split-window-right)
(global-set-key (kbd "C-x C-0") 'delete-window)

(use-package markdown-mode
  :config
  (progn
    (define-key markdown-mode-map (kbd "C-M-f") 'forward-symbol)
    (define-key markdown-mode-map (kbd "C-M-b") 'backward-symbol)
    (define-key markdown-mode-map (kbd "C-M-u") 'my/backward-up-list)

    (define-key markdown-mode-map (kbd "C-c C-n") 'outline-next-visible-heading)
    (define-key markdown-mode-map (kbd "C-c C-p") 'outline-previous-visible-heading)
    (define-key markdown-mode-map (kbd "C-c C-f") 'outline-forward-same-level)
    (define-key markdown-mode-map (kbd "C-c C-b") 'outline-backward-same-level)
    (define-key markdown-mode-map (kbd "C-c C-u") 'outline-up-heading)

    (defvar markdown-imenu-generic-expression
      '(("title"  "^\\(.+?\\)[\n]=+$" 1)
        ("h2-"    "^\\(.+?\\)[\n]-+$" 1)
        ("h1"     "^#\\s-+\\(.+?\\)$" 1)
        ("h2"     "^##\\s-+\\(.+?\\)$" 1)
        ("h3"     "^###\\s-+\\(.+?\\)$" 1)
        ("h4"     "^####\\s-+\\(.+?\\)$" 1)
        ("h5"     "^#####\\s-+\\(.+?\\)$" 1)
        ("h6"     "^######\\s-+\\(.+?\\)$" 1)
        ("fn"     "^\\[\\^\\(.+?\\)\\]" 1) ))))

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

(setq py-install-directory "~/.emacs.d/python-mode.el-6.1.3/")
  (add-to-list 'load-path py-install-directory)
  (use-package python-mode)

;; Browse the Python Documentation using Info (C-h S)
(use-package info-look)

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

(when (eq window-system 'x)
  ;; Font family
  (set-fontset-font "fontset-default" 'symbol "Inconsolata")
  (set-default-font "Inconsolata")
  ;; Font size
  (set-face-attribute 'default nil :height 113))