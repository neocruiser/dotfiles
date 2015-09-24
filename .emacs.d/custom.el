
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-view-program-selection (quote ((output-pdf "Evince") ((output-dvi style-pstricks) "dvips and gv") (output-dvi "xdvi") (output-html "xdg-open"))))
 '(blink-cursor-mode nil)
 '(bmkp-last-as-first-bookmark-file "~/.emacs.d/bookmarks")
 '(browse-url-browser-function (quote browse-url-firefox))
 '(column-number-mode t)
 '(custom-safe-themes (quote ("a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "3a727bdc09a7a141e58925258b6e873c65ccf393b2240c51553098ca93957723" "756597b162f1be60a12dbd52bab71d40d6a2845a3e3c2584c6573ee9c332a66e" "6a37be365d1d95fad2f4d185e51928c789ef7a4ccf17e7ca13ad63a8bf5b922f" default)))
 '(diredp-hide-details-initially-flag nil t)
 '(fringe-mode 0 nil (fringe))
 '(helm-adaptive-history-length 250)
 '(helm-bibtex-format-citation-functions (quote ((org-mode . helm-bibtex-format-citation-org-link-to-PDF) (latex-mode . helm-bibtex-format-citation-cite) (markdown-mode . helm-bibtex-format-citation-pandoc-citeproc) (default . helm-bibtex-format-citation-default))))
 '(helm-mode t)
 '(inhibit-startup-echo-area-message nil)
 '(markdown-command "/home/neocruiser/Markdown_1.0.1/Markdown.pl")
 '(menu-bar-mode nil)
 '(nxhtml-autoload-web nil t)
 '(safe-local-variable-values (quote ((require-final-newline))))
 '(save-place t nil (saveplace))
 '(show-paren-mode t)
 '(size-indication-mode t)
 '(tool-bar-mode nil)
 '(yas-prompt-functions (quote (my-yas/prompt))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "DejaVu Sans Mono" :foundry "unknown" :slant normal :height 98 :width normal :weight bold))))
 '(cursor ((t (:background "tan" :foreground "black"))))
 '(flyspell-duplicate ((t (:foreground "Gold3"))))
 '(flyspell-incorrect ((t (:foreground "light salmon"))))
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
