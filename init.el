(require 'package)

;; Set up package archives and make sure 'Use Package' is installed
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; The Use Package package provides a macro for easy package configuration
(require 'use-package)
(setq use-package-always-ensure t)

;; Fonts
;; There are prettier ways to set font but this way is fast and doesn't cause re-rendering.
(add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono-12"))

;; Theme
(load-theme 'wombat t)

;; Sane defaults
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(setq-default
 fill-column 80
 sentence-end-double-space nil
 kill-whole-line t
 lisp-backquote-indentation nil
 blink-cursor-blinks 1
 fast-but-imprecise-scrolling t
 auto-save-interval 60
 kill-do-not-save-duplicates t)

;; Custom functions

;; Never kill scratch-buffer
(defun me/bury-scratch-buffer ()
  (if (string= (buffer-name) "*scratch*")
      (ignore (bury-buffer))
    t))
(add-hook 'kill-buffer-query-functions 'me/bury-scratch-buffer)

(defun me/back-to-mark ()
  (interactive)
  (set-mark-command 0))

(defun me/kill-dwim ()
  (interactive)
  (if (region-active-p)
      (kill-region (region-beginning) (region-end) nil)
    (kill-line)))

(defun me/kill-current-buffer ()
  "Kill current buffer."
  (interactive)
  (kill-buffer (current-buffer)))

(defun me/dired-up-directory ()
  "Move up directory and cleanup previously used buffer."
  (interactive)
  (let ((cb (current-buffer)))
    (progn (dired-up-directory)
	   (kill-buffer cb))))

(defun me/copy-dwim ()
  "Run the command `kill-ring-save' on the current region
or the current line if there is no active region."
  (interactive)
  (if (region-active-p)
      (kill-ring-save nil nil t)
    (kill-ring-save (point-at-bol) (point-at-eol))))

(defun me/open-line-below ()
  "Open a newline below current line."
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent))

(defun me/open-line-above ()
  "Open a newline above current line."
  (interactive)
  (previous-line)
  (move-end-of-line nil)
  (newline-and-indent))

;; Built-in packages - Misc
(use-package emacs
  ;; :map some-mode-map body
  :bind 
  (:map global-map
	("C-8" . backward-list)
	("C-9" . forward-list)
	("M-1" . delete-other-windows)
	("M-2" . split-window-below)
	("M-3" . split-window-right)
	("M-4" . delete-window)
	("s-r" . replace-string)
	("M-z" . zap-up-to-char)
	("M-o" . me/open-line-above)
	("C-o" . me/open-line-below)
	("C-k" . me/kill-dwim)
	("C-x k" . me/kill-current-buffer)
	("C-0" . me/back-to-mark)))

(use-package dired
  :ensure nil
  :config
  (setq dired-recursive-copies t
	dired-recursive-deletes t
	dired-dwim-target t
	delete-by-moving-to-trash t)
  :bind*			    
  (:map dired-mode-map
	("-" . me/dired-up-directory)
	("e" . wdired-change-to-wdired-mode)))

;; Built-in packages - Languages
(use-package sh-mode
  :ensure nil
  :commands shell-script-mode
  :bind (:map sh-mode-map ("C-x C-e" . sh-execute-region)))

;; External packages - misc
(use-package org
  :commands (org-agenda
	     org-capture)

  :config
  (setq org-startup-indented t
	org-startup-with-latex-preview t
	org-pretty-entities t
	org-ellipsis " …"
  	org-export-preserve-breaks t
	org-highlight-latex-and-related '(native)
	org-src-fontify-natively t
	org-fontify-quote-and-verse-blocks t
	org-startup-folded nil
	org-hide-leading-stars t
	org-cycle-separator-lines -1
	org-catch-invisible-edits 'error
	org-ctrl-k-protect-subtree t)
  :bind (:map org-mode-map
	      ("C-c h" . consult-org-heading)))


;; External packages - languages
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode))
  :init
  (setq markdown-command "multimarkdown") ;; Requires multimarkdown to be installed on your system
  :custom
  (markdown-enable-highlighting-syntax t))

;; LSP
(use-package eglot
  :defer t
  :hook
  (haskell-mode . eglot-ensure)
  (c-mode . eglot-ensure)
  (python-mode . eglot-ensure)
  (LaTeX-mode . eglot-ensure)
  :custom
  (eglot-autoshutdown t)
  (eglot-autoreconnect nil)
  (eglot-confirm-server-initiated-edits nil)
  (eldoc-idle-delay 1)
  (eldoc-echo-area-display-truncation-message nil)

  :bind (:map eglot-mode-map
	      ("C-c C-a" . eglot-code-actions)
	      ("C-c C-f" . eglot-format-buffer)))

;; External packages - editing
(use-package multiple-cursors
  :config
  (global-set-key (kbd "M-m") 'mc/mark-all-like-this-dwim)
  (global-set-key [(super down)] 'mc/mark-next-like-this)
  :bind
  (:map global-map
	("s-<down>" . mc/mark-next-like-this)
	("s-," . mc/mark-all-in-region-regexp)))

(use-package expand-region
  :bind
  ("C-<return>" . er/expand-region))

(use-package paredit
  :diminish
  :hook ((emacs-lisp-mode lisp-mode) . enable-paredit-mode))

;; Completion
(use-package corfu
  :defer t
  :custom
  (corfu-auto-delay 0.2)
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-commit-predicate nil)
  (corfu-quit-at-boundary t)
  (corfu-quit-no-match t)
  (corfu-echo-documentation nil)
  :config
  (corfu-mode)

  :init
  ;; See also `corfu-excluded-modes'.
  (global-corfu-mode))

;; Completion framework
(use-package vertico
  :config
  (vertico-mode))

;; Fuzzy matching
(use-package orderless
  :init
  (setq completion-styles '(orderless)
	completion-category-defaults nil
	orderless-skip-highlighting t
	completion-category-overrides '((file (styles partial-completion)))))

;; Minibuffer completion
(use-package consult
  :config
  (setq consult-preview-key nil)
  (recentf-mode)
  :bind
  ("C-c r" . consult-recent-file)
  ("C-c f" . consult-ripgrep)
  ("C-c l" . consult-line)
  ("C-c i" . consult-imenu)
  ("C-x b" . consult-buffer))
