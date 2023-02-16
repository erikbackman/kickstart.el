(require 'package)

;; Set up package archives.
;; Whenever you want to update your packages use 'M-x package-refresh-contents' followed by
;; 'M-x package-update' or 'M-x package-update-all'.
;;
;; 'M-x' means holding 'Alt' (or Command on MacOS) and pressing 'x'
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; If you are using Emacs version 29.60.0 or higher then 'use-package'
;; is built-in and you don't need the following lines:
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; README!
;;
;; The 'use-package' package provides a macro for easy package configuration and
;; deferred loading.
;;
;; I highly recommend you read its documentation as it will explain what keys
;; such as :init, :config and :custom will expand to. You can read the
;; documentation for use package directly from within emacs! Use 'C-h i' to
;; access the info system and 'm' to navigate to the use-package section.
;; I suggest you try this now as I will keep referring to the manual throughout this file.
(require 'use-package)
(setq use-package-always-ensure t)

;;; LOOKS
;;; --------------------------------------------------------------------------

;; Fonts
;; There are prettier ways to set font but this way is fast and doesn't cause re-rendering.
(add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono-12"))

;; Theme
(load-theme 'wombat t)
;; You can use M-x consult-theme to try other themes.

;; Sane defaults
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(setq-default
 fill-column 80
 sentence-end-double-space nil
 kill-whole-line nil
 lisp-backquote-indentation nil
 blink-cursor-blinks 1
 fast-but-imprecise-scrolling t
 auto-save-interval 60
 kill-do-not-save-duplicates t
 bidi-paragraph-direction 'left-to-right
 bidi-inhibit-bpa t)

;; 'repeat' sets up keymaps for repeating various commands without having to press
;; the entire key sequence again. For example when cycling windows using 'C-x o',
;; to select the next window you can simply keep pressing 'o'.
;; Fore more information use M-x describe-package
;;
;; 'Use-package' also supports setting up custom repeat keymaps. See the use-package manaual or evaluate:
;; use-package > Configuring Packages > Key bindings > Binding to repeat-maps
;; To go back to the top node from within an info-buffer, press 'd'.
(when (version<= "28" emacs-version)
  (repeat-mode 1))

;; This should not be needed for Emacs version 29.0.60 and higher.
(unless (version<= "29.0.60" emacs-version)
  (global-so-long-mode 1))

;; Highlight matching parentheses.
(show-paren-mode 1)

;;; CUSTOM FUNCTIONS
;;; --------------------------------------------------------------------------
;; Never kill scratch-buffer.
(defun me/bury-scratch-buffer ()
  (if (string= (buffer-name) "*scratch*")
      (ignore (bury-buffer))
    t))
(add-hook 'kill-buffer-query-functions 'me/bury-scratch-buffer)

;; You can achieve what this function does by pressing C-u C-SPC
(defun me/back-to-mark ()
  "Jump back to previous mark i.e after searching."
  (interactive)
  (set-mark-command 0))

(defun me/dired-up-directory ()
  "Move up directory and cleanup previously used buffer."
  (interactive)
  (let ((cb (current-buffer)))
    (progn (dired-up-directory)
	   (kill-buffer cb))))

;;; BUILT-IN PACKAGES
;;; --------------------------------------------------------------------------

;; Note that use-package is NOT a package manager but simply uses the one
;; provided by package.el when told to ensure that packages are installed. Here
;; we simply use the use-package macro to configure built-in packages in a
;; declerative manner.
(use-package emacs
  :bind
  ;; See: https://www.gnu.org/software/emacs/manual/html_node/elisp/Dotted-Pair-Notation.html
  ;; for information on "dotted pair notation" i.e (foo . bar)
  ;; or press 'C-h i' (to access the info-system) then use 'm' to navigate to:
  ;; Elisp > Dotted Pair Notation
  (:map global-map
	("C-8" . backward-list)
	("C-9" . forward-list)
	("M-1" . delete-other-windows)
	("M-2" . split-window-below)
	("M-3" . split-window-right)
	("M-4" . delete-window)
	("s-r" . replace-string)
	("M-z" . zap-up-to-char)
	("C-x C-b" . ibuffer-other-window)
	("C-x k" . kill-current-buffer)
	("C-0" . me/back-to-mark)))

;; Emacs Directory Editor 'C-x d'
(use-package dired
  :ensure nil
  :config
  (setq dired-recursive-copies t
	dired-recursive-deletes t
	dired-dwim-target t
	delete-by-moving-to-trash t)
  :bind
  (:map dired-mode-map
	("-" . me/dired-up-directory)
	("e" . wdired-change-to-wdired-mode)))

(use-package eshell
  :ensure nil
  :commands (eshell)
  :config
  (require 'esh-mode)
  (defun my/eshell-clear ()
    (interactive)
    (eshell/clear-scrollback)
    (eshell-emit-prompt))

  (defun eshell/open (file) (find-file file))
  :bind
  (:map eshell-mode-map
	("C-l" . my/eshell-clear)))

;; LANGUAGES
;; --------------------------------------------------------------------------
(use-package sh-mode
  :ensure nil
  :commands shell-script-mode
  :bind (:map sh-mode-map ("C-x C-e" . sh-execute-region)))

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


;;; EXTERNAL PACKAGES
;;; --------------------------------------------------------------------------
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode))
  :init
  (setq markdown-command "multimarkdown") ; Requires multimarkdown to be installed on your system
  :custom
  (markdown-enable-highlighting-syntax t))

;; LSP
;; --------------------------------------------------------------------------

;; Eglot is an LSP client. See https://github.com/joaotavora/eglot for more information.
;; Note that Eglot is built-in with Emacs version 29.0.60.
(use-package eglot
  :custom
  (eglot-autoshutdown t)
  (eglot-autoreconnect nil)
  (eglot-confirm-server-initiated-edits nil)
  (eldoc-idle-delay 1)
  (eldoc-echo-area-display-truncation-message nil)

  ;; To set up hooks to start an LSP-session automatically for certain modes you can use the ':hook' keyword followed by
  ;; dotted pairs of the form (major-mode . eglot-ensure), for example:
  ;; :hook
  ;; (haskell-mode . eglot-ensure)
  ;; (rust-mode . eglot-ensure)

  :bind (:map eglot-mode-map
	      ("C-c C-a" . eglot-code-actions)
	      ("C-c C-f" . eglot-format-buffer)))

;; Editing
;; --------------------------------------------------------------------------

;;; Enables structural editing for LISP.
;;; See:
;;; http://danmidwood.com/content/2014/11/21/animated-paredit.html
;;; https://github.com/joelittlejohn/paredit-cheatsheet
(use-package paredit
  :diminish
  :hook ((emacs-lisp-mode lisp-mode) . enable-paredit-mode))

;; Completion
;; --------------------------------------------------------------------------

;; Corfu: Enhances completion at point with a small completion popup.
;; See: https://github.com/minad/corfu
(use-package corfu
  :custom
  (corfu-auto-delay 0.2)
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-commit-predicate nil)
  (corfu-quit-at-boundary t)
  (corfu-quit-no-match t)
  (corfu-echo-documentation nil)
  :init
  (global-corfu-mode))

;; Vertico: Provides a performant and minimalistic vertical completion UI based on the default completion system.
;; See: https://github.com/minad/vertico
(use-package vertico
  :config
  (vertico-mode))

;; Fuzzy matching
;; Orderless: Emacs completion style that matches multiple regexps in any order
;; See: https://github.com/minad/orderless
(use-package orderless
  :init
  (setq completion-styles '(orderless flex)
	completion-category-defaults nil
	completion-category-overrides '((file (styles partial-completion)))))

;; Minibuffer completion
;; Consult: Provides search and navigation commands.
;; See: https://github.com/minad/consult
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
