(require 'package)

;; Set up package archives.
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
(setq package-native-compile t)
(setq use-package-always-ensure t)

;;; LOOKS
;;; --------------------------------------------------------------------------

;; Fonts
;; There are prettier ways to set font but this is fast and doesn't cause re-rendering.
(add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono-12"))

;; Theme
(load-theme 'wombat t)
;; You can use M-x consult-theme to try other themes.

;; Don't write custom settings to this file (when using M-x customize).
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; Some sane defaults
(setq dired-dwim-target t) ; Default other dired window when copying/moving files.
(setq dired-listing-switches "-alh")
;; Don't keep dired buffers around.
(setq dired-kill-when-opening-new-dired-buffer t)

;; Backups
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))


;; 'repeat' sets up keymaps for repeating various commands without having to press
;; the entire key sequence again. For example when cycling windows using 'C-x o',
;; to select the next window you can simply keep pressing 'o'.
;; Fore more information use M-x describe-package
;;
;; 'Use-package' also supports setting up custom repeat keymaps. See the 'use-package' manaual:
;; use-package > Configuring Packages > Key bindings > Binding to repeat-maps
;; To go back to the top node from within an info-buffer, press 'd'.
(when (version<= "28" emacs-version)
  (repeat-mode 1))

;; Highlight matching parentheses.
(show-paren-mode 1)

;; Global keybinds
(global-set-key (kbd "C-x k") . kill-current-buffer) ;; mapped to kill-buffer by default.
(global-set-key (kbd "M-z") . zap-up-to-char) ;; mapped to zap-to-char by default:

;;; --------------------------------------------------------------------------
;; Note that use-package is NOT a package manager but simply uses the one
;; provided by package.el when told to ensure that packages are installed. Here
;; we simply use the use-package macro to configure built-in packages in a
;; declerative manner. For example, we could configure the org useing `use-package', note that
;; this will cause org to be loaded only after you use any of the keybinds specified in the configuration
;; below.
(use-package org
  :custom
  (org-confirm-babel-evaluate nil)
  :bind
  (:map global-map
	("C-c n n" . #'org-capture)
	("C-c n a" . #'org-agenda)))


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
