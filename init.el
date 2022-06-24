(require 'package)

;; Set up package archives and make sure 'Use Package' is installed
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)

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
(require 'use-package)
(setq use-package-always-ensure t)

;;; LOOKS
;;; --------------------------------------------------------------------------

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
 kill-whole-line nil
 lisp-backquote-indentation nil
 blink-cursor-blinks 1
 fast-but-imprecise-scrolling t
 auto-save-interval 60
 kill-do-not-save-duplicates t)

(when (string-greaterp emacs-version "28")
  (repeat-mode 1))

(show-paren-mode)

;;; CUSTOM FUNCTIONS
;;; Here are some custom functions I find useful.
;;; If you decide you do not want to use them then remember to delete their key mappings
;;; in the (use-package emacs..) section.
;;; --------------------------------------------------------------------------
;;;
;;; By default emacs uses 'C-k' to kill (cut) a line and 'C-w' to kill (cut)
;;; current region. Similarly 'M-w' will copy active region. I find that having
;;; two keybinds to do similar operations annoying and so `me/kill-dwim',
;;; `me/copy-dwim' bundles these two opterations into one picks the correct one.
;;; C-w is still useful to kill between point and mark.
(defun me/if-active-region-else (then else)
  (if (region-active-p)
      (funcall then (region-beginning) (region-end))
    (funcall else)))

(defun me/kill-dwim ()
  "Run the command `kill-region' on the current region
or `kill-line' if there is no active region'"
  (interactive)
  (me/if-active-region-else #'kill-region #'kill-line))

(defun me/copy-dwim ()
  "Run the command `kill-ring-save' on the current region
or the current line if there is no active region."
  (interactive)
  (me/if-active-region-else
   #'kill-ring-save
   (lambda () (kill-ring-save (point-at-bol) (point-at-eol) nil))))
;; --------------------------------------------------------------------------

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

;;; BUILT-IN PACKAGES
;;; --------------------------------------------------------------------------

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
	("M-o" . me/open-line-above)
	("C-o" . me/open-line-below)
	("C-k" . me/kill-dwim)
	("M-w" . me/copy-dwim)
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
(use-package eglot
  :custom
  (eglot-autoshutdown t)
  (eglot-autoreconnect nil)
  (eglot-confirm-server-initiated-edits nil)
  (eldoc-idle-delay 1)
  (eldoc-echo-area-display-truncation-message nil)
  
  ;; Adding hooks to automatically start an eglot (LSP) session when a buffer is visited.
  ;; See https://github.com/joaotavora/eglot
  
  ; :hook
  ; (haskell-mode . eglot-ensure) 
  ; (rust-mode . eglot-ensure)
  
  :bind (:map eglot-mode-map
	      ("C-c C-a" . eglot-code-actions)
	      ("C-c C-f" . eglot-format-buffer)))

;; Editing
;; --------------------------------------------------------------------------
(use-package multiple-cursors
  :config
  (global-set-key [(super down)] 'mc/mark-next-like-this)
  :bind
  (:map global-map
	("s-<down>" . mc/mark-next-like-this)
	("s-," . mc/mark-all-in-region-regexp)))

;;; Expand region lets you incrementally expand a region
;;; See: https://github.com/magnars/expand-region.el
;; (use-package expand-region
;;   :bind
;;   ("C-<return>" . er/expand-region))

;;; Enables structural editing for LISP.
;;; See: http://danmidwood.com/content/2014/11/21/animated-paredit.html
(use-package paredit
  :diminish
  :hook ((emacs-lisp-mode lisp-mode) . enable-paredit-mode))

;; Completion
;; --------------------------------------------------------------------------

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

;; Completion framework
;; See: https://github.com/minad/vertico
(use-package vertico
  :config
  (vertico-mode))

;; Fuzzy matching
;; See: https://github.com/minad/orderless
(use-package orderless
  :init
  (setq completion-styles '(orderless flex)
	completion-category-defaults nil
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
