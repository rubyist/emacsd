(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously
	 "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
	 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(make-directory "~/.emacs.d/autosaves/" t)
(setq
 backup-by-copying t
 backup-directory-alist
 '(("." . "~/.emacs.d/autosaves/"))
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t)

(setq auth-sources '(macos-keychain-internet))

(tooltip-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)
(setq initial-buffer-choice nil)
(setq frame-title-format "E M Λ C S")
(setq use-file-dialog nil)
(setq use-dialog-box nil)
(setq cursor-in-non-selected-windows nil)
(setq ring-bell-function 'ignore)

(defalias 'yes-or-no-p 'y-or-n-p)

(setq display-line-numbers-type t)
(global-display-line-numbers-mode nil)
(global-hl-line-mode)
(global-visual-line-mode)

(add-to-list 'initial-frame-alist '(width . (text-pixels . 1260)))
(add-to-list 'initial-frame-alist '(height . (text-pixels . 1920)))
(add-to-list 'initial-frame-alist '(top . 0))
(add-to-list 'initial-frame-alist '(left . 1280))

(unbind-key "C-z")
(unbind-key "C-<wheel-down>")
(unbind-key "C-<wheel-up>")
(unbind-key "C-<double-wheel-down>")
(unbind-key "C-<double-wheel-up>")
(unbind-key "C-<triple-wheel-down>")
(unbind-key "C-<triple-wheel-up>")

(use-package multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(global-set-key (kbd "C-M-j") 'mc/mark-all-dwim)

(electric-pair-mode)

(use-package exec-path-from-shell
  :init
  (setq exec-path-from-shell-variables
	'("PATH" "MANPATH" "SSH_AUTH_SOCK"))
  (exec-path-from-shell-initialize))

(use-package treemacs)

(use-package treemacs-projectile
  :after (treemacs projectile))

(use-package treemacs-magit
  :after (treemacs magit))

(use-package ace-window)
(global-set-key (kbd "M-o") 'ace-window)

(use-package which-key
  :init
  (which-key-mode))

(use-package vertico
  :init
  (vertico-mode)
  (setq vertico-cycle t))

(use-package corfu
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-separator ?\s)

  ; :hook ((prog-mode . corfu-mode))

  :init
  (corfu-global-mode))

(use-package orderless
  :ensure t
  :custom (completion-styles '(orderless)
			     completion-category-defaults nil
			     completion-category-overrides '((file (styles . partial-completion)))))

(use-package savehist
  :init
  (savehist-mode))

(use-package marginalia
  :init
  (marginalia-mode))

(use-package projectile
  :init
  (setq projectile-switch-project-action #'projectile-commander
	projectile-mode-line-function '(lambda () (format " %s" (projectile-project-name)))
	projectile-project-search-path '(("~/src" . 2)))
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  )

(use-package consult
  :bind (
	 ("C-c h" . consult-history)
	 ("C-c m" . consult-mode-command)
	 ("C-c k" . consult-kmacro)
	 ("C-x M-:" . consult-complex-command)
	 ("C-x b" . consult-buffer)
	 ("C-x 4 b" . consult-buffer-other-window)
	 ("C-x 5 b" . consult-buffer-other-frame)
	 ("C-x r b" . consult-bookmark)
	 ("C-x p b" . consult-project-buffer)
	 ("M-#" . consult-register-load)
	 ("M-'" . consult-register-store)
	 ("C-M-#" . consult-register)
	 ("M-y" . consult-yank-pop)
	 ("M-g e" . consult-compile-error)
	 ("M-g f" . consult-flymake)
	 ("M-g g" . consult-goto-line)
	 ("M-g M-g" . consult-goto-line)
	 ("M-g o" . consult-outline)
	 ("M-g m" . consult-mark)
	 ("M-g k" . consult-global-mark)
	 ("M-g i" . consult-imenu)
	 ("M-g I" . consult-imenu-multi)
	 ("M-s d" . consult-find)
	 ("M-s D" . consult-locate)
	 ("M-s g" . consult-grep)
	 ("M-s G" . consult-git-grep)
	 ("M-s r" . consult-ripgrep)
	 ("M-s l" . consult-line)
	 ("M-s L" . consult-line-multi)
	 ("M-s m" . consult-multi-occur)
	 ("M-s k" . consult-keep-lines)
	 ("M-s u" . consult-focus-lines)
	 ("M-s e" . consult-isearch-history)
	 :map isearch-mode-map
	 ("M-e" . consult-search-history)
	 ("M-s e" . consult-isearch-history)
	 ("M-s l" . consult-line)
	 ("M-s L" . consult-line-multi)
	 :map minibuffer-local-map
	 ("M-s" . consult-history)
	 ("M-r" . consult-history))

  :hook (completion-list-mode . consult-preview-at-point-mode)

  :init

  (setq register-preview-delay 0.5
	register-preview-function #'consult-register-format)

  (advice-add #'register-preview :override #'consult-register-window)
  (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)

  (setq xref-show-xrefs-function #'consult-xref
	xref-show-definitions-function #'consult-xref)

  :config
  (consult-customize
   consult-theme
   :preview-key '(:debounce 2.0 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-recent-file
   consult--source-project-recent-file
   :preview-key (kbd "M-."))

  (setq conulst-narrow-key "<")
  (autoload 'projectile-project-root "projectile")
  (setq consult-project-function (lambda (_) (projectile-project-root)))
  )

(use-package embark
  :bind
  (("C-." . embark-act)
   ("C-;" . embark-dwim)
   ("C-h B" . embark-bindings))

  :init
  (setq prefix-help-command #'embark-prefix-help-command)

  :config
  (add-to-list 'display-buffer-alist
	       '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
		 nil
		 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :after (embark consult))

(use-package emacs
  :init
  (defun crm-indicator (args)
	 (const (concat "[CRM] " (car args)) (cdr args)))
  (advice-add #'comleting-read-multiple :filter-args #'crm-indicator)

  (setq minibuffer-prompt-properties
	'(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
  (setq read-extended-command-predicate
	#'command-completion-default-include-p)
  (setq enable-recursive-minibuffers t)
  (setq completion-cycle-threshold 3)
  (setq tab-always-indent 'complete))

(use-package ripgrep)
(use-package wgrep
  :config
  (setq wgrep-auto-save-buffer t))

(use-package magit)
(remove-hook 'server-switch-hook 'magit-commit-diff)
(remove-hook 'magit-status-sections-hook 'magit-insert-unpushed-to-upstream-or-recent)

(use-package forge
  :after magit
  :bind
  (("C-c r" . code-review-forge-pr-at-point)))
(remove-hook 'magit-status-sections-hook 'forge-insert-issues)
(remove-hook 'magit-status-sections-hook 'forge-insert-pullreqs)

(use-package code-review
  :after magit)

(use-package git-gutter)
(global-git-gutter-mode +1)

(use-package git-timemachine)

(use-package flycheck
  :init (global-flycheck-mode))

(use-package yaml-mode)

(use-package elixir-mode)
(add-hook 'elixir-mode-hook
	  (lambda () (add-hook 'before-save-hook 'elixir-format nil t)))

(use-package mix
  :after elixir-mode
  :config
  (add-hook 'elixir-mode-hook 'mix-minor-mode)
  (setq compilation-scroll-output t))

(use-package exunit)
(add-hook 'elixir-mode-hook 'exunit-mode)

(use-package haskell-mode)
(use-package fsharp-mode)

(use-package web-mode)

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1))

(use-package tide
  :after (typescript-mode flycheck)
  :hook ((typescript-mode . tide-setup)
	 (typescript-mode . tide-hl-identifier-mode)
	 (before-save . tide-format-before-save)))

(add-hook 'typescript-mode-hook #'setup-tide-mode)
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-hook 'web-mode-hook
	  (lambda ()
	    (when (string-equal "tsx" (file-name-extension buffer-file-name))
	      (setup-tide-mode))))

(flycheck-add-mode 'typescript-tslint 'web-mode)

(use-package hl-todo
  :init
  (global-hl-todo-mode))

(use-package zop-to-char
  :bind
  (("M-z" . zop-up-to-char)))


(use-package yasnippet)
(add-to-list 'load-path
	     "~/.emacs.d/snippets")
(use-package yasnippet-snippets)
(yas-reload-all)
(yas-global-mode 1)

(use-package eglot
  :commands (eglot eglot-ensure)
  :hook ((elixir-mode . eglot-ensure)))

(use-package purescript-mode)
(use-package psc-ide)

(use-package json-mode)

(use-package jq-mode)
(add-to-list 'auto-mode-alist '("\\.jq$" . jq-mode))

(use-package restclient)
(use-package restclient-jq
  :straight (:host github
		   :repo "pashky/restclient.el"
		   :files ("restclient-jq.el")))
(add-to-list 'auto-mode-alist '("\\.rest$" . restclient-mode))
(use-package ob-restclient)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((restclient . t)))

;(use-package org-roam)
;(use-package websocket
;  :after org-roam)

;(use-package org-roam-ui
;  :after org-roam
  ;; :config
  ;; (setq org-roam-ui-sync-theme t
  ;; 	org-roam-ui-follow t
  ;; 	org-roam-ui-update-on-save t))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(font-lock-add-keywords 'org-mode
                        '(("^ *\\([-]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))


(setq org-ellipsis " ჻")
(add-hook 'org-mode-hook 'org-indent-mode)

(use-package magit-todos)
(magit-todos-mode)

(use-package avy)
(global-set-key (kbd "C-:") 'avy-goto-char-timer)
(global-set-key (kbd "C-c C-j") 'avy-resume)

(use-package vterm)
(add-hook 'vterm-mode (lambda () (setq-local global-hl-line-mode nil)))
(add-hook 'vterm-copy-mode (lambda () (call-interactively 'hl-line-mode)))
(use-package vterm-toggle)

(use-package uuidgen)
(defun sb/replace-uuid ()
  "Replace the UUID in quotes with a new UUID."
  (interactive)
  (save-excursion
    (let (p1 p2)
      (skip-chars-backward "-a-z0-9")
      (setq p1 (point))
      (skip-chars-forward "-a-z0-9")
      (setq p2 (point))
      (delete-region p1 p2)
      (uuidgen nil))))
(global-set-key (kbd "C-c u g") 'uuidgen)
(global-set-key (kbd "C-c u r") 'sb/replace-uuid)

(use-package elfeed)
(setq elfeed-feeds
      '("https://planet.emacslife.com/atom.xml"
	"https://karthinks.com/index.xml"))

(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first. If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))
(global-set-key [remap move-beginning-of-line]
		'smarter-move-beginning-of-line)
(global-set-key (kbd "C-a") 'smarter-move-beginning-of-line)

;; (use-package nano-theme)
;; (nano-light)
;; (use-package nano-modeline)
;; (nano-modeline-mode)

;; (set-face-attribute 'default nil :family "Menlo" :height 150)
(setq default-frame-alist '((font . "Office Code Pro D-14")))
(set-face-attribute 'default nil :weight 'light)
(set-face-attribute 'bold nil :weight 'medium)

(setq modus-themes-italic-constructs t
      modus-themes-bold-constructs t
      modus-themes-subtle-line-numbers t
      modus-themes-fringes 'subtle
      modus-themes-mode-line '(accented borderless (padding . 4) (height . 0.9))
      modus-themes-hl-line '(accented)
      modus-themes-paren-match '(bold intense)
      modus-themes-region '(bg-only no-extend accented))
(load-theme 'modus-operandi)

(setq user-full-name "Scott Barron"
      user-mail-address "scott@barron.io")

(defun sb/send-string-to-vterm (name str)
  (set-buffer name)
  (vterm-send-string (string-trim str))
  (vterm-send-return))

(defun sb/send-region-to-vterm (name)
  (interactive)
  (sb/send-string-to-vterm
   name
   (buffer-substring-no-properties (region-beginning) (region-end))))

(defun sb/send-line-to-vterm (name)
  (interactive)
  (sb/send-string-to-vterm
   name
   (buffer-substring-no-properties (line-beginning-position) (line-end-position))))

(defun sb/iex-project-name ()
  (concat 
   (file-name-base (string-remove-suffix "/" (projectile-project-root)))
   "-iex"))

(defun sb/iex-start-project ()
  (interactive)
  (vterm (sb/iex-project-name))
  (set-buffer (sb/iex-project-name))
  (vterm--set-directory (projectile-project-root))
  (vterm-send-string (concat "cd " (projectile-project-root) "\n"))
  (vterm-send-string "iex -S mix\n")
  (select-window (previous-window)))

(defun sb/iex-send-region ()
  (interactive)
  (sb/send-region-to-vterm (sb/iex-project-name)))

(defun sb/iex-send-line ()
  (interactive)
  (sb/send-line-to-vterm (sb/iex-project-name)))

(global-set-key (kbd "C-c v l") 'sb/send-line-to-vterm)
(global-set-key (kbd "C-c v r") 'sb/send-region-to-vterm)
(global-set-key (kbd "C-c i l") 'sb/iex-send-line)
(global-set-key (kbd "C-c i r") 'sb/iex-send-region)
(global-set-key (kbd "C-c i p") 'sb/iex-start-project)

(add-to-list 'display-buffer-alist
	     '(".*-iex"
		(display-buffer-in-side-window)
		(window-height . 0.25)
		(side . bottom)
		(slot . 0)))

(add-to-list 'display-buffer-alist
	     '("\*exunit-compilation\*"
	       (display-buffer-in-side-window)
	       (window-height . 0.25)
	       (side . bottom)
	       (slot . 1)))

(setq-default mode-line-format '("%e" mode-line-front-space
			 mode-line-modified "  %b  " (vc-mode vc-mode) "  " mode-name))

(setq custom-file "~/.emacs.d/emacs-custom.el")
(load custom-file)

;; ("%e" mode-line-front-space
;;  (:propertize
;;   ("" mode-line-mule-info mode-line-client mode-line-modified mode-line-remote)
;;   display
;;   (min-width
;;    (5.0)))
;;  mode-line-frame-identification mode-line-buffer-identification "  " mode-line-position
;;  (vc-mode vc-mode)
;;  "  " mode-line-modes mode-line-misc-info mode-line-end-spaces)

(defun ask-before-closing ()
  "Close only if y was pressed."
  (interactive)
  (if (y-or-n-p (format "Are you sure you want to close this frame? "))
      (save-buffers-kill-emacs)                                                                                            
    (message "Canceled frame close")))

(global-set-key (kbd "C-x C-c") 'ask-before-closing)
