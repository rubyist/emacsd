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
(setq inhibit-start-echo-area-message t)
(setq initial-buffer-choice nil)
(setq frame-title-format nil)
(setq use-file-dialog nil)
(setq use-dialog-box nil)
(setq pop-up-windows nil)
(setq cursor-in-non-selected-windows nil)
(setq ring-bell-function 'ignore)

(defalias 'yes-or-no-p 'y-or-n-p)

(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode t)
(global-hl-line-mode)

(add-to-list 'initial-frame-alist '(width . (text-pixels . 1260)))
(add-to-list 'initial-frame-alist '(height . (text-pixels . 1920)))
(add-to-list 'initial-frame-alist '(top . 0))
(add-to-list 'initial-frame-alist '(left . 1280))

(unbind-key "C-z")

(electric-pair-mode)

(use-package exec-path-from-shell
  :init
  (setq exec-path-from-shell-variables
	'("PATH" "MANPATH" "SSH_AUTH_SOCK"))
  (exec-path-from-shell-initialize))

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
(use-package wgrep)

(use-package magit)
(use-package git-gutter)
(global-git-gutter-mode +1)
(use-package git-timemachine)

(use-package flycheck
  :init (global-flycheck-mode))

(use-package elixir-mode)
(add-hook 'elixir-mode-hook
	  (lambda () (add-hook 'before-save-hook 'elixir-format nil t)))

(use-package hl-todo
  :init
  (global-hl-todo-mode))

(use-package zop-to-char
  :bind
  (("M-z" . zop-up-to-char)))


(use-package yasnippet)
(add-to-list 'load-path
	     "~/.emacs.d/snippets")
(yas-reload-all)
(add-hook 'prog-mode-hook #'yas-minor-mode)

(use-package eglot
  :commands (eglot eglot-ensure)
  :hook ((elixir-mode . eglot-ensure)))

(use-package jq-mode)
(add-to-list 'auto-mode-alist '("\\.jq$" . jq-mode))

(use-package restclient)
(use-package restclient-jq
  :straight (:host github
		   :repo "pashky/restclient.el"
		   :files ("restclient-jq.el")))
(add-to-list 'auto-mode-alist '("\\.rest$" . restclient-mode))

(use-package org-roam)
(use-package websocket
  :after org-roam)

(use-package org-roam-ui
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
	org-roam-ui-follow t
	org-roam-ui-update-on-save t))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(font-lock-add-keywords 'org-mode
                        '(("^ *\\([-]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

(setq org-ellipsis " ჻")

(use-package avy)
(global-set-key (kbd "C-:") 'avy-goto-char-timer)
(global-set-key (kbd "C-c C-j") 'avy-resume)

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

(setq user-full-name "Scott Barron"
      user-mail-address "scott@barron.io")

