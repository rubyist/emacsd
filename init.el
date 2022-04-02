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

(use-package orderless
  :ensure t
  :custom (completion-styles '(orderless)
			     completion-category-defaults nil
			     completion-category-overrides '((file (styles partial-completion)))))

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
  (setq enable-recursive-minibuffers t))

(use-package magit)

(use-package ruby-end)

(use-package elixir-mode
  :init
  (add-to-list 'elixir-mode-hook
	       (defun auto-activate-ruby-end-mode-for-elixir-mode ()
		 (set (make-variable-buffer-local 'ruby-end-expand-keywords-berfore-re)
		      "\\(?:^\\|\\s-+\\)\\(?:do\\)")
		 (set (make-variable-buffer-local 'ruby-end-check-statement-modifiers) nil)
		 (ruby-end-mode +1)))
  (add-hook 'elixir-mode-hook
	    (lambda () (add-hook 'before-save-hook 'elixir-format nil t))))

;; TODO these don't turn on and stay on for some reason?
;; (linum-mode)
;; (hl-line-mode)


; (global-hl-todo-mode)

; (linum-mode)
; (linum-relative-mode)

(setq user-full-name "Scott Barron"
      user-mail-address "scott@barron.io")

;(straight-use-package 'which-key)
; (straight-use-package 'magit)
; (straight-use-package 'company-mode)
; (straight-use-package 'hl-todo)
; (straight-use-package 'zop-to-char)
; (straight-use-package 'linum-relative)
; (straight-use-package 'avy)
; (straight-use-package 'nano-theme)
; (straight-use-package 'ag)
; (straight-use-package 'rg)
; (straight-use-package 'ripgrep)
; (straight-use-package 'wgrep)
