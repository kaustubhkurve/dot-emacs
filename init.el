;;; package --- Emacs Configuration

;;; Commentary:
;; Contains the Emacs configuration

;;; Code:

(require 'package)

(add-to-list 'package-archives
       '("melpa" . "http://melpa.org/packages/") t)

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(eval-when-compile
  (require 'use-package))

(use-package emacs
  :init
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
  :bind ("C-x j" . json-pretty-print)
  :config
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (menu-bar-mode -1)

  (add-to-list 'default-frame-alist '(font . "Source Code Pro Medium-11"))
  (add-to-list 'default-frame-alist '(fullscreen . fullscreen))
  (load-theme 'modus-operandi t)
  (set-cursor-color "brown")

  (setq inhibit-startup-message t)
  (setq custom-file "~/.emacs.d/local/custom-set.el")
  (setq backup-directory-alist `((".*" . ,temporary-file-directory)))

  (column-number-mode 1)
  (delete-selection-mode 1)
  (load custom-file 'noerror)
  (show-paren-mode 1)

  ;; Highlight Comment Annotations
  (defun font-lock-comment-annotations ()
    (font-lock-add-keywords
     nil '(("\\<\\(FIX\\(ME\\)?\\|TODO\\|HACK\\|REFACTOR\\):"
	    1 font-lock-warning-face t))))

  (add-hook 'prog-mode-hook 'font-lock-comment-annotations)

  ;; Worst case if we are on mac, overide keys
  (when (eq system-type 'darwin)
    (setq mac-function-modifier 'control)
    (setq mac-command-modifier 'meta)
    (global-set-key (kbd "<home>") 'left-word)
    (global-set-key (kbd "<end>") 'right-word))

  (add-hook 'dired-mode-hook
	    (lambda ()
	      (dired-omit-mode 1)
	      (setq dired-omit-files
		    (concat dired-omit-files "\\|\\.pyc$"))
	      ))
  :custom
  (enable-recursive-minibuffers t)
  (read-extended-command-predicate #'command-completion-default-include-p))


;;; Packages

(use-package exec-path-from-shell
  :ensure t
  :init (when (memq window-system '(mac ns x))
	  (exec-path-from-shell-initialize)))


(use-package company
  :ensure t
  :config
  (global-company-mode)
  (push 'company-robe company-backends)
  (setq company-idle-delay 0.2)
  (setq company-minimum-prefix-length 1)
  (setq company-tooltip-align-annotations t))


(use-package flycheck
  :ensure t
  :config
  (global-flycheck-mode))


(use-package company-jedi
  :ensure t
  :config
  (add-to-list 'company-backends 'company-jedi))


(use-package elpy
  :ensure t
  :init
  (elpy-enable))


(use-package vertico
  :ensure t
  :custom
  (vertico-scroll-margin 0)
  (vertico-count 20)
  (vertico-cycle t)
  :init
  (vertico-mode))


(use-package vertico-directory
  :after vertico
  :ensure nil
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))


(use-package savehist
  :init
  (savehist-mode))


(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))


(use-package consult
  :ensure t
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)                  ;; Alternative: consult-fd
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  :hook (completion-list-mode . consult-preview-at-point-mode)

  :init
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  (advice-add #'register-preview :override #'consult-register-window)

  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  :config
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  (setq consult-narrow-key "<") ;; "C-+"
  )


(use-package marginalia
  :ensure t
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))


(use-package embark
  :ensure t
  :bind
  (("C-." . embark-act)
   ("C-;" . embark-dwim)
   ("C-h B" . embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))


(use-package embark-consult
  :ensure t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))


(use-package yasnippet
  :ensure t
  :commands yas-minor-mode
  :hook (go-mode . yas-minor-mode))


(use-package go-mode
  :ensure t)


;; Scala Configuration
(use-package scala-mode
  :ensure t
  :interpreter ("scala" . scala-mode))


(use-package sbt-mode
  :ensure t
  :commands sbt-start sbt-command
  :config
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
  (setq sbt:program-options '("-Dsbt.supershell=false")))


(use-package magit
  :ensure t
  :config (global-auto-revert-mode 1)
  :bind ("C-x g" . magit-status))


(use-package switch-window
  :ensure t
  :bind ("M-p" . switch-window))


(use-package which-key
  :ensure t
  :init (which-key-mode))


(use-package markdown-mode
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "pandoc"))


(use-package js2-mode
  :ensure t)


(use-package xref-js2
  :ensure t
  :init (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  :hook
  (js2-mode . js2-imenu-extras-mode)
  ('js2-mode-hook (lambda ()
		    (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t))))


(use-package rust-mode
  :ensure t
  :config
  (setq indent-tabs-mode nil)
  (setq rust-format-on-save t)
  :bind (("C-c C-r" . rust-run)
	 ("C-c C-t" . rust-test)
	 ("C-c C-c" . rust-compile)))


(use-package yaml-mode
  :ensure t
  :bind (:map yaml-mode-map ("\C-m" . newline-and-indent))
  :hook (yaml-mode . ansible-doc-mode))


(use-package dockerfile-mode
  :ensure t)


(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))


(use-package org
  :ensure t
  :bind (("\C-cl" . org-store-link)
	 ("\C-ca" . org-agenda))
  :config
  (setq org-log-done t)
  (setq org-agenda-files (list "~/org/personal.org" "~/org/work.org")))


(use-package toml-mode
  :ensure t)


(use-package php-mode
  :ensure t)


(use-package web-mode
  :ensure t
  :mode
  (("\\.phtml\\'" . web-mode)
   ("\\.php\\'" . web-mode)
   ("\\.tpl\\'" . web-mode)
   ("\\.[agj]sp\\'" . web-mode)
   ("\\.as[cp]x\\'" . web-mode)
   ("\\.erb\\'" . web-mode)
   ("\\.mustache\\'" . web-mode)
   ("\\.djhtml\\'" . web-mode)))


(use-package dumb-jump
  :ensure t
  :init (dumb-jump-mode))


(use-package editorconfig
  :ensure t
  :config (editorconfig-mode 1))


(use-package protobuf-mode
  :ensure t
  :mode ("\\.proto$" . protobuf-mode))


(use-package rvm
  :ensure t
  :init (advice-add 'inf-ruby-console-auto :before #'rvm-activate-corresponding-ruby))


(use-package robe
  :ensure t)


(use-package ruby-mode
  :ensure t
  :hook ((ruby-mode . robe-mode)
	 (ruby-mode . flycheck-mode)
	 (ruby-mode . rubocop-mode)))


(use-package plantuml-mode
  :ensure t
  :config (setq plantuml-default-exec-mode 'executable))


(use-package ag
  :ensure t)


(use-package crux
  :ensure t)


(use-package terraform-mode
  :ensure t)

(use-package rubocop
  :ensure t)


(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))

(use-package lsp-mode
  :ensure t
  :hook
  (rust-mode . lsp-deferred)
  (go-mode . lsp-deferred)
  (go-mode . lsp-go-install-save-hooks)
  (scala-mode . lsp-deferred)
  :commands (lsp lsp-deferred)
  :config
  (setq lsp-prefer-flymake nil)
  (setq lsp-keep-workspace-alive nil)
  (setq lsp-enable-snippet nil)
  (setq lsp-headerline-breadcrumb-enable nil))


(use-package lsp-metals
  :ensure t)

(use-package lsp-java
  :ensure t
  :hook (java-mode . lsp-deferred))


;;; init.el ends here
