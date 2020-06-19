;; init.el --- Emacs configuration

;; INSTALL PACKAGES
;; --------------------------------------

(require 'package)

(add-to-list 'package-archives
       '("melpa" . "http://melpa.org/packages/") t)

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar myPackages
  '(better-defaults
    use-package
    company
    company-jedi
    company-go
    company-racer
    racer
    elpy
    pyvenv
    flycheck
    flycheck-rust
    go-mode
    rust-mode
    projectile
    helm
    helm-projectile
    helm-gtags
    ag
    helm-ag
    magit
    smartparens
    switch-window
    which-key
    js2-mode
    xref-js2
    yaml-mode
    dockerfile-mode
    multiple-cursors
    crux
    restclient
    expand-region
    aggressive-indent
    haskell-mode
    intero
    toml-mode
    solarized-theme
    scala-mode
    anzu
    php-mode
    ac-php
    dumb-jump
    tide
    editorconfig
    web-mode
    terraform-mode
    markdown-mode
    protobuf-mode
    lsp-mode
    lsp-ui
    company-lsp
    robe
    rubocop
    plantuml-mode
    cider
    ))

(mapc #'(lambda (package)
    (unless (package-installed-p package)
      (package-install package)))
      myPackages)

;; BASIC CUSTOMIZATION
;; --------------------------------------

;; Key bindings for mac
(when (eq system-type 'darwin)
  (setq mac-function-modifier 'control)
  (setq mac-command-modifier 'meta)
  (global-set-key (kbd "<home>") 'left-word)
  (global-set-key (kbd "<end>") 'right-word)
  )

(setq solarized-scale-org-headlines nil)
(load-theme 'solarized-light t)
(set-cursor-color "brown")

(setq inhibit-startup-message t) ;; hide the startup message
(column-number-mode 1) ;; show column/row in mode line
(delete-selection-mode 1) ;; Enable deletions when typing after a mark is active
(setq custom-file "~/.emacs.d/local/custom-set.el")
(load custom-file 'noerror)

(show-paren-mode 1)

;; Save all backup files in /tmp
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))

;; Company Mode
;; --------------------------------------

(add-hook 'after-init-hook 'global-company-mode)
(setq company-idle-delay 0.2)
(setq company-minimum-prefix-length 1)


;; PYTHON CONFIGURATION
;; --------------------------------------

(elpy-enable)
(setq elpy-rpc-python-command "python3")
;; (elpy-use-cpython "python3")
(setq python-shell-interpreter "python3")
(setq py-python-command "/usr/bin/python3")
(setq elpy-rpc-backend "jedi")
(setq elpy-interactive-python-command "ipython")
(setq elpy-use-ipython 1)
(add-to-list 'company-backends 'company-jedi)

;; use flycheck with elpy
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

(add-hook 'elpy-mode-hook #'smartparens-mode)


;; UI CONFIGURATION
;; --------------------------------------------------------------

(setq initial-frame-alist '((font . "Source Code Pro-14")))
(setq default-frame-alist '((font . "Source Code Pro-14")))

(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (replace-regexp-in-string
                          "[ \t\n]*$"
                          ""
                          (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq eshell-path-env path-from-shell) ; for eshell users
    (setq exec-path (split-string path-from-shell path-separator))))

(when window-system (set-exec-path-from-shell-PATH))

;; GO CONFIGURATION
;; ---------------------------------------------------------

;; CONF BASED ON GOLANG SERVER BEGINS HERE
(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :hook (go-mode . lsp-deferred))

;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

;; Optional - provides fancier overlays.
(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)

;; Company mode is a standard completion package that works well with lsp-mode.
(use-package company
  :ensure t
  :config
  ;; Optionally enable completion-as-you-type behavior.
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1))

;; company-lsp integrates company mode completion with lsp-mode.
;; completion-at-point also works out of the box but doesn't support snippets.
(use-package company-lsp
  :ensure t
  :commands company-lsp)

;; Optional - provides snippet support.
(use-package yasnippet
  :ensure t
  :commands yas-minor-mode
  :hook (go-mode . yas-minor-mode))

;; HELM CONFIGURATION
;; ----------------------------------------------------------

(require 'helm)
(require 'helm-config)

;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-c h o") 'helm-occur)

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(setq helm-M-x-fuzzy-match t) ;; optional fuzzy matching for helm-M-x
(setq helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match    t)

(setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t)

(setq helm-gtags-ignore-case t
      helm-gtags-auto-update t
      helm-gtags-use-input-at-cursor t
      helm-gtags-pulse-at-cursor t
      helm-gtags-prefix-key "\C-cg"
      helm-gtags-suggested-key-mapping t)

(require 'helm-gtags)
;; Enable helm-gtags-mode
(add-hook 'dired-mode-hook 'helm-gtags-mode)
(add-hook 'eshell-mode-hook 'helm-gtags-mode)
(add-hook 'c-mode-hook 'helm-gtags-mode)
(add-hook 'c++-mode-hook 'helm-gtags-mode)
(add-hook 'asm-mode-hook 'helm-gtags-mode)

(define-key helm-gtags-mode-map (kbd "C-c g a") 'helm-gtags-tags-in-this-function)
(define-key helm-gtags-mode-map (kbd "C-j") 'helm-gtags-select)
(define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
(define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
(define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
(define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)

(semantic-mode 1)
(helm-mode 1)

;; SMARTPARENS CONFIG
;; -------------------------------------------------------------

(require 'smartparens)

;; FULLSCREEN TOGGLE CONFIGURATION
;; --------------------------------------------------------------

(defun my-fullscreen ()
  (interactive)
  (set-frame-parameter nil 'fullscreen 'fullboth)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (menu-bar-mode -1))

(defun my-non-fullscreen ()
  (interactive)
  (set-frame-parameter nil 'width 82)
  (set-frame-parameter nil 'fullscreen 'fullheight)
  (menu-bar-mode t))

(defun toggle-fullscreen ()
  (interactive)
  (if (eq (frame-parameter nil 'fullscreen) 'fullboth)
      (my-non-fullscreen)
    (my-fullscreen)))

(global-set-key (kbd "<f11>") 'toggle-fullscreen)

(my-fullscreen)

;; PROJECTILE CONFIG
;; ------------------------------------------------------------------

(projectile-mode 1)
(setq projectile-completion-system 'helm)
(helm-projectile-on)
(setq projectile-switch-project-action 'helm-projectile)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(setq helm-ag-insert-at-point 'symbol)

;; MAGIT CONFIGURATION
;; -------------------------------------------------------------------

(global-auto-revert-mode 1)
(global-set-key (kbd "C-x g") 'magit-status)

;; SWITCH WINDOW CONFIGURATION
;; -------------------------------------------------------------------

(require 'switch-window)
(global-set-key (kbd "M-p") 'switch-window)

;; WHICH KEY CONFIGURATION
;; -------------------------------------------------------------------

(which-key-mode)

;; JAVASCRIPT CONFIGURATION
;; -------------------------------------------------------------------

(require 'xref-js2)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-hook 'js2-mode-hook #'js2-imenu-extras-mode)
;; (add-hook 'js-mode-hook 'js2-minor-mode)
;; (add-hook 'js2-mode-hook 'ac-js2-mode)
(define-key js-mode-map (kbd "M-.") nil)

(add-hook 'js2-mode-hook (lambda ()
  (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))

;; RUST MODE CONFIGURATION
(require 'rust-mode)

(add-hook 'rust-mode-hook
          (lambda () (setq indent-tabs-mode nil)))

;; (add-hook 'rust-mode-hook #'racer-mode)
;; (add-hook 'racer-mode-hook #'eldoc-mode)
;; (add-hook 'racer-mode-hook #'company-mode)
;; (define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
;; (setq company-tooltip-align-annotations t)

;; (setq rust-format-on-save t)
(define-key rust-mode-map (kbd "C-c C-r") 'rust-run)
(define-key rust-mode-map (kbd "C-c C-t") 'rust-test)
(define-key rust-mode-map (kbd "C-c C-c") 'rust-compile)

;; TODO: Automatically run clippy on buffer change (rust-run-clippy)
;; (use-package dap-mode)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language

;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
;; (setq lsp-keymap-prefix "s-l")

(use-package lsp-mode
  :ensure t
  :hook (rust-mode . lsp-deferred)
  :commands (lsp lsp-deferred))

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)

(use-package helm-lsp
  :ensure t
  :commands helm-lsp-workspace-symbol)

(use-package lsp-treemacs
  :ensure t
  :commands lsp-treemacs-errors-list)

(setq lsp-ui-doc-enable nil)
(setq lsp-ui-sideline-enable nil)

;; YAML MODE CONFIGURATION
;; -------------------------------------------------------------------

(require 'yaml-mode)
(add-hook 'yaml-mode-hook
  '(lambda ()
    (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

(add-hook 'yaml-mode-hook #'ansible-doc-mode)

;; DOCKERFILE MODE CONFIGURATION
;; -------------------------------------------------------------------
(require 'dockerfile-mode)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

;; MULTIPLE CURSORS CONFIGURATION
;; -------------------------------------------------------------------
(require 'multiple-cursors)
(global-set-key (kbd "C-c C->") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; EXPAND REGION CONFIGURATION
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;; AGGRESSIVE INDENT CONFIGURATION
;; (global-aggressive-indent-mode 1) ;; Activate it globally to see how it goes

;; Org mode configuration
(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)
(setq org-agenda-files (list "~/org/personal.org" "~/org/work.org"))

;; Haskell Configuration
(add-hook 'haskell-mode-hook 'intero-mode)

;; TOML Configuration
(require 'toml-mode)

;; Enable anzu mode
(global-anzu-mode +1)

;; Enable PHP mode
(require 'php-mode)

;; Highlight Comment Annotations
(defun font-lock-comment-annotations ()
  (font-lock-add-keywords
   nil '(("\\<\\(FIX\\(ME\\)?\\|TODO\\|HACK\\|REFACTOR\\):"
          1 font-lock-warning-face t))))

(add-hook 'prog-mode-hook 'font-lock-comment-annotations)

;; bind json pretty print. This is a global keybinding. Some minor modes
;; may overwrite this
(global-set-key (kbd "C-x j") 'json-pretty-print)

;; Dired mode customizations
;; Older versions of emacs do not require including dired-x
(require 'dired-x)
(add-hook 'dired-mode-hook
	  (lambda ()
	    (dired-omit-mode 1)
	    (setq dired-omit-files
		  (concat dired-omit-files "\\|\\.pyc$"))
	    ))

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ctp?\\'" . web-mode))

(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 4)
  (setq web-mode-css-indent-offset 4)
  (setq web-mode-code-indent-offset 4)
  (setq web-mode-indent-style 4)
  )

(add-hook 'web-mode-hook  'my-web-mode-hook)

;; Dump Jump Configuration
(dumb-jump-mode)

;; TIDE CONFIGURATION
(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; formats the buffer before saving
(add-hook 'before-save-hook 'tide-format-before-save)
(add-hook 'typescript-mode-hook #'setup-tide-mode)

;; EditorConfig Configuration
(require 'editorconfig)
(editorconfig-mode 1)

;; Enable protobuf mode
;; (protobuf-mode)

;; RUBY MODE
(add-hook 'ruby-mode-hook 'robe-mode)
(add-hook 'ruby-mode-hook 'flycheck-mode)
(add-hook 'ruby-mode-hook #'rubocop-mode)

(eval-after-load 'company
  '(push 'company-robe company-backends))

(defun ruby-mode-reload-env()
  (when (not (robe-running-p)) (robe-start))
  (ruby-load-file buffer-file-name)
)

;; Reloads the current ruby process if a file is opened or changed
;; There must be a better way to do this, investigate!
(defun ruby-mode-custom-hooks ()
  (add-hook 'find-file-hook 'ruby-mode-reload-env)
  (add-hook 'after-save-hook 'ruby-mode-reload-env)
  )

(add-hook 'ruby-mode-hook #'ruby-mode-custom-hooks)

;; PLANTUML MODE
(require 'plantuml-mode)
;; Paranoid issue, if for some reason plantuml-set-exec-mode is changed
;; to server in a plantuml buffer, bye bye privacy!
;; The check is (or plantuml-exec-mode plantuml-default-exec-mode)
(setq plantuml-default-exec-mode 'executable)

;; MARKDOWN MODE
(use-package markdown-mode
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "pandoc"))

;; init.el ends here
