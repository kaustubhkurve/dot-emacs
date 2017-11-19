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
    anti-zenburn-theme
    anzu
    ))

(mapc #'(lambda (package)
    (unless (package-installed-p package)
      (package-install package)))
      myPackages)


;; BASIC CUSTOMIZATION
;; --------------------------------------

(load-theme 'anti-zenburn t)
(setq inhibit-startup-message t) ;; hide the startup message
(column-number-mode 1) ;; show column/row in mode line
;; (global-linum-mode t) ;; enable line numbers globally
(delete-selection-mode 1) ;; Enable deletions when typing after a mark is active
(setq custom-file "~/.emacs.d/local/custom-set.el")
(load custom-file 'noerror)

;; Company Mode
;; --------------------------------------

(add-hook 'after-init-hook 'global-company-mode)
(setq company-idle-delay 0.2)
(setq company-minimum-prefix-length 1)


;; PYTHON CONFIGURATION
;; --------------------------------------

(elpy-enable)
(setq elpy-rpc-python-command "python3")
(elpy-use-cpython "python3")
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

;; enable autopep8 on save
;; (require 'py-autopep8)
;; (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)

;; UI CONFIGURATION
;; --------------------------------------------------------------

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(magit-log-arguments (quote ("--graph" "--color" "--decorate" "-n256")))
 '(menu-bar-mode nil)
 '(package-selected-packages
   (quote
    (intero haskell-mode yaml-mode which-key switch-window smartparens restclient racer multiple-cursors magit key-chord js2-mode helm-projectile helm-gtags helm-ag flycheck-rust flatui-theme expand-region evil-tutor elpy dockerfile-mode crux company-racer company-jedi company-go better-defaults aggressive-indent ag)))
 '(send-mail-function (quote mailclient-send-it))
 '(show-paren-mode t)
 '(tool-bar-mode nil))

(setq initial-frame-alist '(
  (font . "Source Code Pro Medium-11")
))

(setq default-frame-alist '(
  (font . "Source Code Pro Medium-11")
  ))

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

(with-eval-after-load 'company
  (add-to-list 'company-backends 'company-go))

(require 'go-mode)
;; (require 'go-mode-autoloads)

(setenv "GOPATH" "/home/kaustubh/workspace/go-projects")

(setq exec-path (cons "/usr/local/go/bin" exec-path))
(add-to-list 'exec-path "/home/kaustubh/workspace/go-projects/bin")

(load-file "$GOPATH/src/golang.org/x/tools/cmd/oracle/oracle.el")

(defun my-go-mode-hook ()
  ; Use goimports instead of go-fmt
  (setq gofmt-command "goimports")
  ; Call Gofmt before saving
  (add-hook 'before-save-hook 'gofmt-before-save)
  ; Customize compile command to run go build
  (if (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command)
           "go build -v && go test -v && go vet"))
  ; Godef jump key binding
  (local-set-key (kbd "M-.") 'godef-jump)
  (define-key go-mode-map (kbd "RET") 'newline-and-indent)
  )

(add-hook 'go-mode-hook 'my-go-mode-hook)
(add-hook 'go-mode-hook #'smartparens-mode)

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
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; FULLSCREEN TOGGLE CONFIGURATION
;; --------------------------------------------------------------

(defun my-fullscreen ()
  (interactive)
  (set-frame-parameter nil 'fullscreen 'fullboth) ;this makes the frame go fullscreen
  (tool-bar-mode -1) ;these 3 lines turn off GUI junk
  (scroll-bar-mode -1)
  (menu-bar-mode -1))

(defun my-non-fullscreen ()
  (interactive)
  (set-frame-parameter nil 'width 82)
  (set-frame-parameter nil 'fullscreen 'fullheight)
  (menu-bar-mode t)) ;I don't turn tool-bar and scroll-bar back on b/c I never want them

(defun toggle-fullscreen ()
  (interactive)
  (if (eq (frame-parameter nil 'fullscreen) 'fullboth)  ;tests if already fullscreened
      (my-non-fullscreen)
    (my-fullscreen)))

(global-set-key (kbd "<f11>") 'toggle-fullscreen)

(my-fullscreen)

;; PROJECTILE CONFIG
;; ------------------------------------------------------------------

(projectile-global-mode)
(setq projectile-completion-system 'helm)
(helm-projectile-on)

(setq projectile-switch-project-action 'helm-projectile)

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

(add-hook 'js-mode-hook 'js2-minor-mode)
(add-hook 'js2-mode-hook 'ac-js2-mode)

;; RUST MODE CONFIGURATION
(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)

(add-hook 'racer-mode-hook #'company-mode)

(require 'rust-mode)
(define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
(setq company-tooltip-align-annotations t)

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

;; Anzu configuration
(global-anzu-mode +1)

;; init.el ends here
