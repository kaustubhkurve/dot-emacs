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
    elpy
    flycheck
    material-theme
    go-mode
    projectile
    helm
    helm-projectile
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
    ample-theme
    ))

(mapc #'(lambda (package)
    (unless (package-installed-p package)
      (package-install package)))
      myPackages)


;; BASIC CUSTOMIZATION
;; --------------------------------------

(setq inhibit-startup-message t) ;; hide the startup message
(load-theme 'ample t) ;; load ample them
;; (global-linum-mode t) ;; enable line numbers globally

;; Company Mode
;; --------------------------------------

(add-hook 'after-init-hook 'global-company-mode)


;; PYTHON CONFIGURATION
;; --------------------------------------

(elpy-enable)
(setq elpy-rpc-python-command "python3")
(elpy-use-cpython "python3")
(setq python-shell-interpreter "python3")
(setq py-python-command "/usr/bin/python3")
(setq elpy-rpc-backend "jedi")
(setq elpy-interactive-python-command "ipython")
(setq elpy-use-ipython)
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
 '(custom-safe-themes
   (quote
    ("39dd7106e6387e0c45dfce8ed44351078f6acd29a345d8b22e7b8e54ac25bac4" "0c387e27a3dd040b33c6711ff92e13bd952369a788eee97e4e4ea2335ac5528f" "12b4427ae6e0eef8b870b450e59e75122d5080016a9061c9696959e50d578057" "7ceb8967b229c1ba102378d3e2c5fef20ec96a41f615b454e0dc0bfa1d326ea6" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "ad950f1b1bf65682e390f3547d479fd35d8c66cafa2b8aa28179d78122faa947" "d8f76414f8f2dcb045a37eb155bfaa2e1d17b6573ed43fb1d18b936febc7bbc2" "a2e7b508533d46b701ad3b055e7c708323fb110b6676a8be458a758dd8f24e27" "0c311fb22e6197daba9123f43da98f273d2bfaeeaeb653007ad1ee77f0003037" "f0d8af755039aa25cd0792ace9002ba885fd14ac8e8807388ab00ec84c9497d7" "4f5bb895d88b6fe6a983e63429f154b8d939b4a8c581956493783b2515e22d6d" "8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" "c4465c56ee0cac519dd6ab6249c7fd5bb2c7f7f78ba2875d28a50d3c20a59473" "e97dbbb2b1c42b8588e16523824bc0cb3a21b91eefd6502879cf5baa1fa32e10" "2305decca2d6ea63a408edd4701edf5f4f5e19312114c9d1e1d5ffe3112cde58" "919469423e232db3cc35f6a94134051dd8a5a95d5cbb2a9b3c519e371a069615" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "c27cd4c7cc83199b224a7c584d5ebc8bc0b6fee77cf3a83218019eca5f33d214" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default)))
 '(magit-log-arguments (quote ("--graph" "--color" "--decorate" "-n256")))
 '(menu-bar-mode nil)
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

(add-to-list 'load-path "~/.emacs.d/go-mode")

(add-to-list 'load-path "~/.emacs.d/go-mode/go-mode.el")
(require 'go-mode-autoloads)

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

;; init.el ends here
