;;; .emacs.el -- Emacs configuration file.

;;; Commentary:

;;; Code:
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

;; TODO add the rest of the packages to this list
(defvar my-packages '(better-defaults
                      projectile
                      clojure-mode
                      cider))

(dolist (p my-packages)
  (unless (package-installed-p p)
    (package-install p)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#0d0d0d" "#cc6666" "#b5bd68" "#f0c674" "#81a2be" "#c9b4cf" "#8abeb7" "#ffffff"])
 '(company-quickhelp-color-background "#4F4F4F")
 '(company-quickhelp-color-foreground "#DCDCCC")
 '(custom-enabled-themes (quote (doom-challenger-deep)))
 '(custom-safe-themes
   (quote
    ("b89a4f5916c29a235d0600ad5a0849b1c50fab16c2c518e1d98f0412367e7f97" "6b289bab28a7e511f9c54496be647dc60f5bd8f9917c9495978762b99d8c96a0" "86704574d397606ee1433af037c46611fb0a2787e8b6fd1d6c96361575be72d2" "6fc18b6b991926ea5debf205ee144b1a1fdcfcb69236024cc0bd863b666a1a11" "84890723510d225c45aaff941a7e201606a48b973f0121cb9bcb0b9399be8cba" "1c082c9b84449e54af757bcae23617d11f563fc9f33a832a8a2813c4d7dfb652" "356e5cbe0874b444263f3e1f9fffd4ae4c82c1b07fe085ba26e2a6d332db34dd" "4697a2d4afca3f5ed4fdf5f715e36a6cac5c6154e105f3596b44a4874ae52c45" "b35a14c7d94c1f411890d45edfb9dc1bd61c5becd5c326790b51df6ebf60f402" "3a3de615f80a0e8706208f0a71bbcc7cc3816988f971b6d237223b6731f91605" "43c808b039893c885bdeec885b4f7572141bd9392da7f0bd8d8346e02b2ec8da" "8aca557e9a17174d8f847fb02870cb2bb67f3b6e808e46c0e54a44e3e18e1020" "93a0885d5f46d2aeac12bf6be1754faa7d5e28b27926b8aa812840fe7d0b7983" "151bde695af0b0e69c3846500f58d9a0ca8cb2d447da68d7fbf4154dcf818ebc" "d1b4990bd599f5e2186c3f75769a2c5334063e9e541e37514942c27975700370" "49ec957b508c7d64708b40b0273697a84d3fee4f15dd9fc4a9588016adee3dad" "9954ed41d89d2dcf601c8e7499b6bb2778180bfcaeb7cdfc648078b8e05348c6" "6d589ac0e52375d311afaa745205abb6ccb3b21f6ba037104d71111e7e76a3fc" "fe666e5ac37c2dfcf80074e88b9252c71a22b6f5d2f566df9a7aa4f9bea55ef8" "a3fa4abaf08cc169b61dea8f6df1bbe4123ec1d2afeb01c17e11fdc31fc66379" "fd944f09d4d0c4d4a3c82bd7b3360f17e3ada8adf29f28199d09308ba01cc092" "a8c210aa94c4eae642a34aaf1c5c0552855dfca2153fa6dd23f3031ce19453d4" "f0dc4ddca147f3c7b1c7397141b888562a48d9888f1595d69572db73be99a024" "d2e9c7e31e574bf38f4b0fb927aaff20c1e5f92f72001102758005e53d77b8c9" "6b2636879127bf6124ce541b1b2824800afc49c6ccd65439d6eb987dbf200c36" "ec5f697561eaf87b1d3b087dd28e61a2fc9860e4c862ea8e6b0b77bd4967d0ba" "f71859eae71f7f795e734e6e7d178728525008a28c325913f564a42f74042c31" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" default)))
 '(fci-rule-color "#383838")
 '(haskell-mode-hook (quote (interactive-haskell-mode)))
 '(haskell-stylish-on-save t)
 '(hindent-reformat-buffer-on-save t)
 '(hl-todo-keyword-faces
   (quote
    (("TODO" . "#dc752f")
     ("NEXT" . "#dc752f")
     ("THEM" . "#2d9574")
     ("PROG" . "#4f97d7")
     ("OKAY" . "#4f97d7")
     ("DONT" . "#f2241f")
     ("FAIL" . "#f2241f")
     ("DONE" . "#86dc2f")
     ("NOTE" . "#b1951d")
     ("KLUDGE" . "#b1951d")
     ("HACK" . "#b1951d")
     ("TEMP" . "#b1951d")
     ("FIXME" . "#dc752f")
     ("XXX+" . "#dc752f")
     ("\\?\\?\\?+" . "#dc752f"))))
 '(jdee-db-active-breakpoint-face-colors (cons "#0d0d0d" "#41728e"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#0d0d0d" "#b5bd68"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#0d0d0d" "#5a5b5a"))
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(package-selected-packages
   (quote
    (direnv cyberpunk-theme pinentry fish-mode tide dante edit-indirect company-lsp lsp-haskell lsp-ui lsp-mode company-ghc all-the-icons svelte-mode hindent proof-general company-coq flycheck-kotlin kotlin-mode markdown-preview-mode markdown-mode idris-mode doom-modeline doom-themes dockerfile-mode flycheck-clj-kondo impatient-mode jedi-direx jedi python-mode js-comint paredit parinfer use-package htmlize org-link-minor-mode elcord telephone-line smart-tabs-mode cider projectile better-defaults clojure-mode zenburn-theme challenger-deep-theme haskell-mode neotree web-mode json-mode flycheck js2-mode spaceline spacemacs-theme)))
 '(pdf-view-midnight-colors (quote ("#b2b2b2" . "#292b2e")))
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map
   (quote
    ((20 . "#BC8383")
     (40 . "#CC9393")
     (60 . "#DFAF8F")
     (80 . "#D0BF8F")
     (100 . "#E0CF9F")
     (120 . "#F0DFAF")
     (140 . "#5F7F5F")
     (160 . "#7F9F7F")
     (180 . "#8FB28F")
     (200 . "#9FC59F")
     (220 . "#AFD8AF")
     (240 . "#BFEBBF")
     (260 . "#93E0E3")
     (280 . "#6CA0A3")
     (300 . "#7CB8BB")
     (320 . "#8CD0D3")
     (340 . "#94BFF3")
     (360 . "#DC8CC3"))))
 '(vc-annotate-very-old-color "#DC8CC3"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-block ((t (:background nil))))
 '(org-block-begin-line ((t (:underline nil :foreground "#858FA5" :background nil))))
 '(org-block-end-line ((t (:overline nil :foreground "#858FA5" :background nil)))))

(setq inhibit-startup-screen t)
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(setq-default tab-width 2)
(when (version<= "26.0.50" emacs-version)
  (global-display-line-numbers-mode))
(setq ring-bell-function 'ignore)
(set-face-attribute 'default nil :font "FiraCode Nerd Font" :height 130)

(require 'ido)
(ido-mode t)

(setq backup-directory-alist `(("." . "~/.saves")))
(setq backup-by-copying t)

(setq-default indent-tabs-mode nil)

;; Javascript setup
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

(add-hook 'typescript-mode-hook #'setup-tide-mode)

;; Use prettier for formatting instead of tide
(use-package prettier-js
  :ensure t
  :after (tide)
  :hook ((js2-mode . prettier-js-mode)
         (web-mode . prettier-js-mode)
         (tide-mode . prettier-js-mode)))

(use-package tide
  :ensure t
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)))
         ;(before-save . prettier-js)))

;; JSX support
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
(add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "jsx" (file-name-extension buffer-file-name))
              (setup-tide-mode))))
;; configure jsx-tide checker to run after your default jsx checker
(require 'flycheck)
(flycheck-add-mode 'javascript-eslint 'web-mode)
(with-eval-after-load 'tide
  (flycheck-add-next-checker 'javascript-eslint 'jsx-tide 'append)) ; make sure tide is loaded before adding jsx-tide checker

;; Setup svelte
(require 'svelte-mode)

;; Setup neotree
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))

;; Setup all-the-icons
(require 'all-the-icons)

;; Setup lsp stuff (language server protocol)
(use-package lsp-mode
  :hook ((haskell-mode . lsp))
  :commands lsp)

(require 'yasnippet)
(yas-global-mode 1)

(use-package lsp-haskell
  :after lsp-clients
  :config
  (setq lsp-haskell-set-hlint t
        lsp-haskell-process-path-hie "haskell-language-server-wrapper"
        lsp-haskell-process-args-hie nil
        lsp-document-sync-method 'full
        tab-width 2))

;; Haskell setup
(require 'haskell-mode)
(require 'haskell-interactive-mode)
(require 'haskell-process)

(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)

;; Keybindings for Haskell
(define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
(define-key haskell-mode-map (kbd "C-`") 'haskell-interactive-bring)
(define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
(define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
(define-key haskell-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
(define-key haskell-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
(define-key haskell-mode-map (kbd "C-c c") 'haskell-process-cabal)

;; Setup company-mode
(require 'company)
(global-company-mode) ; Enable in all
;; Integrate with haskell-mode
(add-hook 'haskell-mode-hook
          (lambda ()
            (set (make-local-variable 'company-backends)
                 (append '((company-capf company-dabbrev-code))
                         company-backends))))

;; elcord
(require 'elcord)
(elcord-mode)

;; Org-mode
(require 'org)

;; The following lines are always needed.  Choose your own keys.
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-switchb)


; (add-hook 'org-mode-hook #'flyspell-mode)
(add-hook 'org-mode-hook #'ispell-minor-mode)

;; HTML export stuff
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "firefox")

(setenv "BROWSER" "firefox")

;; Latex export stuff

(setq org-latex-listings '('minted 'hyperref))

(require 'ox-latex)
(add-to-list 'org-latex-packages-alist '("" "minted" "" "hyperref"))

(unless (boundp 'org-latex-classes)
  (setq org-latex-classes nil))
(add-to-list 'org-latex-classes
             '("article"
               "\\documentclass{article}"
               ("\\section{%s}" . "\\section*{%s}")))

;; prolog
(add-to-list 'auto-mode-alist '("\\.pl\\'" . prolog-mode))

;; use-package
(require 'use-package)

;; parinfer

(use-package parinfer
  :ensure t
  :bind
  (("C-," . parinfer-toggle-mode))
  :init
  (progn
    (setq parinfer-extensions
          '(defaults       ; should be included.
             pretty-parens  ; different paren styles for different modes.
                                        ; evil           ; If you use Evil.
                                        ; lispy          ; If you use Lispy. With this extension, you should install Lispy and do not enable lispy-mode directly.
             paredit        ; Introduce some paredit commands.
             smart-tab      ; C-b & C-f jump positions and smart shift with tab & S-tab.
             smart-yank))   ; Yank behavior depend on mode.
    (add-hook 'emacs-lisp-mode-hook #'parinfer-mode)
    (add-hook 'clojure-mode-hook #'parinfer-mode)
    (add-hook 'emacs-lisp-mode-hook #'parinfer-mode)
    (add-hook 'common-lisp-mode-hook #'parinfer-mode)
    (add-hook 'scheme-mode-hook #'parinfer-mode)
    (add-hook 'lisp-mode-hook #'parinfer-mode)))

;; Flycheck clojure clj-kondo
(use-package flycheck-clj-kondo
  :ensure t)

(use-package clojure-mode
  :ensure t
  :config
  (require 'flycheck-clj-kondo))

(require 'python-mode)
(setq py-install-directory "/usr/bin/python3")
(add-to-list 'load-path py-install-directory)
(defun MY-LOCAL-SHELL ()
  "Start interactive shell."
  (interactive)
  (py-shell nil py-install-directory))

(defun load-into-py-shell ()
  "Start interactive shell with file loaded."
  (interactive)
  (py-shell-send-file buffer-file-name))

(define-key python-mode-map (kbd "C-c C-l") 'load-into-py-shell)

;; Load jedi auto-complete for python
(require 'jedi)
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:environment-virtualenv nil)
(setq jedi:complete-on-dot t)

(setq-local flycheck-python-pylint-executable "python3")

;; Eldoc
(global-eldoc-mode t)

;; Fira code mode
(use-package fira-code-mode
  :custom (fira-code-mode-disabled-ligatures '("#{" "#(" "#_" "#_(" "x"))
  :config (global-fira-code-mode))

;; Doom theme
(require 'doom-themes)
(load-theme 'doom-challenger-deep t)
(doom-themes-neotree-config)
(doom-themes-org-config)

;; Doom modeline
(require 'doom-modeline)

;; Customisations
(setq doom-modeline-icon t)
(setq doom-modeline-major-mode-color-icon t)
(setq doom-modeline-buffer-state-icon t)

;; Coq stuff
(require 'proof-general)
(require 'company-coq)

;; Open .v files with Proof-General's coq-mode
(require 'proof-site "~/.emacs.d/elpa/proof-general-20190618.1328/generic/proof-site.el")

;; Load company-coq when opening Coq files
(add-hook 'coq-mode-hook #'company-coq-mode)

;; Idris
(require 'idris-mode)

;; EasyPG
(setenv "GPG_AGENT_INFO" "emacs")

;; Spell config
(setq ispell-program-name "hunspell")
(setq ispell-cmd-args "a")
(setq ispell-local-dictionary "en_GB")

;; direnv config
(use-package direnv
 :config
 (direnv-mode))

;; Remove lock files
(setq create-lockfiles nil)

(provide '.emacs)
;;; .emacs ends here
