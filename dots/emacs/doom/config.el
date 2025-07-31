;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "John Doe"
      user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font
      (font-spec
       :family "Victor Mono"
       :weight 'semi-bold
       :size 20)
      doom-variable-pitch-font
      (font-spec
       :family "Victor Mono"
       :weight 'semi-bold
       :size 14))

;; Neotree settings
(setq doom-neotree-enable-variable-pitch nil
      doom-themes-neotree-enable-variable-pitch nil
      doom-themes-neotree-file-icons t)

;; Display major mode icon in the modeline
(setq doom-modeline-major-mode-icon t)

;; Add line wrapping
(global-visual-line-mode t)

;; Nice size for the default window
(defun get-default-height ()
  (/ (- (display-pixel-height) 120)
     (frame-char-height)))

;; This is only necessary on macOS
(when (eq system-type 'darwin)
  (add-to-list 'default-frame-alist '(width . 140))
  (add-to-list 'default-frame-alist (cons 'height (get-default-height))))

;; Since I'm using a non-posix shell
(setq shell-file-name (executable-find "bash"))
(setq-default vterm-shell (executable-find "fish"))

;; Elpa/straight/melpa bug?
(setq package-install-upgrade-built-in t)

;; FIXME: This only works if `aspell' is installed globally
;; on the machine
;; (use-package! spell-fu
;;  ;; Set the default dictionary to british english
;;  :config (setq ispell-dictionary "en_GB"))

;; Add more default pairs to evil-surround
(use-package! evil-surround
  :config
  (setq-default evil-surround-pairs-alist
                (progn ;; FIXME: These don't work correctly for some reason
                  (dolist (elem (mapcar (lambda (c) `(,c . evil-surround-read-tag)) '(?= ?+ ?~)))
                    (push elem evil-surround-pairs-alist))
                  evil-surround-pairs-alist)))

;; Don't format on save for these modes
(setq +format-on-save-disabled-modes
      '(emacs-lisp-mode sql-mode clojure-mode tex-mode latex-mode org-msg-edit-mode python rjsx-mode js2-mode less-css-mode format-all-mode haskell-mode yaml-mode))

(use-package! apheleia
  :config
    ;; (apheleia-global-mode -1)
    ;; Don't respect identation config set in emacs
    (setq apheleia-formatters-respect-indent-level nil))
    ;; This isn't really needed with the config above
    ;; (setf (alist-get 'prettier-json apheleia-formatters)
    ;;       '("apheleia-npx" "prettier" "--stdin-filepath" filepath "--parser=json")))

;; Remove extra ligatures
(setq +ligatures-extras-in-modes nil)

;; Disable extra ligatures even though they're not
;; enabled through the seemingly broken `+extra' flag for the ligatures module
;; See issue: https://github.com/hlissner/doom-emacs/issues/5738
(eval-after-load 'clojure-mode '(setq clojure--prettify-symbols-alist nil))
(eval-after-load 'python '(setq python-prettify-symbols-alist nil))
(eval-after-load 'js2-mode '(setq js--prettify-symbols-alist nil))
(eval-after-load 'lisp-mode '(setq lisp-prettify-symbols-alist nil))

;; ------------------------------- LSP -------------------------------
;; Eglot global lsp servers config
;;
;; This syntax also works:
;; '((:haskell .
;;       (:formattingProvider "fourmolu"
;;        :maxCompletions 30)
;;
;; TIP: You can always check if the resulting json is valib by evaluating
;; (json-encode eglot-workspace-configuration)
;; and see the current loaded workspace configuration with `eglot-show-workspace-configuration'
(setq-default eglot-workspace-configuration
              '((haskell
                 (formattingProvider . "fourmolu")
                 (maxCompletions . 30))

                (typescript
                 (codeActionsOnSave))

                (nixd (formatting (command . ["nix" "fmt"])))))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(nix-mode . ("nixd"))))

;; Don't assume this is the only checker that flycheck should use
(use-package! flycheck-eglot
  :after (flycheck eglot)
  :custom (flycheck-eglot-exclusive nil))

;; Restrict eldoc popup window size to 1.
(setq-default eldoc-echo-area-use-multiline-p 1)

(use-package! eglot
  :custom (eglot-connect-timeout 30))

;; ------------------------------- END -------------------------------

(use-package! magit
  :custom (magit-diff-refine-hunk 'all))

(setq projectile-require-project-root t)

(use-package! haskell-mode
  :config (remove-hook! haskell-mode #'(eglot-ensure projectile-mode)))

(use-package! scala-mode
  :mode (("\\.mill\\'" . scala-mode)
         ("\\.sc\\'" . scala-mode)))

(use-package! yasnippet
  ;; Disable yasnippet globally
  :config (yas-global-mode -1))

(use-package! python
  ;; Enable yasnippet for python
  :config (add-hook! 'python-mode #'yas-minor-mode))

(use-package yaml-mode
  :config (add-hook 'yaml-mode-hook #'flymake-actionlint-action-load-when-actions-file))

;; Remove rainbow delimiters from lisp modes
(after! elisp-mode
  (remove-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode))

(after! clojure-mode
  (remove-hook 'clojure-mode-hook #'rainbow-delimiters-mode))

(after! racket-mode
  (remove-hook 'racket-mode-hook #'rainbow-delimiters-mode))

(after! geiser-mode
  (remove-hook 'geiser-mode-hook #'rainbow-delimiters-mode))

(after! scheme-mode-hook
  (remove-hook 'scheme-mode-hook #'rainbow-delimiters-mode))

(after! yuck-mode
  (add-hook! 'yuck-mode-hook #'parinfer-rust-mode))

(use-package! parinfer-rust-mode
  :custom
  (parinfer-rust-library
   (if (eq system-type 'darwin)
       "/Users/nvj/parinfer-rust-darwin.so"
       parinfer-rust-library)))

(use-package! kubed
  :config (map! :leader :desc "kubed" "k" #'kubed-prefix-map))

(map!
 :desc "Toggle comment for a line or region."
 :n "C-/" #'comment-line)

(use-package! unisonlang-mode
  :config
  (map! :after unisonlang-mode
        :map unisonlang-mode-map
        :localleader
        :n "f" #'unisonlang-mode-add-fold
        :n "u" #'unisonlang-mode-remove-fold))

(use-package! restclient
  :config
  (add-to-list 'auto-mode-alist '("\\.http\\'" . restclient-mode))
  (map! :after restclient
        :map restclient-mode-map
        :localleader
        :n "p" #'restclient-http-send-current
        :n "e" #'restclient-http-send-current-raw
        :n "n" #'restclient-jump-next
        :n "N" #'restclient-jump-prev
        :n "y" #'restclient-copy-curl-command))

(use-package! bqn-mode
  :config
  ;; Set BQN input method
  (after! bqn-mode
    (set-input-method "BQN-Z")
    (setq default-input-method "BQN-Z"))
  ;; Increase text scale in some BQN buffers
  (add-hook! '(bqn-mode-hook bqn-comint-mode-hook) (text-scale-set 3))
  (add-hook! 'bqn-mode
    (setq-local face-attribute 'default :family "BQN386 Unicode"))
  ;; Set BQN386 font in other bqn major modes. `bqn-mode' already uses the font by default
  (add-hook! '(bqn-comint-mode-hook bqn-keymap-mode-hook bqn-glyph-mode-hook)
    (face-remap-add-relative 'default '(:family "BQN386 Unicode")))
  ;; Keybindings for bqn-mode
  (map! :leader
        :after bqn-mode
        :map bqn-mode-map
        :localleader
        :n "s d" #'bqn-help-symbol-info-at-point
        :n "s n" #'bqn-help-symbol-at-point-is-called
        :n "s k" #'bqn-keymap-mode-show-keyboard
        :n "s g" #'bqn-glyph-mode-show-glyphs
        :n "x b" #'bqn-comint-send-buffer
        :n "x l" #'bqn-comint-send-dwim))

(use-package! uiua-ts-mode
  :mode "\\.ua\\'"
  :ensure t
  :config
  ;; Register uiua LSP sever in eglot
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 '(uiua-ts-mode . ("uiua" "lsp"))))
  (add-hook! '(uiua-ts-mode-hook uiua-mode-hook)
    (list
     ;; Use Uiua386 font in uiua buffers
     (face-remap-add-relative 'default '(:family "Uiua386"))
     ;; Autostart eglot
     (eglot-ensure)
     ;; Increase text scale
     (text-scale-set 3))))

(use-package! idris2-mode
  :config
  (add-hook! 'idris2-mode (lambda () (company-mode 0)))
  (map! :localleader
        :map idris2-mode-map
        "p" #'idris2-proof-search
        "c" #'idris2-case-dwim
        "a l" #'idris2-make-lemma
        "l" #'idris2-load-file
        "a c" #'idris2-add-clause
        "t" #'idris2-type-at-point
        "d" #'idris2-jump-to-def-same-window))

;; Make format errors popup small and escapable
(set-popup-rule! "*format-all-errors*" :ttl 0 :quit t)

;; ------------------------- Modeline and file explorer icons -------------------------

;; Associate the .pl file extension with prolog and not the default perl
(progn
  (rassq-delete-all 'perl-mode auto-mode-alist)
  (add-to-list 'auto-mode-alist '("\\.pl\\'" . prolog-mode)))

;; When getting the file icon for the buffer *only* use the
;; major-mode to determine the correct icon to use, not the
;; file extension.
(eval-after-load 'all-the-icons
  '(defun all-the-icons--icon-info-for-buffer (&optional f)
     "Get icon info for the current buffer.

When F is provided, the info function is calculated with the format
`all-the-icons-icon-%s-for-file' or `all-the-icons-icon-%s-for-mode'."
     (let* ((base-f (concat "all-the-icons-icon" (when f (format "-%s" f))))
            (mode-f (intern (concat base-f "-for-mode"))))
       (funcall mode-f major-mode))))

;; When getting the file icon for `ivy-mode' file explorer hard code
;; the prolog icon for files that end with ".pl"
(eval-after-load 'all-the-icons-ivy
  '(defun all-the-icons-ivy-icon-for-file (s)
     "Return icon for filename S.
  Return the octicon for directory if S is a directory.
  Otherwise fallback to calling `all-the-icons-icon-for-file'."
     (cond
      ((string-match-p "\\/$" s)
       (all-the-icons-octicon "file-directory" :face 'all-the-icons-ivy-dir-face))
      ((string-match-p ".*\\.pl" s) (all-the-icons-icon-for-mode 'prolog-mode))
      (t (all-the-icons-icon-for-file s)))))

;; --------------------------------------- END ---------------------------------------

;; Disable italics in tree-sitter font face
(use-package! tree-sitter
  :config (set-face-italic 'tree-sitter-hl-face:property nil))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-ayu-mirage)

;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; Latex captions underneath blocks
(setq org-latex-caption-above nil)

(defvar flymake-allowed-file-name-masks nil)

;; Set tsx files to use rjsx-mode
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . rjsx-mode))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
