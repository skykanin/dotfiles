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

;; Add line wrapping
(global-visual-line-mode t)

;; FIXME: This only works if `aspell' is installed globally
;; on the machine
(use-package! spell-fu
  ;; Set the default dictionary to british english
  :config (setq ispell-dictionary "en_GB"))

;; Add more default pairs to evil-surround
(use-package! evil-surround
  :config
  (setq-default evil-surround-pairs-alist
    (progn ;; These don't work correctly
      (dolist (elem '((?\= . ("=" . "=")) (?\+ . ("+" . "+"))))
            (push elem evil-surround-pairs-alist))
      evil-surround-pairs-alist)))

;; Configure elcord
(use-package! elcord
  :config (elcord-mode)
  :custom (elcord-use-major-mode-as-main-icon t)
          (elcord-refresh-rate 10)
          ;; NOTE: Bypass discord API URL caching?
          (elcord-icon-base "https://raw.githubusercontent.com/skykanin/elcord/master/icons/"))

;; Don't format on save for these modes
(setq +format-on-save-enabled-modes
      '(not emacs-lisp-mode sql-mode clojure-mode tex-mode latex-mode org-msg-edit-mode python rjsx-mode js2-mode less-css-mode format-all-mode))

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
(setq-default eglot-workspace-configuration
  '((haskell
      (formattingProvider . "fourmolu")
      (maxCompletions . 30))))

;; Restrict eldoc popup window size to 1.
(setq-default eldoc-echo-area-use-multiline-p 1)

;; ------------------------------- END -------------------------------

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

;; Display major mode icon in the modeline
(setq doom-modeline-major-mode-icon t)

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

(use-package! bqn-mode
  :config
  ;; Set BQN input method
  (after! bqn-mode
    (set-input-method "BQN-Z")
    (setq default-input-method "BQN-Z"))
  ;; Set BQN386 font in other bqn major modes. `bqn-mode' already uses the font by default
  (add-hook! '(bqn-keymap-mode-hook bqn-glyph-mode-hook)
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

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-dracula)

;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; Latex captions underneath blocks
(setq org-latex-caption-above nil)

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
