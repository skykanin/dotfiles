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
(setq doom-font (font-spec
                 :family "VictorMono Nerd Font"
                 :weight 'semi-bold
                 :size 18)
       doom-variable-pitch-font (font-spec :family "sans" :size 15))

;; Add line wrapping
(global-visual-line-mode t)

;; Configure elcord
(use-package! elcord
  :config (elcord-mode))

;; Configure flycheck-clj-kondo
(use-package! flycheck-clj-kondo)

;; Configure lsp-haskell
(setq lsp-haskell-formatting-provider "fourmolu")

;; Don't format on save for these modes
(setq +format-on-save-enabled-modes '(not emacs-lisp-mode sql-mode clojure-mode haskell-mode tex-mode latex-mode org-msg-edit-mode python-mode))

(use-package! idris-mode
  :mode ("\\.l?idr\\'" . idris-mode)
  :config

  (after! lsp-mode
    (add-to-list 'lsp-language-id-configuration '(idris-mode . "idris2"))

    (lsp-register-client
     (make-lsp-client
      :new-connection (lsp-stdio-connection "idris2-lsp")
      :major-modes '(idris-mode)
      :server-id 'idris2-lsp)))

  (setq lsp-semantic-tokens-enable t)

  (add-hook 'idris-mode-hook #'lsp!))

;; Make sure haskell and clojure REPL windows open on the right side
;; of the current buffer

;; (setq repl-options
;;   '(:side 'right
;;     :actions '(#'display-buffer-in-side-window)
;;     :select t
;;     :modeline t
;;     :size 0.5
;;     :quit 'current)
;;
;; (set-popup-rule! "^\\*cider-repl" :side 'right :actions '(#'display-buffer-in-side-window) :select t :modeline t :size 0.5 :quit 'current)
;; (set-popup-rule! "\\*haskell\\*" :side 'right :actions '(#'display-buffer-in-side-window) :select t :modeline t :size 0.5 :quit 'current)
;; (set-popup-rule! "haskell" :ignore t)

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-gruvbox)

;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; Latex captions underneath blocks
(setq org-latex-caption-above nil)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)


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
