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

;; Add line wrapping
(global-visual-line-mode t)

;; Configure elcord
(use-package! elcord
  :config (elcord-mode))

;; Configure flycheck-clj-kondo
(use-package! flycheck-clj-kondo)

;; Configure svelte
(use-package! svelte-mode
  :config
  (setq lsp-semantic-tokens-enable t)
  (after! lsp-mode
    (add-to-list 'lsp-language-id-configuration '(svelte-mode . "svelte"))

    (lsp-register-client
     (make-lsp-client
      :new-connection (lsp-stdio-connection "svelte-language-server")
      :major-modes '(svelte-mode)
      :server-id 'svelte-language-server))))

;; Configure lsp-haskell
(setq lsp-haskell-formatting-provider "fourmolu")

;; Don't format on save for these modes
(setq +format-on-save-enabled-modes
      '(not emacs-lisp-mode sql-mode clojure-mode tex-mode latex-mode org-msg-edit-mode python rjsx-mode js2-mode less-css-mode format-all-mode))

;; Disable extra ligatures even though they're not
;; enabled through the seemingly broken `+extra' flag for the ligatures module
;; See issue: https://github.com/hlissner/doom-emacs/issues/5738
(eval-after-load 'clojure-mode '(setq clojure--prettify-symbols-alist nil))
(eval-after-load 'python '(setq python-prettify-symbols-alist nil))
(eval-after-load 'js2-mode '(setq js--prettify-symbols-alist nil))
(eval-after-load 'lisp-mode '(setq lisp-prettify-symbols-alist nil))

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

(use-package! lsp-mode
  :defer
  :hook
  ((elm-mode . lsp-deferred))

  :init
  (with-eval-after-load 'lsp-mode
    ;; To avoid watching all Scrive API docs.
    (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]api_docs\\'" t)
    (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]_build-adminonly\\'" t)
    (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]_local\\'" t)))

;; Make the LSP shut the fuck up with prompts
(setq! +lsp-prompt-to-install-server 'quiet)

;; Temporary config to stop HLS from prompting restart
;; when it crashes which is constantly
;; (remove-hook 'haskell-mode-local-vars-hook #'lsp!)
;; Remember to add the hook again if you remove it !!!
;; (add-hook 'haskell-mode-local-vars-hook #'lsp!)

;; Make format errors popup small and escapable
(set-popup-rule! "*format-all-errors*" :ttl 0 :quit t)

;; Associate the .pl file extension with prolog and not the default perl
(add-to-list 'auto-mode-alist '("\\.pl\\'" . prolog-mode))

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
