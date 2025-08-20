;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;; (package! some-package)

(package! c3-mode
  :recipe (:host github
           :repo "c3lang/editor-plugins"
           :files ("emacs/*.el")))

(package! lean4-mode
  :recipe (:host github
           :repo "leanprover/lean4-mode"
           :files ("*.el" "data")))

(package! flycheck-clj-kondo)

(package! svelte-mode)

(package! unisonlang-mode)

(package! fish-mode)

(package! quarto-mode)

(package! restclient)

(package! yuck-mode)

(package! kubed
  :recipe (:host sourcehut
           :repo "eshel/kubed"
           :branch "master"))

(package! quarto-mode)

;; NOTE: It's possible to disable parts of lsp-ui as well see docs
;; https://emacs-lsp.github.io/lsp-mode/tutorials/how-to-turn-off/
;; (package! lsp-ui :disable t)

;; When developing on `lsp-mode' use the local repo
;; (package! lsp-mode :recipe (:local-repo "~/Projects/lsp-mode" :files ("*.el" "clients/*.el") :build (:not compile)))

(package! idris-mode
  :recipe (:host github
           :repo "idris-hackers/idris-mode"
           :branch "main"))

(package! idris2-mode
  :recipe (:host github
           :repo "idris-community/idris2-mode"
           :branch "main"))

(package! bqn-mode
  :recipe (:host github
           :repo "museoa/bqn-mode"
           :branch "trunk"))

(package! uiua-ts-mode
  :recipe (:host github
           :repo "crmsnbleyd/uiua-ts-mode"
           :branch "main"))

(package! pkl-mode
  :recipe (:host github
           :repo "sin-ack/pkl-mode"
           :branch "master")
  ;; Version 1.0.3
  :pin "c57fe374a9c57eee6432d0b449e410ab8dc40a89")

(package! flymake-actionlint)

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/raxod502/straight.el#the-recipe-format
;;(package! another-package
;;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;;(package! this-package
;;  :recipe (:host github :repo "username/repo"
;;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
;; (package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;;(package! builtin-package :recipe (:nonrecursive t))
;;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see raxod502/straight.el#279)
;;(package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
;;(package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
;;(unpin! pinned-package)
;; ...or multiple packages
;;(unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
;;(unpin! t)
