(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("01cf34eca93938925143f402c2e6141f03abb341f27d1c2dba3d50af9357ce70" "188fed85e53a774ae62e09ec95d58bb8f54932b3fd77223101d036e3564f9206" default))
 '(safe-local-variable-values
   '((ispell-dictionary . "en_GB")
     (lsp-haskell-formatting-provider . "fourmolu")
     (haskell-mode-stylish-haskell-path . "fourmolu")
     (lsp-haskell-plugin-import-lens-code-actions-on . f)
     (haskell-initialisation-options :haskell
                                     (:plugin
                                      (:importLens
                                       (:globalOn "false"))))
     (lsp-ui-doc-enable)
     (lsp-ui-doc-show-with-cursor)
     (lsp-file-watch-threshold . 20000)
     (haskell-mode-stylish-haskell-path . "brittany")
     (haskell-stylish-on-save . t)
     (lsp-haskell-formatting-provider . "brittany"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
