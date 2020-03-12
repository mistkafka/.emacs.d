;; lsp mode
(require-package 'lsp-mode)
(lsp-mode t)

;; (require-package 'lsp-ui)
;; (add-hook 'lsp-mode-hook 'lsp-ui-mode)
;; (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
;; (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)

(provide 'init-lsp)
