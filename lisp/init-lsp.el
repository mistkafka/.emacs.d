;; lsp mode
(require-package 'lsp-mode)
(lsp-mode t)

;; disable lsp-ui
;; (require-package 'lsp-ui)
;; (add-hook 'lsp-mode-hook 'lsp-ui-mode)
;; (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
;; (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)


;; 性能
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb
(setq lsp-idle-delay 0.500)
(setq lsp-print-performance t)


(provide 'init-lsp)
