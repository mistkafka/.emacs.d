(require-package 'company)
(add-hook 'after-init-hook 'global-company-mode)

;; 配置company，作为lsp的补全前端
(require-package 'company-lsp)
(push 'company-lsp company-backends)

(global-set-key (kbd (format "M-c")) 'company-complete)

(provide 'init-company)
