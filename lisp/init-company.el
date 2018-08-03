(require-package 'company)
(add-hook 'after-init-hook 'global-company-mode)

;; 配置company，作为lsp的补全前端
(require-package 'company-lsp)

(push 'company-yasnippet company-backends)
(push 'company-lsp company-backends)    ; company-lsp的优先级要放高一点，不然会补全不了

(global-set-key (kbd "M-c") 'company-complete)

(provide 'init-company)
