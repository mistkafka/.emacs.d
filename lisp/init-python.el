;; 需要安装pyls: https://github.com/palantir/python-language-server
(require-package 'lsp-python)
(add-hook 'python-mode-hook #'lsp-python-enable)

(provide 'init-python)
