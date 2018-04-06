(require-package 'web-mode)
(require-package 'typescript-mode)
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))

(provide 'init-typescript)
