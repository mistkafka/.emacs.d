(defconst
  lsp-typescript--get-root
  (lsp-make-traverser #'(lambda (dir)
			  (directory-files dir nil "tsconfig.json\\|package.json")
			  )
		      ))
(lsp-define-stdio-client
 lsp-typescript
 "javascript"
 lsp-typescript--get-root
 '("typescript-language-server" "--stdio"))

(add-hook 'typescript-mode-hook #'lsp-typescript-enable) ;; for typescript support

(require-package 'web-mode)
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-hook 'web-mode-hook #'lsp-typescript-enable) ;; *.tsx

(provide 'init-typescript)
