;; 因为有冲突，所以先注释 https://emacs-china.org/t/topic/5492/2
;; ;; (require-package 'lsp-css)
;; (defconst lsp-css--get-root
;;   (lsp-make-traverser #'(lambda (dir)
;;                           (directory-files dir nil "package.json"))))

;; (lsp-define-stdio-client
;;  lsp-css
;;  "css"
;;  lsp-css--get-root
;;  '("css-languageserver" "--stdio"))
;; (add-hook 'css-mode-hook #'lsp-css-enable)
;; (add-hook 'less-mode-hook #'lsp-css-enable)
;; (add-hook 'sass-mode-hook #'lsp-css-enable)
;; (add-hook 'scss-mode-hook #'lsp-css-enable)

(provide 'init-css)
