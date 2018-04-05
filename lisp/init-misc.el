
;; 不要backup文件了！哪天吃了苦头再说
(setq make-backup-files nil)

;; 书签存储地址
(setq bookmark-default-file "~/.emacs.d/.cache/bookmarks")

;; 默认启动ivy-mode
(require 'ivy)
(ivy-mode 1)

;; 默认启动which-key-mode
(require 'which-key)
(which-key-mode)

;; 我更喜欢带正则表达式的search
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)

;; magit的按键绑定
(require 'magit)
(mistkafka/keyboard/bind "gs" 'magit-status)
(mistkafka/keyboard/bind "gp" 'magit-push-current-to-upstream)

;; 复制黏贴, 在终端里依然不work
;; (global-set-key (kbd "S-c") 'clipboard-kill-region)
;; (global-set-key (kbd "S-v") 'clipboard-yank)

;; layout管理器
(require 'eyebrowse)
(eyebrowse-mode t)
(mistkafka/keyboard/bind "l" 'eyebrowse-switch-to-window-config)

;; flycheck语法检查器
(require 'flycheck)

;; lsp mode
(require 'lsp-mode)
(lsp-mode t)

(require 'lsp-ui)
(add-hook 'lsp-mode-hook 'lsp-ui-mode)
(define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
(define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
(add-hook 'typescript-mode-hook 'flycheck-mode)
(add-hook 'web-mode-hook 'flycheck-mode)

(require 'lsp-typescript)
(add-hook 'typescript-mode-hook #'lsp-typescript-enable) ;; for typescript support
(add-hook 'js-mode-hook #'lsp-typescript-enable)
(add-hook 'web-mode-hook #'lsp-typescript-enable) ;; *.tsx
(add-hook 'js2-mode-hook #'lsp-typescript-enable) ;; for js2-mode support
(add-hook 'rjsx-mode #'lsp-typescript-enable) ;; for rjsx-mode support

;; (require 'lsp-python)
(lsp-define-stdio-client lsp-python "python"
			 (lsp-make-traverser #'(lambda (dir)
						 (when (directory-files dir nil ".git") ;hardcode，只有项目root目录才可以是python root
						   (directory-files
						    dir
						    nil
						    "\\(__init__\\|setup\\)\\.py"))))
			 '("pyls"))
(add-hook 'python-mode-hook #'lsp-python-enable)

;; (require 'lsp-css)
(defconst lsp-css--get-root
  (lsp-make-traverser #'(lambda (dir)
                          (directory-files dir nil "package.json"))))

(lsp-define-stdio-client
 lsp-css
 "css"
 lsp-css--get-root
 '("css-languageserver" "--stdio"))
(add-hook 'css-mode-hook #'lsp-css-enable)
(add-hook 'less-mode-hook #'lsp-css-enable)
(add-hook 'sass-mode-hook #'lsp-css-enable)
(add-hook 'scss-mode-hook #'lsp-css-enable)


(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
(require 'company-lsp)
(push 'company-lsp company-backends)

(provide 'init-misc)
