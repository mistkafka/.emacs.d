
;; 不要backup文件了！哪天吃了苦头再说
(setq make-backup-files nil)

;; 书签存储地址
(setq bookmark-default-file "~/.emacs.d/.cache/bookmarks")

;; 默认启动ivy-mode
(require-package 'ivy)
(require-package 'counsel)
(ivy-mode 1)
(when (commandp 'counsel-M-x)
  (global-set-key [remap execute-extended-command] #'counsel-M-x))


;; 默认启动which-key-mode
(require-package 'which-key)
(which-key-mode)

;; 我更喜欢带正则表达式的search
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)

;; magit的按键绑定
(require-package 'magit)
(mistkafka/keyboard/bind "gs" 'magit-status)
(mistkafka/keyboard/bind "gp" 'magit-push-current-to-upstream)
(mistkafka/keyboard/bind "ghb" 'magit-blame)

(mistkafka/keyboard/bind "gbn" 'magit-branch-and-checkout)
(mistkafka/keyboard/bind "gbc" 'magit-branch-checkout)

(mistkafka/keyboard/bind "gmm" 'magit-merge)
(mistkafka/keyboard/bind "gma" 'magit-merge-abort)
(mistkafka/keyboard/bind "gmc" 'magit-merge) ;magit-merge-continue

(mistkafka/keyboard/bind "grr" 'magit-rebase)
(mistkafka/keyboard/bind "gri" 'magit-rebase-interactive)

;; 复制黏贴, 在终端里依然不work
;; (global-set-key (kbd "S-c") 'clipboard-kill-region)
;; (global-set-key (kbd "S-v") 'clipboard-yank)

;; layout管理器
(require-package 'eyebrowse)
(eyebrowse-mode t)
(mistkafka/keyboard/bind "l" 'eyebrowse-switch-to-window-config)

;; flycheck语法检查器
(require-package 'flycheck)

;; lsp mode
(require-package 'lsp-mode)
(lsp-mode t)

(require-package 'lsp-ui)
(add-hook 'lsp-mode-hook 'lsp-ui-mode)
(define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
(define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
(add-hook 'typescript-mode-hook 'flycheck-mode)
(add-hook 'web-mode-hook 'flycheck-mode)

;; (require-package 'lsp-javascript-typescript)
;; (require 'lsp-typescript)
;; (add-hook 'typescript-mode-hook #'lsp-typescript-enable) ;; for typescript support
;; (add-hook 'js-mode-hook #'lsp-typescript-enable)
;; (add-hook 'web-mode-hook #'lsp-typescript-enable) ;; *.tsx
;; (add-hook 'js2-mode-hook #'lsp-typescript-enable) ;; for js2-mode support
;; (add-hook 'rjsx-mode #'lsp-typescript-enable) ;; for rjsx-mode support
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
(add-hook 'web-mode-hook #'lsp-typescript-enable) ;; *.tsx

(require-package 'lsp-python)
(add-hook 'python-mode-hook #'lsp-python-enable)

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

;; 配置company，作为lsp的补全前端
(require-package 'company)
(add-hook 'after-init-hook 'global-company-mode)
(require-package 'company-lsp)
(push 'company-lsp company-backends)

;; ert测试
(defun mistkafka/misc/ert-test-current-buffer ()
  (interactive)
  (eval-buffer)
  (ert t))

;; ssh远程链接(tramp)
(require 'tramp-cache)
(setq tramp-persistency-file-name "/Users/mistkafka/.emacs.d/.cache/tramp")

;; theme
(require-package 'doom-themes)
(setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
      doom-themes-enable-italic t) ; if nil, italics is universally disabled
(load-theme 'doom-one-light t)
(doom-themes-org-config)

;; yasnippet配置
(require-package 'yasnippet)
(require 'yasnippet)
(require 'yasnippet-snippets)
(yas-global-mode 1)
(yas-reload-all)
;; (require 'company-yasnippet)
;; (push 'company-yasnippet company-backends)


;; auto-complate
(require-package 'auto-complete)
(ac-config-default)
(setq-default ac-sources '(ac-source-abbrev
                           ac-source-dictionary
                           ac-source-words-in-same-mode-buffers
			   ac-source-yasnippet))

;; cnfonts
(require-package'cnfonts)
(cnfonts-enable)

;; js-doc
(setq js-doc-mail-address "zhenguolin@me.com"
      js-doc-author (format "MistKafka <%s>" js-doc-mail-address)
      js-doc-url "https://mistkafka.github.io"
      js-doc-license "MIT")

(add-hook 'typescript-mode-hook
          #'(lambda ()
              (define-key typescript-mode-map "\C-ci" 'js-doc-insert-function-doc)
              (define-key typescript-mode-map "@" 'js-doc-insert-tag)))

;; rename current file and buffer

(provide 'init-misc)
