
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
;; (add-hook 'typescript-mode-hook 'flycheck-mode)
;; (add-hook 'web-mode-hook 'flycheck-mode)

;; (require-package 'lsp-javascript-typescript)
;; (require 'lsp-typescript)
;; (add-hook 'typescript-mode-hook #'lsp-typescript-enable) ;; for typescript support
;; (add-hook 'js-mode-hook #'lsp-typescript-enable)
;; (add-hook 'web-mode-hook #'lsp-typescript-enable) ;; *.tsx
;; (add-hook 'js2-mode-hook #'lsp-typescript-enable) ;; for js2-mode support
;; (add-hook 'rjsx-mode #'lsp-typescript-enable) ;; for rjsx-mode support
;; (defconst
;;   lsp-typescript--get-root
;;   (lsp-make-traverser #'(lambda (dir)
;; 			  (directory-files dir nil "tsconfig.json\\|package.json")
;; 			  )
;; 		      ))
;; (lsp-define-stdio-client
;;  lsp-typescript
;;  "javascript"
;;  lsp-typescript--get-root
;;  '("typescript-language-server" "--stdio"))
;; (add-hook 'typescript-mode-hook #'lsp-typescript-enable) ;; for typescript support
;; (add-hook 'web-mode-hook #'lsp-typescript-enable) ;; *.tsx

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

;; Apple
(defun mistkafka/utils/get-region-str ()
  (buffer-substring-no-properties
   (region-beginning)
   (region-end)))

(defun mistkafka/osx/speech ()
  "Speech region content.
Or word at point.
Or prompt user input."
  (interactive)
  (let ((content))
    (cond ((region-active-p) (setq content (mistkafka/utils/get-region-str)))
          ((word-at-point) (setq content (word-at-point)))
          (t (setq content (ivy-read "Speech Content: " nil))))

    (call-process "osascript"
                  nil 0 nil
                  "-e" (format "say \"%s\"" content))))

;; git emoji
(require 'misc-config)
(defun mistkafka/counsel-git-emoji ()
  "search and insert gitmoji"
  (interactive)
  (let (emoji-selected-raw-str
        emoji)
    (setq emoji-selected-raw-str (ivy-read "%d search gitmoji: " MISTKAFKA-GITMOJIS))
    (setq emoji (nth 1 (s-match "^\\(:[a-zA-Z_]*:\\) .*" emoji-selected-raw-str)))
    (insert emoji)
    ))

;; no tab
(setq-default indent-tabs-mode nil)


(defun recovery-my-leader-key ()
  "移除magit的`M-n'绑定，这是我的 leader-key"
  (interactive)
  (local-set-key (kbd "M-n") nil))

(add-hook 'magit-mode-hook
          'recovery-my-leader-key)
;; 移除term-mode的"M-n"绑定
(add-hook 'term-mode-hook
          'recovery-my-leader-key)

;; common lisp
(setq inferior-lisp-program "/usr/local/bin/sbcl")
(setq slime-contribs '(slime-fancy))
;; (load (expand-file-name "~/quicklisp/slime-helper.el"))

;; 禁止吵死人的bell
(setq visible-bell nil)
(setq ring-bell-function 'ignore)

;; 工具函数
(defun my/system-dialog (title message)
  (call-process "osascript"
                nil 0 nil
                "-e" (format "display dialog \"%s\" with title \"%s\"" message title)))

;; window编号
(require-package 'window-numbering)
(window-numbering-mode 1)

;; 黏贴板设置，我不喜欢s-c、s-v与emacs的复制黏贴参合在一块
(setq select-enable-clipboard   nil
      x-select-enable-clipboard nil)

(defun mistkafka/clipboard/write-clipboard (str)
  (shell-command-to-string (format "printf \"%s\" | pbcopy" str)))

(defun mistkafka/clipboard/get-paste-str ()
  (shell-command-to-string "pbpaste"))

(defun mistkafka/clipboard/copy-region ()
  (interactive)
  (mistkafka/clipboard/write-clipboard (mistkafka/utils/get-region-str)))

(defun mistkafka/clipboard/paste ()
  (interactive)
  (insert (mistkafka/clipboard/get-paste-str)))

(global-unset-key (kbd "s-c"))
(global-unset-key (kbd "s-v"))
(global-set-key (kbd "s-c") 'mistkafka/clipboard/copy-region)
(global-set-key (kbd "s-v") 'mistkafka/clipboard/paste)

;; term-mode相关配置
(defun mistkafka/term-mode/send-init-command ()
  "source ~/.bash_profile来初始化终端环境"
  (interactive)
  (term-send-raw-string "source ~/.bash_profile")
  (term-send-raw-string "\C-m")
  )

(defun mistkafka/term-mode/toggle-term-line-char-mode ()
  (interactive)
  (cond
   ((term-in-char-mode) (progn
                          (term-line-mode)
                          (message "切换到line-mode")))
   ((term-in-line-mode) (progn
                          (term-char-mode)
                          (message "切换到char-mode")))
    )
   )

(setq mistkafka/term-mode/gll-term-names '("server" "mysql" "tsc" "terminal"))

(defun mistkafka/term-mode/init-gll-terms-group ()
  (interactive)
  (cl-loop for term-name in mistkafka/term-mode/gll-term-names
           do (ansi-term "/bin/bash" term-name))
  (run-at-time "0.5" nil 'mistkafka/term-mode/do-send-init-command))

(defun mistkafka/term-mode/reset-keybind ()
  (interactive)
  (recovery-my-leader-key)
  (local-set-key (kbd "M-x") nil))

(defun mistkafka/term-mode/do-send-init-command ()
  (cl-loop for term-name in mistkafka/term-mode/gll-term-names
           for bffr-name = (format "*%s*" term-name)
           do (when bffr-name
                (with-current-buffer bffr-name
                  (rename-buffer term-name)
                  (mistkafka/term-mode/send-init-command)
                  (mistkafka/term-mode/reset-keybind)))))


;; 邮箱配置，http://www.ict4g.net/adolfo/notes/2014/12/27/emacs-imap.html

(add-to-list 'load-path "/usr/local/Cellar/mu/HEAD-b952724/share/emacs/site-lisp/mu/mu4e/")
(require 'mu4e)
;; tell mu4e where my Maildir is
(setq mu4e-maildir "/Users/mistkafka/.maildir")
;; tell mu4e how to sync email
(setq mu4e-get-mail-command "/usr/local/Cellar/isync/1.3.0/bin/mbsync -a")
;; tell mu4e to use w3m for html rendering
;; (setq mu4e-html2text-command "/usr/local/Cellar/w3m/0.5.3_5/bin/w3m -T text/html")
;; (setq mu4e-html2text-command nil)
(setq mu4e-view-prefer-html t)

;; taken from mu4e page to define bookmarks，这个我不知道有什么用
(add-to-list 'mu4e-bookmarks
             '("size:5M..500M"       "Big messages"     ?b))
;; mu4e requires to specify drafts, sent, and trash dirs
;; a smarter configuration allows to select directories according to the account (see mu4e page)
(setq mu4e-drafts-folder "/Drafts")
(setq mu4e-sent-folder "/Sent")
(setq mu4e-trash-folder "/Trash")
(add-hook 'mu4e-view-mode-hook
  (lambda()
    ;; try to emulate some of the eww key-bindings
    (local-set-key (kbd "<tab>") 'shr-next-link)
    (local-set-key (kbd "<backtab>") 'shr-previous-link)))

(mistkafka/keyboard/bind "mm" 'mu4e)
(mistkafka/keyboard/bind "mu" 'mu4e-update-mail-and-index)

;; editor config
(require-package 'editorconfig)
(editorconfig-mode 1)

;; 开启行号
(global-linum-mode +1)

(provide 'init-misc)
