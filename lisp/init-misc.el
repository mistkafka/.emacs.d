
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

;; flycheck语法检查器
(require-package 'flycheck)

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
(require-package 'yasnippet)
(require-package 'yasnippet-snippets)
(yas-global-mode 1)
(yas-reload-all)

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
                  (linum-mode -1)
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

;; magit扩展工具
(defun mistkafka/magit/blame-try-open-gll-project-task ()
  (interactive)
  (let* ((hash (magit-blame-chunk-get :hash))
         (commit-message (shell-command-to-string (format "git log --format=\"%%B\" -n 1 %s" hash)))
         (project-task-id (string-to-number (nth 1 (s-match "\\[\\#\\([0-9]+\\)\\]" commit-message)))))
    (gll/open-project-task project-task-id)))

;; poor kill-ring-show
(setq mistkafka/kill-ring/snippets nil)
(defun mistkafka/kill-ring/empty ()
  (interactive)
  (setq mistkafka/kill-ring/snippets nil)
  (message "黏贴板：已清空！"))

(defun mistkafka/kill-ring/add (snippet)
  (setq mistkafka/kill-ring/snippets
        (cons snippet mistkafka/kill-ring/snippets))
  (message "黏贴板：添加成功！"))

(defun mistkafka/kill-ring/add--from-latest-kill-ring ()
  (interactive)
  (mistkafka/kill-ring/add
   (substring-no-properties
    (nth 0 kill-ring))))

(defun mistkafka/kill-ring/insert ()
  (interactive)
  (let ((selected (ivy-read "Select: "  mistkafka/kill-ring/snippets)))
    (when selected
      (insert selected))))

(defun mistkafka/kill-ring/copy ()
  (interactive)
  (let ((selected (ivy-read "Select: "  mistkafka/kill-ring/snippets)))
    (when selected
      (kill-new selected)
      (message "黏贴板：复制成功！"))))

(mistkafka/keyboard/bind "ke" 'mistkafka/kill-ring/empty)
(mistkafka/keyboard/bind "ka" 'mistkafka/kill-ring/add--from-latest-kill-ring)
(mistkafka/keyboard/bind "ki" 'mistkafka/kill-ring/insert)
(mistkafka/keyboard/bind "kc" 'mistkafka/kill-ring/copy)

(provide 'init-misc)
