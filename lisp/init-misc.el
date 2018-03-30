
;; 不要backup文件了！哪天吃了苦头再说
(setq make-backup-files nil)

;; 书签存储地址
(setq bookmark-default-file "~/.emacs.d/.cache/bookmarks")

;; 默认启动ivy-mode
(ivy-mode 1)

;; 我更喜欢带正则表达式的search
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)

(provide 'init-misc)
