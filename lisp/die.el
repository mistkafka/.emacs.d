;;;; 放置一些尝试过的、但未成功的代码片段



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
