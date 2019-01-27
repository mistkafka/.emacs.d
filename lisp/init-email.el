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

(provide 'init-email)
