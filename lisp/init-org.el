(defun mistkafka/org-setup-todo-keywords ()
  
  (setq org-todo-keywords
	(quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d@/!)")
		(sequence "PROJECT(p)" "|" "DONE(d@/!)" "CANCELLED(c@/!)")
                (sequence "HABIT(h)" "HABIT+(+)" "HABIT-(-)" "|" "DONE(d@/!)" "CANCELLED(c@/!)"))))
  
  (setq org-todo-repeat-to-state "NEXT")
  
  )

(defun mistkafka/org-setup-archive ()

  ;; keep inherited tags when archive
  ;; https://lists.gnu.org/archive/html//emacs-orgmode/2011-01/msg01195.html
  (defadvice org-archive-subtree (before add-inherited-tags-before-org-archive-subtree activate)
    "add inherited tags before org-archive-subtree"
    (org-set-tags-to (org-get-tags-at)))
  
  )

(defun mistkafka/org-setup-org-crypt ()
  "加密文章：http://coldnew.github.io/blog/2013/07/13_5b094.htm"
  
  (require 'org-crypt)
  (org-crypt-use-before-save-magic)
  (setq org-crypt-tag-matcher "secret")
  (setq org-tags-exclude-from-inheritance (quote ("secret")))
  (setq org-crypt-key "193E92ED"))

(defun mistkafka/org-defun-alias-funs ()
  
  (defun org-agenda-add-file--mistkafka-alias ()
    (interactive)
    (org-agenda-file-to-front))

  (defun org-agenda-remove-file--mistkafka-alias ()
    (interactive)
    (org-remove-file))
  )

(defun mistkafka/org-indent-current-buffer ()
  (interactive)
  (org-indent-region (point-min) (point-max)))

(defun mistkafka/org-setup-hook ()
  (add-hook 'org-mode-hook
	    (lambda ()
	      (add-hook 'before-save-hook 'mistkafka/org-indent-current-buffer nil 'make-it-local))
	    )
  )

(setq org-emphasis-regexp-components
      ;; markup 记号前后允许中文，必须在org load之前执行。因为org里的defvar不用覆盖已有的值
      ;; 并且org在初始化的时候，就需要这个值了
      (list (concat " \t('\"{"            "[:nonascii:]")
            (concat "- \t.,:!?;'\")}\\["  "[:nonascii:]")
            " \t\r\n,\"'"
            "."
            1))
(defun mistkafka/org-setup-markup ()
  (setq org-match-substring-regexp
        (concat
         ;; 限制上标和下标的匹配范围，org 中对其的介绍见：(org) Subscripts and superscripts
         "\\([0-9a-zA-Zα-γΑ-Ω]\\)\\([_^]\\)\\("
         "\\(?:" (org-create-multibrace-regexp "{" "}" org-match-sexp-depth) "\\)"
         "\\|"
         "\\(?:" (org-create-multibrace-regexp "(" ")" org-match-sexp-depth) "\\)"
         "\\|"
         "\\(?:\\*\\|[+-]?[[:alnum:].,\\]*[[:alnum:]]\\)\\)")))

(defun mistkafka/org-setup-pomodoro ()
  (add-hook 'org-pomodoro-finished-hook
            (lambda ()
              (my/system-dialog "Pomodoro completed!" "Time for a break.")))
  (add-hook 'org-pomodoro-break-finished-hook
            (lambda ()
              (my/system-dialog "Pomodoro Short Break Finished" "Ready for Another?")))
  (add-hook 'org-pomodoro-long-break-finished-hook
            (lambda ()
              (my/system-dialog "Pomodoro Long Break Finished" "Ready for Another?")))
  (add-hook 'org-pomodoro-killed-hook
            (lambda ()
              (my/system-dialog "Pomodoro Killed" "One does not simply kill a pomodoro!"))))

(with-eval-after-load 'org
  (progn
    (mistkafka/org-setup-todo-keywords)
    (mistkafka/org-setup-archive)
    (mistkafka/org-setup-hook)
    (mistkafka/org-setup-org-crypt)
    (mistkafka/org-defun-alias-funs)
    (mistkafka/org-setup-markup)
    (mistkafka/org-setup-pomodoro)
    ))

(defun mistkafka/org-mode/inbox ()
  (interactive)
  (let ((new-inbox-item (ivy-read "Inbox: " nil)))
    (when new-inbox-item
      (with-current-buffer (find-file-noselect "~/gtd/index.org.gpg")
        (goto-char (org-find-property "IS-INBOX-HEADLINE"))
        (goto-char (org-end-of-subtree))
        (insert "\n** ")
        (insert new-inbox-item)
        (save-buffer)
        (message "添加成功！"))
      )))

(defconst weekdays-arr ["Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"])
(defun mistkafka/org-mode/new-journay ()
  (interactive)
  (find-file "~/gtd/journal.org.gpg")
  (goto-char (point-max))
  (insert "\n* ")
  (insert (format-time-string "%Y-%m-%d " (current-time)))
  (insert (aref weekdays-arr
                (- (string-to-number (format-time-string "%u" (current-time))) 1)))
  (insert "\n")
  (org-narrow-to-element))

(mistkafka/keyboard/bind "ti" 'mistkafka/org-mode/inbox)
(mistkafka/keyboard/bind "ta" 'org-agenda)
  

(provide 'init-org)
