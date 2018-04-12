(require 'gll-config)

(defun gll/fetch-project-task-detail (id)
  (let* ((json-object-type 'hash-table)
         (json-array-type 'list)
         (json-key-type 'string))
    (nth 0 (json-read-from-string
            (shell-command-to-string (format "gllue-cli project-task-detail %s" id))))))

(defun gll/auto-gen-commit-message ()
  (interactive)
  (let* ((branch (magit-get-current-branch))
         (branch-parts (split-string branch "-"))
         (emoji (nth 1 branch-parts))
         (project-task-id (nth 2 branch-parts)))
    (insert (format ":%s: [#%s] " emoji project-task-id))
    (if (not (equal 0 (string-to-number project-task-id)))
        (insert (gethash "title" (gll/fetch-project-task-detail project-task-id)))
      (message "不是合法的Project ID，跳过拉取任务Title。"))))

(defun gll/open-project-task(id &optional browser)
  "Open gllue project task detail of ID.
If BROWSER is provated, use the BROWSER open the link."
  (interactive "nProject Task Id:")
  (let* ((url (format "%s%d" PROJECT-ROOT-URL id))
         (cmd (format "open \"%s\"" url)))
    (unless browser
      (setq browser "Safari"))
    (setq cmd (format "%s -a \"%s\"" cmd browser))
    (shell-command cmd)))

(defun gll/open-project-task-at-point()
  (interactive)
  (let ((id (thing-at-point 'word)))
    (when id
        (setq id (string-to-number id)))
    (if (not (equal 0 id))
        (gll/open-project-task id)
      (message "未捕获任务ID"))))

(defun gll/merge-request ()
  (interactive)
  (ivy-read
   "%d Assigner: "
   GLLUE-ASSIGNERS
   :action (lambda (assigner)
             (let ((id (nth 0 (split-string assigner " "))))
               (shell-command-to-string (format "gllue-cli merge-request -a %s" id))))))

(mistkafka/keyboard/bind "Gm" 'gll/merge-request)
(mistkafka/keyboard/bind "Gp" 'gll/open-project-task-at-point)
(mistkafka/keyboard/bind "GP" 'gll/open-project-task)
(mistkafka/keyboard/bind "Gc" 'gll/auto-gen-commit-message)

(provide 'init-gll)
