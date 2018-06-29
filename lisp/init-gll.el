(require 'gll-config)

(define-derived-mode gll-vue-tpl-mode web-mode "gll-vue-tpl")
(provide 'gll-vue-tpl-mode)

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

(defun gll/edit-gllue-vue-template-string-at-point ()
  (interactive)
  (let* ((find-region)
         (template-string-start)
         (template-string-end)
         (tmp-buffer-name)
         (tmp-buffer)
         (current-buffer-name)
         (current-buffer-ins)
         (edit-content))

    (setq find-region (evil-select-quote ?\` nil nil nil 1))
    (if find-region
        (progn
          (setq template-string-start (nth 0 find-region))
          (setq template-string-end (nth 1 find-region))

          (setq edit-content (buffer-substring-no-properties template-string-start template-string-end))

          (setq current-buffer-name (buffer-name))
          (setq current-buffer-ins (current-buffer))
          (setq tmp-buffer-name (format "*- vue-template-editor: %s -*" current-buffer-name))
          (setq tmp-buffer (generate-new-buffer tmp-buffer-name))
          (switch-to-buffer tmp-buffer)

          ;; 在没有file的buffer里执行web-mode会报错，导致后续语句全挂了
          (ignore-errors (gll-vue-tpl-mode))
          (insert edit-content)

          ;; 切换model会重置local variable，所以只能在切换到web-mode后执行
          (setq-local gllue/vue-template-editor-origin-buffer--local current-buffer-ins)
          (setq-local gllue/vue-template-editor-origin-string-start--local template-string-start)

          (local-set-key (kbd "C-x C-s") 'gll/save-current-gllue-vue-template-string)
          (local-set-key (kbd "C-x s") 'gll/save-current-gllue-vue-template-string)

          (message "进入narrow编辑模式，编辑完成通过`gllue/save-current-gllue-vue-template-string`退出"))
      (message "没有找到template string。"))))

(defun gll/save-current-gllue-vue-template-string ()
  (interactive)
  (let* ((current-buffer-name (buffer-name))
         (origin-infos)
         (origin-buffer-name)
         (origin-string-start-point)
         (origin-string-end-point)
         (gllue/vue-template-editor-origin-string-start gllue/vue-template-editor-origin-string-start--local)
         (gllue/vue-template-editor-origin-string-end)
         (edited-content (buffer-substring-no-properties (point-min) (point-max))))

    (with-current-buffer gllue/vue-template-editor-origin-buffer--local
      (goto-char gllue/vue-template-editor-origin-string-start)
      (setq gllue/vue-template-editor-origin-string-end (nth 1 (evil-select-quote ?\` nil nil nil 1)))
      (delete-region gllue/vue-template-editor-origin-string-start gllue/vue-template-editor-origin-string-end)
      (insert edited-content)
      (save-buffer)
      (message "回写成功！"))))

(defun gll/kill-all-vue-template-edit-buffer ()
  (interactive)
  (mapcar
   (lambda (buffer)
     (when (s-starts-with? "*- vue-template-editor: " (buffer-name buffer))
       (kill-buffer buffer)))
   (buffer-list))
  (message "已清除所有vue template editor buffer"))

(mistkafka/keyboard/bind "Gm" 'gll/merge-request)
(mistkafka/keyboard/bind "Gp" 'gll/open-project-task-at-point)
(mistkafka/keyboard/bind "GP" 'gll/open-project-task)
(mistkafka/keyboard/bind "Gc" 'gll/auto-gen-commit-message)

(provide 'init-gll)
