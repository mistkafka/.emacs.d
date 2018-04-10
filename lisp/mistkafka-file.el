(defun mistkafka/file/rename-current ()
  "Rename current FILE and BUFFER name."
  (interactive)
  (mistkafka/file/validate-write-current-file 'mistkafka/file/do-rename-current))

(defun mistkafka/file/delete-current ()
  (interactive)
  (mistkafka/file/validate-write-current-file 'mistkafka/file/do-delete-current))

(defun mistkafka/file/validate-write-current-file (execute-fun)
  (let (
	(file-name (mistkafka/file/safe-get-file-name))
	)
    (cond
     ((equal nil file-name)        (message "失败：当前Buffer没有对应的文件！"))
     ((file-directory-p file-name) (message "失败：暂时不支持对文件夹进行写操作！"))
     (t                            (funcall execute-fun)))
    ))

(defun mistkafka/file/safe-get-file-name ()
  "get file name or directory name.
If there is not buffer-file, return nil"
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    filename))

(defun mistkafka/file/do-delete-current ()
  (let (
	(trash-directory "~/.Trash")
	(delete-by-moving-to-trash nil)
	)
    (move-file-to-trash (buffer-file-name)))	; 删除还是太危险了，扔到垃圾箱吧。
  (kill-buffer (buffer-name)))

(defun mistkafka/file/do-rename-current ()
  (let (
	(old-name (buffer-file-name))
	(old-point (point))
	(old-buffer (current-buffer))
	new-name
	)
    (setq new-name (ivy-read "New File Name: " nil
			     :initial-input old-name))
    (save-buffer)
    (copy-file old-name new-name)
    (find-file new-name)
    (kill-buffer old-buffer)
    (goto-char old-point)
    (message "重命名成功！")))

(mistkafka/keyboard/bind "fr" 'mistkafka/file/rename-current)
(mistkafka/keyboard/bind "fD" 'mistkafka/file/delete-current)

(provide 'mistkafka-file)
