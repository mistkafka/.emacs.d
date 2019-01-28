(require-package 'term)

;; 移除term-mode的"M-n"绑定
(add-hook 'term-mode-hook
          'recovery-my-leader-key)

;; send shell commands
(defun mistkafka/term-mode/send-init-command ()
  "source ~/.bash_profile来初始化终端环境"
  (interactive)
  (mistkafka/term-mode/execute-shell-command "source ~/.bash_profile"))

(defun mistkafka/term-mode/send-ssh-to-glow-devbox-command ()
  "ssh to devbox"
  (mistkafka/term-mode/execute-shell-command "ssh devbox"))

;; term-mode command
(defun mistkafka/term-mode/execute-shell-command (cmd)
  (term-send-raw-string cmd)
  (term-send-raw-string "\C-m")         ; 回车
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

(defun mistkafka/term-mode/reset-keybind ()
  (interactive)
  (recovery-my-leader-key)
  (local-set-key (kbd "M-x") nil))

;; gl term group
(setq mistkafka/term-mode/glow-devbox-term-names '("log" "mysql" "supervisorctl" "terminal" "python3"))

(defun mistkafka/term-mode/init-glow-terms-group ()
  "启动多个terminal，根据作用分别命名"
  (interactive)
  (cl-loop for term-name in mistkafka/term-mode/glow-devbox-term-names
           do (ansi-term "/bin/bash" term-name))
  (run-at-time "0.5" nil 'mistkafka/term-mode/do-send-common-init-command))

(defun mistkafka/term-mode/do-send-common-init-command ()
  (cl-loop for term-name in mistkafka/term-mode/glow-devbox-term-names
           for bffr-name = (format "*%s*" term-name)
           do (when bffr-name
                (with-current-buffer bffr-name
                  (rename-buffer term-name)
                  (mistkafka/term-mode/send-init-command)
                  (linum-mode -1)
                  (mistkafka/term-mode/reset-keybind)
                  (mistkafka/term-mode/send-ssh-to-glow-devbox-command)
                  )))
  (run-at-time "1" nil 'mistkafka/term-mode/do-send-special-init-command)
  )

(defun mistkafka/term-mode/do-send-special-init-command()
  (with-current-buffer "mysql"
    (mistkafka/term-mode/execute-shell-command "mysql -uroot mia"))

  (with-current-buffer "supervisorctl"
    (mistkafka/term-mode/execute-shell-command "sudo supervisorctl"))

  (with-current-buffer "log"
    (mistkafka/term-mode/execute-shell-command "cd /nail/logs"))

  (with-current-buffer "python3")
  (mistkafka/term-mode/execute-shell-command "cd /nail/srv")
  (mistkafka/term-mode/execute-shell-command "python3")
  )

(provide 'init-term)
