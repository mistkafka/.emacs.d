(defconst mistkafka-project-manager-store-path "~/.emacs.d/.cache/mistkafka-projects" "用来保存曾经打开过的git项目")

(defun mistkafka/get-git-root-path (&optional current-path)
  "get the CURRENT-PATH's git root path"
  (interactive)
  (unless current-path
    (setq current-path default-directory))
  (locate-dominating-file current-path ".git"))

(defun mistkafka/project-manager-find-file ()
  "Find file in current git project."
  (interactive)
  (let* ((default-directory (mistkafka/get-git-root-path))
	 (files (split-string (shell-command-to-string "git ls-files") "\n" t))
	 (selected-file (ivy-read "pattern: " files)))
    (when selected-file
      (find-file selected-file)
      (goto-char (point-min))
      )))

(defun mistkafka/project-manager-git-grep ()
  (interactive)
  (let* ((default-directory (mistkafka/get-git-root-path))
	 (keyword (ivy-read "keyword: " nil))
	 (cmd (format "git --no-pager grep --full-name -n --no-color -i -e \"%s\"" keyword))
	 (result-str (shell-command-to-string cmd))
	 (result (split-string result-str "\n" t))
	 (seleted (ivy-read "choose: " result))
	 lst)
    (when seleted
      (setq lst (split-string seleted ":"))
      (find-file (car lst))
      (goto-char (point-min))
      (forward-line (1- (string-to-number (cadr lst)))))
    )
  )

(provide 'init-project-manager)
