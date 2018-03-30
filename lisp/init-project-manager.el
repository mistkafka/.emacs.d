;;; -*- lexical-binding: t -*-

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

(defun mistkafka/project-manager/get-do-git-grep-function (path)
  "return a ivy-collect-function with bind `default-directory' to PATH"
  (lambda (string &optional _pred &rest _u)
    (let ((default-directory path))
      (split-string
       (shell-command-to-string (format "git --no-pager grep --full-name -n --no-color -i -e \"%s\"" string))
       "\n"
       t))))

(defun mistkafka/project-manager/reactive-git-grep ()
  "实时输入pattern进行git grep。适用于小项目。"
  (interactive)
  (let* ((start-path (mistkafka/get-git-root-path))
	 (collect-fn (mistkafka/project-manager/get-do-git-grep-function start-path))
	 (seleted (ivy-read "pattern: " collect-fn))
	 lst)
    (when seleted
      (setq lst (split-string seleted ":"))
      (find-file (car lst))
      (goto-char (point-min))
      (forward-line (1- (string-to-number (cadr lst)))))
    )
  )

(defun mistkafka/project-manager-git-grep ()
  "输入一段pattern，然后进行git grep。适用于大项目。"
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
