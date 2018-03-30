;;; -*- lexical-binding: t -*-

(defun mistkafka/git-project/get-git-root-path (&optional current-path)
  "get the CURRENT-PATH's git root path"
  (interactive)
  (unless current-path
    (setq current-path default-directory))
  (locate-dominating-file current-path ".git"))

(defun mistkafka/git-project/find-file ()
  "Find file in current git project."
  (interactive)
  (let* ((default-directory (mistkafka/git-project/get-git-root-path))
	 (files (split-string (shell-command-to-string "git ls-files") "\n" t))
	 (selected-file (ivy-read "pattern: " files)))
    (when selected-file
      (find-file selected-file)
      (goto-char (point-min))
      )))

(defun mistkafka/git-project/get-do-git-grep-function (path)
  "return a ivy-collect-function with bind `default-directory' to PATH"
  (lambda (string &optional _pred &rest _u)
    (let ((default-directory path))
      (split-string
       (shell-command-to-string (format "git --no-pager grep --full-name -n --no-color -i -e \"%s\"" string))
       "\n"
       t))))

(defun mistkafka/git-project/reactive-git-grep ()
  "实时输入pattern进行git grep。适用于小项目。"
  (interactive)
  (let* ((start-path (mistkafka/git-project/get-git-root-path))
	 (collect-fn (mistkafka/git-project/get-do-git-grep-function start-path))
	 (seleted (ivy-read "pattern: " collect-fn))
	 lst)
    (when seleted
      (setq lst (split-string seleted ":"))
      (find-file (car lst))
      (goto-char (point-min))
      (forward-line (1- (string-to-number (cadr lst)))))
    )
  )

(defun mistkafka/git-project/git-grep ()
  "输入一段pattern，然后进行git grep。适用于大项目。"
  (interactive)
  (let* ((default-directory (mistkafka/git-project/get-git-root-path))
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

(defun mistkafka/git-project/get-file-name ()
    (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
      filename))

(defun mistkafka/git-project/get-file-name-in-project ()
  "Get current buffer file name in the current git project"
  (let* ((filename (mistkafka/git-project/get-file-name))
         (project-path (expand-file-name (mistkafka/git-project-get-git-root-path filename))))
    (setq filename (replace-regexp-in-string project-path "/" filename))))

(defun mistkafka/git-project/copy-file-name-to-clipboard (filename)
  "Utils, copy FILENAME to clipboard"
  (when filename
    (kill-new filename)
    (message "Copied filename '%s' to the clipboard" filename)))

(defun mistkafka/git-project/copy-file-name-in-system-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (mistkafka/git-project/copy-file-name-to-clipboard 
   (mistkafka/git-project/get-file-name)))

(defun mistkafka/git-project/copy-file-name-in-project-to-clipboard ()
  "Copy the current buffer file name in the current git project to the clipboard"
  (interactive)
  (mistkafka/git-project/copy-file-name-to-clipboard 
   (mistkafka/git-project/get-file-name-in-project)))

(global-set-key (kbd "C-x p f") 'mistkafka/git-project/find-file)
(global-set-key (kbd "C-x p g") 'mistkafka/git-project/git-grep)
(global-set-key (kbd "C-x p G") 'mistkafka/git-project/reactive-git-grep)
(global-set-key (kbd "C-x p s") 'bookmark-jump)



(provide 'init-git-project)
