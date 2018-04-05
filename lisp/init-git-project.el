;;; -*- lexical-binding: t -*-

(require-package 's)

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
  (let* ((default-directory (mistkafka/git-project/get-git-root-path))
	 (collect-fn (mistkafka/git-project/get-do-git-grep-function default-directory))
	 (seleted (ivy-read "pattern: " collect-fn))
	 lst)
    (when seleted
      (setq lst (split-string seleted ":"))
      (find-file (car lst))
      (goto-char (point-min))
      (forward-line (1- (string-to-number (cadr lst))))
      )
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
      (forward-line (1- (string-to-number (cadr lst))))
      )
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

(defvar mistkafka/git-project/locate-paths-of-code '("gllue" "elisp" "node.js" "front-end" "elisp")
  "更新projects时，需要扫描的几个文件夹")

(defvar mistkafka/git-project/cache-file-path "/Users/mistkafka/.emacs.d/.cache/git-projects"
  "缓存文件的路径")

(defvar mistkafka/git-project/cache nil
  "projects的缓存")

(defun mistkafka/git-project/read-projects-cache ()
  "从缓存文件中读取projects，并返回。"
  (with-temp-buffer
    (insert-file-contents mistkafka/git-project/cache-file-path)
    (s-split "\n" (buffer-string) t)))

(defun mistkafka/git-project/update-projects-cache ()
  "更新projects的缓存"
  (interactive)
  (setq mistkafka/git-project/cache (mistkafka/git-project/do-update-projects-cache))
  (message "更新成功！"))

(defun mistkafka/git-project/do-update-projects-cache ()
  "负责执行projects缓存的更新。将新结果写入缓存文件，并返回。"
  (let* ((cmd-tpl "locate .git | grep '^/Users/mistkafka/Code/\\(%s\\).*\\.git$' ")
	(locate-paths (s-join "\\|" mistkafka/git-project/locate-paths-of-code))
	(cmd (format cmd-tpl locate-paths))
	(result-str (shell-command-to-string cmd)))
    (setq result-str (s-replace-regexp "/\\.git$" "" result-str))
    (with-temp-file mistkafka/git-project/cache-file-path
      (delete-region (point-min) (point-max))
      (insert result-str)
      (s-split "\n" result-str t))))

(defun mistkafka/git-project/get-update-locate-db-cmd ()
  "获取更新locate db的命令。
由于更新locate db比较耗时，最好不要在emacs里处理。所以这里选择把命令复制到kill-ring里，然后复制到终端里运行"
  (interactive)
  (kill-new "sudo /usr/libexec/locate.updatedb --localpaths '/Users/mistkafka/Code'")
  (message "更新命令已经复制到黏贴板中，在终端中运行更新命令！"))

(defun mistkafka/git-project/switch-to-git-project (should-update-cache?)
  (interactive "P")
  (let (selected)
    (when should-update-cache?
      (mistkafka/git-project/update-projects-cache))
    (unless mistkafka/git-project/cache
      (setq mistkafka/git-project/cache (mistkafka/git-project/read-projects-cache)))
    (setq selected (ivy-read "Switch to Project: " mistkafka/git-project/cache))
    (when selected
      (find-file selected)
      (goto-char (point-min))
      (mistkafka/git-project/find-file))
    ))

(mistkafka/keyboard/bind "pf" 'mistkafka/git-project/find-file)
(mistkafka/keyboard/bind "pg" 'mistkafka/git-project/git-grep)
(mistkafka/keyboard/bind "pG" 'mistkafka/git-project/reactive-git-grep)
(mistkafka/keyboard/bind "ps" 'mistkafka/git-project/switch-to-git-project)

(provide 'init-git-project)
