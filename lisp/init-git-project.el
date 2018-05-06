;;; -*- lexical-binding: t -*-

(require-package 's)
(require 'json-storage)
(require 'seq)

(require 'mistkafka-file)

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
	 (files (split-string (shell-command-to-string "git ls-files -z") "\0" t))
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

(defun mistkafka/git-project/reactive-git-grep (is-in-current-directory?)
  "实时输入pattern进行git grep。适用于小项目。"
  (interactive "P")
  (let* ((default-directory (mistkafka/git-project/determine-git-grep-path is-in-current-directory?))
	 (collect-fn (mistkafka/git-project/get-do-git-grep-function default-directory))
	 (seleted (ivy-read "pattern: " collect-fn))
	 lst)
    (when seleted
      (setq lst (split-string seleted ":"))
      (setq default-directory (mistkafka/git-project/get-git-root-path))
      (find-file (car lst))
      (goto-char (point-min))
      (forward-line (1- (string-to-number (cadr lst))))
      )
    )
  )


(defun mistkafka/git-project/git-grep (is-in-current-directory?)
  "输入一段pattern，然后进行git grep。适用于大项目。"
  (interactive "P")
  (let* ((default-directory (mistkafka/git-project/determine-git-grep-path is-in-current-directory?))
	 (keyword (ivy-read "keyword: " nil))
	 (cmd (format "git --no-pager grep --full-name -n --no-color -i -e \"%s\"" keyword))
	 (result-str (shell-command-to-string cmd))
	 (result (split-string result-str "\n" t))
	 (seleted (ivy-read "choose: " result))
	 lst)
    (when seleted
      (setq lst (split-string seleted ":"))
      (setq default-directory (mistkafka/git-project/get-git-root-path))
      (find-file (car lst))
      (goto-char (point-min))
      (forward-line (1- (string-to-number (cadr lst))))
      )
    )
  )

(defun mistkafka/git-project/determine-git-grep-path (is-in-current-directory?)
  "git grep 的辅助函数。如果`IS-IN-CURRENT-DIRECTORY?'不为nil，则返回当前文件所在的文件夹。
否则，返回当前git项目的根目录。"
  (if is-in-current-directory?
      default-directory
    (mistkafka/git-project/get-git-root-path)))

(defun mistkafka/git-project/get-file-name-in-project ()
  "Get current buffer file name in the current git project"
  (let* ((filename (mistkafka/file/safe-get-file-name))
         (project-path (expand-file-name (mistkafka/git-project/get-git-root-path))))
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
   (mistkafka/file/safe-get-file-name)))

(defun mistkafka/git-project/copy-file-name-in-project-to-clipboard ()
  "Copy the current buffer file name in the current git project to the clipboard"
  (interactive)
  (mistkafka/git-project/copy-file-name-to-clipboard 
   (mistkafka/git-project/get-file-name-in-project)))

(defvar mistkafka/git-project/locate-paths-of-code '("gllue" "elisp" "node.js" "front-end" "elisp")
  "更新projects时，需要扫描的几个文件夹")

(defvar mistkafka/git-project/cache-file-path "/Users/mistkafka/.emacs.d/.cache/git-projects"
  "缓存文件的路径")

(defconst mistkafka/git-project/cache-storage (json-storage/create-storage
					       :path mistkafka/git-project/cache-file-path
					       :schema-alist '(
							       ("projects-from-locate"     . [])
							       ("projects-from-manual-reg" . [])
							       )))

(defun mistkafka/git-project/read-projects-cache ()
  "从缓存文件中读取projects，并返回。"
  (json-storage/get-value "projects-from-locate" mistkafka/git-project/cache-storage))

(defun mistkafka/git-project/update-cache-of-projects-from-locate ()
  "执行一遍`locate'命令来刷新`projects-from-locate'的缓存"
  (interactive)
  (let (
	(projects (mistkafka/git-project/do-update-projects-cache))
	)
    (json-storage/set-value "projects-from-locate" projects mistkafka/git-project/cache-storage)
    )
  (message "更新成功！"))

(defun mistkafka/git-project/do-update-projects-cache ()
  "负责执行projects缓存的更新"
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
  
  (when should-update-cache?
    (mistkafka/git-project/update-cache-of-projects-from-locate))
  
  (let* (
	 projects
	 selected
	 )
    (setq projects (vconcat (json-storage/get-value "projects-from-locate" mistkafka/git-project/cache-storage)
			    (json-storage/get-value "projects-from-manual-reg" mistkafka/git-project/cache-storage)))
    (setq projects (append projects nil))
    (setq projects (delete-dups projects))
    (setq selected (ivy-read "Switch to Project: " projects))
    (when selected
      (find-file selected)
      (goto-char (point-min))
      (mistkafka/git-project/find-file))
    ))

(defun mistkafka/git-project/registe-current-project ()
  "把当前的git项目注册到project列表里。
有些时候一些git项目并不在~/Code目录下，用locate去搜索很耗时，所以采用这种手动注册的方式来进行。"
  (interactive)
  (let (
	(git-root-path (mistkafka/git-project/get-git-root-path))
	(manual-reg-projects (json-storage/get-value "projects-from-manual-reg" mistkafka/git-project/cache-storage))
	 )
    
    (if (and git-root-path
	     (not (seq-contains manual-reg-projects git-root-path)))
	(progn
	  (json-storage/set-value "projects-from-manual-reg" (vconcat manual-reg-projects (vector git-root-path)) mistkafka/git-project/cache-storage)
	  (message "注册成功！"))
      (message "未选择，或已经存在！"))
    )
  )

(defun mistkafka/git-project/remove-registed-project ()
  (interactive)
  (let* (
	 (manual-reg-projects (json-storage/get-value "manual-reg-projects" mistkafka/git-project/cache-storage))
	 selected
	 )
    (if (and manual-reg-projects
	     (/= 0 (length manual-reg-projects)))
	(progn
	  (setq selected (ivy-read "remove which:"
				   (append manual-reg-projects nil) ; cover vector to list
				   ))
	  (setq manual-reg-projects (seq-remove selected manual-reg-projects))
	  (setq manual-reg-projects (vconcat [] manual-reg-projects)) ; cover list to vector
	  (json-storage/set-value "projects-from-manual-reg" manual-reg-projects mistkafka/git-project/cache-storage)
	  (message (format "删除'%s'成功" selected))
	  )
      (message "没有注册过project"))
    ))

(mistkafka/keyboard/bind "pf" 'mistkafka/git-project/find-file)
(mistkafka/keyboard/bind "pg" 'mistkafka/git-project/git-grep)
(mistkafka/keyboard/bind "pG" 'mistkafka/git-project/reactive-git-grep)
(mistkafka/keyboard/bind "ps" 'mistkafka/git-project/switch-to-git-project)

(mistkafka/keyboard/bind "fcp" 'mistkafka/git-project/copy-file-name-in-project-to-clipboard)
(mistkafka/keyboard/bind "fcs" 'mistkafka/git-project/copy-file-name-in-project-to-clipboard)

(provide 'init-git-project)
