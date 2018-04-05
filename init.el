;; 初始化包管理器，指向国内的镜像仓库
(require 'package)
(setq package-archives '(("gnu"   . "http://elpa.emacs-china.org/gnu/")
                         ("melpa" . "http://elpa.emacs-china.org/melpa/")))
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory)) ;; 添加`./lisp'到`load-path'中，这里将放置所有配置文件
(package-initialize)

(defun require-package (package)
  "如果PACKAGE已经安装，则`require'这个PACKAGE。
否则，安装这个PACKAGE"
  (if (package-installed-p package)
      (require package)
    (mistkafka/package-install package)))

(defun mistkafka/package-install (package)
  "安全的安装PACKAGE。需要说明的是，`package-install'一个package之后，
会自动`require'这个PACKAGE"
  (unless package-archive-contents
    (package-refresh-contents))
  (unless (assoc package package-archive-contents)
    (package-refresh-contents))
  (package-install package))

(require-package 'use-package)

;; 加载第三方包
(require-package 'ivy)

(require-package 's)
(defun mistkafka/keyboard/bind (key-describe command)
  "将COMMAND按键绑定到`M-n'下的KEY-DESCRIBE."
  (global-set-key
   (kbd (format "M-n %s" (s-join " " (s-split "" key-describe t))))
   command))

;; 加载配置
(require 'init-exec-path)
(require 'init-org)
(require 'init-git-project)
(require 'init-typescript)
(require 'init-misc)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(lsp-project-whitelist (quote ("^/Users/mistkafka/Code/gllue/web1/$")))
 '(org-agenda-files (quote ("~/gtd/index.org.gpg")))
 '(package-selected-packages
   (quote
    (use-package lsp-python company-lsp lsp-javascript-typescript lsp-ui lsp-mode eyebrowse which-key typescript-mode magit ivy web-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
