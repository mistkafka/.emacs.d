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
(require 'mistkafka-file)
(require 'init-gll)
(require 'mistkafka-habitica)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("aa0a998c0aa672156f19a1e1a3fb212cdc10338fb50063332a0df1646eb5dfea" "a866134130e4393c0cad0b4f1a5b0dd580584d9cf921617eee3fd54b6f09ac37" "0598de4cc260b7201120b02d580b8e03bd46e5d5350ed4523b297596a25f7403" "013c62a1fcee7c8988c831027b1c38ae215f99722911b69e570f21fc19cb662e" "4597d1e9bbf1db2c11d7cf9a70204fa42ffc603a2ba5d80c504ca07b3e903770" "bbb4a4d39ed6551f887b7a3b4b84d41a3377535ccccf901a3c08c7317fad7008" "5715d3b4b071d33af95e9ded99a450aad674e308abb06442a094652a33507cd2" "c5d320f0b5b354b2be511882fc90def1d32ac5d38cccc8c68eab60a62d1621f2" "4e21fb654406f11ab2a628c47c1cbe53bab645d32f2c807ee2295436f09103c6" "891debfe489c769383717cc7d0020244a8d62ce6f076b2c42dd1465b7c94204d" default)))
 '(lsp-project-whitelist (quote ("^/Users/mistkafka/Code/gllue/web1/$")))
 '(org-agenda-files (quote ("~/gtd/index.org.gpg")))
 '(package-selected-packages
   (quote
    (js-doc cnfonts counsel auto-complete page-break-lines yasnippet-snippets yasnippet-snippetst yasnippet doom-themes use-package lsp-python company-lsp lsp-javascript-typescript lsp-ui lsp-mode eyebrowse which-key typescript-mode magit ivy web-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'upcase-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'downcase-region 'disabled nil)
