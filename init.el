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
  (let* ((key-describe-items (s-split "" key-describe t))
         (key-describe-length (length key-describe-items))
         (maped-key-describe-items nil)
         (the-index -1))
    (cl-loop for i from -1 below key-describe-length
             do (mistkafka/keyboard/bind--do-bind
                 (mistkafka/keyboard/bind--generate-describe-items i key-describe-items)
                 command)
             )))
(defun mistkafka/keyboard/bind--generate-describe-items (current-index origin-key-describe-items)
  (let* ((the-length (length origin-key-describe-items))
         (index 0)
         (maped-item nil))
    
    (mapcar
     (lambda (item)
       (setq maped-item (if (<= index current-index)
                            (format "M-%s" item)
                          item))
       (setq index (+ 1 index))
       maped-item)
     
     origin-key-describe-items)
    ))
(defun mistkafka/keyboard/bind--do-bind (key-describe-items command)
  (message (format "%s" (s-join " " key-describe-items)))
  (global-set-key
   (kbd (format "M-n %s" (s-join " " key-describe-items)))
   command))

;; 基础配置
(require 'init-exec-path)

;; 编程相关
(require 'init-lsp)
(require 'init-company)

;; 语言
(require 'init-org)
(require 'init-typescript)
(require 'init-python)
(require 'init-css)

;; 文件/项目
(require 'init-git-project)
(require 'mistkafka-file)
(require 'init-eyebrowse)

;; 杂类 or 待分类
(require 'init-ui)
(require 'init-misc)
(require 'init-cnfonts)
(require 'init-gll)
;; (require 'mistkafka-habitica)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "aa0a998c0aa672156f19a1e1a3fb212cdc10338fb50063332a0df1646eb5dfea" "a866134130e4393c0cad0b4f1a5b0dd580584d9cf921617eee3fd54b6f09ac37" "0598de4cc260b7201120b02d580b8e03bd46e5d5350ed4523b297596a25f7403" "013c62a1fcee7c8988c831027b1c38ae215f99722911b69e570f21fc19cb662e" "4597d1e9bbf1db2c11d7cf9a70204fa42ffc603a2ba5d80c504ca07b3e903770" "bbb4a4d39ed6551f887b7a3b4b84d41a3377535ccccf901a3c08c7317fad7008" "5715d3b4b071d33af95e9ded99a450aad674e308abb06442a094652a33507cd2" "c5d320f0b5b354b2be511882fc90def1d32ac5d38cccc8c68eab60a62d1621f2" "4e21fb654406f11ab2a628c47c1cbe53bab645d32f2c807ee2295436f09103c6" "891debfe489c769383717cc7d0020244a8d62ce6f076b2c42dd1465b7c94204d" default)))
 '(lsp-project-whitelist (quote ("^/Users/mistkafka/Code/gllue/web1/$")))
 '(org-agenda-files (quote ("~/gtd/plan.org.gpg" "~/gtd/index.org.gpg")))
 '(package-selected-packages
   (quote
    (tide-mode org-crypt tramp-cache dockerfile-mode stylus-mode editorconfig nginx-mode window-number multi-term paredit slime evil smartparens htmlize 0blayout color-theme-sanityinc-tomorrow js-doc cnfonts counsel auto-complete page-break-lines yasnippet-snippets yasnippet-snippetst yasnippet doom-themes use-package lsp-python company-lsp lsp-javascript-typescript lsp-ui lsp-mode eyebrowse which-key typescript-mode magit ivy web-mode)))
 '(safe-local-variable-values (quote ((epa-file-select-keys "98DE5D28"))))
 '(send-mail-function (quote mailclient-send-it)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'upcase-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'downcase-region 'disabled nil)
