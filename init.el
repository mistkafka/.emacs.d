;; -*- lexical-binding: t -*-
(setq debug-on-error t)			; 多学学debug

;; 初始化包管理器，指向国内的镜像仓库
(package-initialize)
(setq package-archives '(("gnu"   . "http://elpa.emacs-china.org/gnu/")
                         ("melpa" . "http://elpa.emacs-china.org/melpa/")))

;; 添加`./lisp'到`load-path'中，这里将放置所有配置文件
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; 加载配置
(require 'init-org)
(require 'init-project-manager)
(require 'init-misc)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files (quote ("~/gtd/index.org.gpg"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
