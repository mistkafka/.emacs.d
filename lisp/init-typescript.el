(defun mistkafka/setup-tide-mode ()
  (tide-setup)
  (tide-hl-identifier-mode +1)
  (eldoc-mode +1))

(add-hook 'typescript-mode-hook #'mistkafka/setup-tide-mode)

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-hook 'web-mode-hook
	  (lambda ()
	    (when (string-equal "tsx" (file-name-extension buffer-file-name))
	      (mistkafka/setup-tide-mode))))

(provide 'init-typescript)
