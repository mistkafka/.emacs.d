
(require-package'cnfonts)
(cnfonts-enable)

(global-set-key (kbd "s--") 'cnfonts-decrease-fontsize)
(global-set-key (kbd "s-=") 'cnfonts-increase-fontsize)

(provide 'init-cnfonts)
