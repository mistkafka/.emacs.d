(require-package 'bookmark)

(global-set-key
 (kbd "C-M-s-!")
 (lambda ()
   (interactive)
   (bookmark-set "1")
   (message "mark current position as 1")
   ))

(global-set-key
 (kbd "C-M-s-@")
 (lambda ()
   (interactive)
   (bookmark-set "2")
   (message "mark current position as 2")
   ))

(global-set-key
 (kbd "C-M-s-#")
 (lambda ()
   (interactive)
   (bookmark-set "3")
   (message "mark current position as 3")
   ))

(global-set-key
 (kbd "C-M-s-!")
 (lambda ()
   (interactive)
   (bookmark-set "1")
   ))

(global-set-key
 (kbd "C-1")
 (lambda ()
   (interactive)
   (bookmark-jump "1")
   ))

(global-set-key
 (kbd "C-2")
 (lambda ()
   (interactive)
   (bookmark-jump "2")
   ))

(global-set-key
 (kbd "C-3")
 (lambda ()
   (interactive)
   (bookmark-jump "3")
   ))


(provide 'init-bookmark)
