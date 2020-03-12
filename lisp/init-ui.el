(require 'init-eyebrowse)

;; custom mode-line
(setq-default
 mode-line-format
 (list
  ;; layout info
  '(:eval (mistkafka/eyebrowse-mode-line))
  ;; window number
  '(:eval (propertize (format "-%s " (window-numbering-get-number-string)) 'face '((:foreground "#308191"))))
  " "
  
  ;; buffer name and edit status
  '(:eval (propertize "%b" 'face 'font-lock-keyword-face
                      'help-echo (buffer-file-name)))
  ":%* "
  
  ;; relative position, column
  "["
  (propertize "%p" 'face 'font-lock-constant-face) ;; % above top
  ","
  (propertize "%02c" 'face 'font-lock-type-face)
  "] "

  ;; the current major mode for the buffer.
  "["
  '(:eval (propertize "%m" 'face 'font-lock-string-face
                      'help-echo buffer-file-coding-system))
  "] "

  '(:eval (propertize (magit-get-current-branch) 'face 'font-lock-string-face))

  'org-pomodoro-mode-line
  ))

;; remove tool-bar
(tool-bar-mode -1)

;; ^L换页符显示成横线
(require-package 'page-break-lines)
(global-page-break-lines-mode t)
(defun m/ui/insert-page-break-line ()
  (interactive)
  (insert ?\^L))


(provide 'init-ui)
