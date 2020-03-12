(require-package 'eyebrowse)

(eyebrowse-mode t)

(defun mistkafka/eyebrowse-create-window-config ()
  "新建layout之后，立即进行命名"
  (interactive)
  (eyebrowse-create-window-config)
  (call-interactively 'eyebrowse-rename-window-config)
  )

;; s -> switch，切换
(mistkafka/keyboard/bind "ls" 'eyebrowse-switch-to-window-config)
(mistkafka/keyboard/bind "s" 'eyebrowse-switch-to-window-config) ; switch非常高频，所以直接绑在s上
;; r -> rename
(mistkafka/keyboard/bind "lr" 'eyebrowse-rename-window-config)
;; x -> 象征'叉'即关闭
(mistkafka/keyboard/bind "lx" 'eyebrowse-close-window-config)
;; l -> last
(mistkafka/keyboard/bind "ll" 'eyebrowse-last-window-config)
;; n -> new 
(mistkafka/keyboard/bind "ln" 'mistkafka/eyebrowse-create-window-config)

(mistkafka/keyboard/bind "l0" 'eyebrowse-switch-to-window-config-0)
(mistkafka/keyboard/bind "l1" 'eyebrowse-switch-to-window-config-1)
(mistkafka/keyboard/bind "l2" 'eyebrowse-switch-to-window-config-2)
(mistkafka/keyboard/bind "l3" 'eyebrowse-switch-to-window-config-3)
(mistkafka/keyboard/bind "l4" 'eyebrowse-switch-to-window-config-4)
(mistkafka/keyboard/bind "l5" 'eyebrowse-switch-to-window-config-5)
(mistkafka/keyboard/bind "l6" 'eyebrowse-switch-to-window-config-6)
(mistkafka/keyboard/bind "l7" 'eyebrowse-switch-to-window-config-7)
(mistkafka/keyboard/bind "l8" 'eyebrowse-switch-to-window-config-8)
(mistkafka/keyboard/bind "l9" 'eyebrowse-switch-to-window-config-9)

(defun mistkafka/eyebrowse-mode-line ()
  (let* ((layouts (eyebrowse--get 'window-configs))
         (layout-counts (length layouts))
         (curr-layout-number (eyebrowse--get 'current-slot))
         (curr-layout (assoc curr-layout-number layouts))
         (curr-layout-name (nth 2 curr-layout))
         (curr-layout-display (propertize
                               (format "%s-%s" curr-layout-number curr-layout-name)
                               'face '((:foreground "#0366d6"))))
         )
    (format "%s/%s"
            layout-counts
            curr-layout-display
            )))

(provide 'init-eyebrowse)
