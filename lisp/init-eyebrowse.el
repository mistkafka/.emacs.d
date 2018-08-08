(require-package 'eyebrowse)

(eyebrowse-mode t)

(mistkafka/keyboard/bind "ls" 'eyebrowse-switch-to-window-config)
(mistkafka/keyboard/bind "lr" 'eyebrowse-rename-window-config)
(mistkafka/keyboard/bind "lx" 'eyebrowse-close-window-config)
(mistkafka/keyboard/bind "ll" 'eyebrowse-last-window-config)

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
