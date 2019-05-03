(defun mistkafka/vue/create-component (name)
  (interactive "sComponent name: ")
  (progn
    (shell-command (format "vue-tools.py create_component %s" name))
    (if (equal "index" name)
        (find-file "main.html")
      (find-file (format "%s.html" name)))
    )
  )

(defun mistkafka/vue/create-component-in-dir (name)
  (interactive "sComponent name: ")
  (progn
    (shell-command (format "vue-tools.py create_component_in_dir %s" name))
    (find-file (format "%s/main.html" name))
    )
  )


(provide 'init-vue)
