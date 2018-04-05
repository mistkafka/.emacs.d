(defun mistkafka/org-setup-todo-keywords ()
  
  (setq org-todo-keywords
	(quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d@/!)")
		(sequence "PROJECT(p)" "|" "DONE(d@/!)" "CANCELLED(c@/!)"))))
  
  (setq org-todo-repeat-to-state "NEXT")
  
  )

(defun mistkafka/org-setup-archive ()

  ;; keep inherited tags when archive
  ;; https://lists.gnu.org/archive/html//emacs-orgmode/2011-01/msg01195.html
  (defadvice org-archive-subtree (before add-inherited-tags-before-org-archive-subtree activate)
    "add inherited tags before org-archive-subtree"
    (org-set-tags-to (org-get-tags-at)))
  
  )

(defun mistkafka/org-setup-org-crypt ()
  "加密文章：http://coldnew.github.io/blog/2013/07/13_5b094.htm"
  
  (require-package 'org-crypt)
  (org-crypt-use-before-save-magic)
  (setq org-crypt-tag-matcher "secret")
  (setq org-tags-exclude-from-inheritance (quote ("secret")))
  (setq org-crypt-key "193E92ED"))

(defun mistkafka/org-defun-alias-funs ()
  
  (defun org-agenda-add-file--mistkafka-alias ()
    (interactive)
    (org-agenda-file-to-front))

  (defun org-agenda-remove-file--mistkafka-alias ()
    (interactive)
    (org-remove-file))
  )

(with-eval-after-load 'org
  (progn
    (mistkafka/org-setup-todo-keywords)
    (mistkafka/org-setup-archive)
    (mistkafka/org-setup-org-crypt)
    (mistkafka/org-defun-alias-funs)))

(provide 'init-org)
