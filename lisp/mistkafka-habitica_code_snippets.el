;; 备份一下一些被弃用的代码
;;
;; 之前的想法是通过扫描整个org文件来进行状态同步、任务新建之类的事情。
;; 但是现阶段这种做法成效慢、过于复杂。
;; 后期应该还会需要这种做法。
(setq mistkafka/habitica/file "/Users/mistkafka/gtd/tmp.org")
(defvar mistkafka/habitica/new-tasks-queue nil)
(defvar mistkafka/habitica/update-tasks-queue nil)

(defun mistkafka/habitica/sync-to-habitica ()
  (interactive)
  (setq mistkafka/habitica/new-tasks-queue nil
        mistkafka/habitica/update-tasks-queue nil)
  (org-map-tree 'mistkafka/habitica/collect-task)
  (mistkafka/habitica/do-sync-to-habitica))


  
(defun mistkafka/habitica/collect-task ()
  "将当前headline的相关信息收集到`新建'与`更新'两个队列中，等待同步。"
  (let* ((task (mistkafka/habitica/heading-task-info))
         (habitica-id (alist-get 'id task))
         (headline-title (alist-get 'text task)))
    (if (and habitica-id (not (s-starts-with-p "tmp-uuid" habitica-id)))
        (setq mistkafka/habitica/update-tasks-queue (cons task mistkafka/habitica/update-tasks-queue))
      (progn
        (unless habitica-id
          (setq habitica-id (mistkafka/habitica/gen-headline-tmp-uuid headline-title))
          (org-set-property mistkafka/habitica/habiticate-id-key habitica-id)
          (setq task `((text . ,headline-title)
                       (id   . ,habitica-id))))
        (setq mistkafka/habitica/new-tasks-queue (cons task mistkafka/habitica/new-tasks-queue))))
    ))



(defun mistkafka/habitica/gen-headline-tmp-uuid (title)
  (format "tmp-uuid-%s"
          (md5 (format "%s%s" title (random)))))

(defun mistkafka/habitica/do-sync-to-habitica ()
  (when mistkafka/habitica/new-tasks-queue
    (mistkafka/habitica/do-sync-to-habitica--new-tasks))
  (when mistkafka/habitica/update-tasks-queue
    (mistkafka/habitica/do-sync-to-habitica--update-tasks))
  (save-buffer)
  (message "同步成功！"))



(defun mistkafka/habitica/do-sync-to-habitica--new-tasks ()
  (let* ((habitica-tasks (cl-loop for x on mistkafka/habitica/new-tasks-queue
                                  for y = (car x)
                                  collect `(("text" . ,(alist-get 'text y))
                                            ("type" . "todo"))))
         (res (mistkafka/habitica/request "/tasks/user" "POST" habitica-tasks))
         (result (alist-get 'data res)))
    (cl-loop for i from 0 below (length result)
             
             for origin-data = (nth i mistkafka/habitica/new-tasks-queue)
             for tmp-id      = (alist-get 'id origin-data)

             for res-data    = (if (> (length result) 1)
                                   (aref result i)
                                 result)
             for id          = (alist-get 'id res-data)
             do (progn
                  (message (format "tmp-id: %s, id: %s" tmp-id id))
                  (goto-char (org-find-property mistkafka/habitica/habiticate-id-key tmp-id))
                  (org-set-property mistkafka/habitica/habiticate-id-key id))
             )
    ))

(defun mistkafka/habitica/do-sync-to-habitica--update-tasks ()
  (cl-loop for x on mistkafka/habitica/update-tasks-queue
           for task = (car x)
           when (string= "DONE" (alist-get 'todo-keyword task))
           do (mistkafka/habitica/make-task-done task)))

(mistkafka/keyboard/bind "h" 'mistkafka/habitica/sync-to-habitica)

;; (defun mistkafka/habitica/is-todo-headline? (headline-element)
;;   (org-element-property :todo-keyword headline-element))

;; (defun mistkafka/habitica/get-headline-title (headline-element)
;;   (let ((title-prop (org-element-property :title headline-element)))
;;     (if (stringp title-prop)
;; 	title-prop
;;       (nth 0 title-prop))))

;; (defun mistkafka/habitica/get-headline-drawer-property (property-name headline-element)
;;   (let (
;;         (props-table (mistkafka/habitica/get-headline-drawer-property-table headline-element))
;;         )
;;     (gethash property-name props-table)))

;; (defun mistkafka/habitica/put-headline-drawer-property (property val headline-element))

;; (defun mistkafka/habitica/get-headline-drawer-property-table (headline-element)
;;   (let (
;;         (props-table (make-hash-table :test 'equal))
;;         (property-drawer-element (mistkafka/habitica/get-headline-section-child 'property-drawer headline-element))
;;         )
;;     (cl-loop for x on property-drawer-element
;;              for y = (car x)
;;              for child-name = (car y)
;;              for child-val  = (nth 1 y)
;;              when (equal child-name 'node-property)
;;              do (progn
;;                   (puthash
;;                    (plist-get child-val :key)
;;                    (plist-get child-val :value)
;;                    props-table
;;                    )))
;;     props-table
;;     ))

;; (defun mistkafka/habitica/get-headline-section-child (child-symbol headline-element)
;;   "获取HEADLINE-ELEMENT的section的child. 合法的CHILD-SYMBOL有:
;; `planning' `property-drawer' `plain-list' `plain-list' `garagraph'等"
  
;;   (let ((section-element (->> (org-element-contents headline-element)
;; 			      (nth 0))))
;;     (if section-element
;; 	(alist-get child-symbol section-element)
;;       nil)))


;; (defun mistkafka/habitica/sync-todo-to-habitica (root-element)
;;   "将element及其所有的`todo'的`headline'同步到habitica"
;;   (org-element-map
;;       root-element
;;       'headline
;;     (lambda (element)
;;       ;; 1. 过滤没有todo的headline
;;       ;; 2. 获取headline的title
;;       ;; 3. 尝试从headline的properties drawer里获取habitica的任务id
;;       ;; 4. (进入异步)创建/更新 habitica的状态
;;       ;; 5. 异步回调：将id插入到properties drawer中，记录同步日志
;;       )
;;     ))
