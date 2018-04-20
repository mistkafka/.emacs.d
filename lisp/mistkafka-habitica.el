;;;  -*- lexical-binding: t; -*-

(require 'json)
(require 'org)
(require 's)
(require 'dash)

;; 暂时将uid, key存在这里面
(require 'habitica-config)

(defconst mistkafka/habitica/base-url "https://habitica.com/api/v3")
(defvar mistkafka/habitica/uid nil)
(defvar mistkafka/habitica/key nil)

(defun my/curl-request (url opts)
  "基于`curl'实现一个简单的http request. OPTS对应curl的各个参数。
之所以会自己实现一个，而不是用`request.el'，这是因为：
1. 所需要的功能比较简单，对curl封装一下就好了
2. request我有点“掌控”不来，结合`cl-function'之后经常出诡异问题，我解决不了。"
  (let ((opts-str nil)
        (cmd-tpl "curl --silent %s %s")
        (cmd nil)
        (result nil))
    (setq opts-str (s-join
                    " "
                    (mapcar (lambda (opt)
                              (format "--%s '%s'" (car opt) (cdr opt)))
                            opts)))
    (setq cmd (format cmd-tpl opts-str url))
    ;; (message (format "*curl-request*, running request: %s" cmd))
    (setq result
          (shell-command-to-string cmd))
    (message (format "*curl-request*, with result: %s" result))
    result
    ))

(defun mistkafka/habitica/request (endpoint method &optional data)
  "对`my/curl-request'进一步进行封装，封装成habitica所需要的request.
ENDPOINT是habitica的接口链接。
METHOD是http的method，post, get, put等。
data是http请求的body，会被转成JSON。在habitica下，传数据都用json。"
  (let* ((url (s-concat mistkafka/habitica/base-url endpoint))
         (x-api-user--header (format "x-api-user: %s" mistkafka/habitica/uid))
         (x-api-key--header  (format "x-api-key: %s" mistkafka/habitica/key))
         (req-opts `(("header" . ,x-api-user--header)
                     ("header" . ,x-api-key--header)
                     ("header" . "content-type: application/json; charset=utf-8"))))

    (unless method
      (setq method "GET"))
    (setq req-opts (append req-opts `(("request" . ,method))))

    (when data
      (setq data (json-encode data))
      (setq req-opts (append req-opts `(("data" . ,data)))))

    ;; curl 如果使用POST，但是又没有data会报错，所以提供一个空数据
    (when (and (string= "POST" method) (not data))
      (setq data "{}")
      (setq req-opts (append req-opts `(("data" . ,data)))))
    
    (json-read-from-string
     (my/curl-request url req-opts))
    ))

(defconst my/org-property-drawer-re
  (concat "^[ \t]*:PROPERTIES:[ \t]*\n"
	  "\\(?:[ \t]*:\\S-+:\\(?: .*\\)?[ \t]*\n\\)*?"
	  "[ \t]*:END:[ \t]*\n?")
  "Matches an entire property drawer.与`org-property-drawer-re'不同的是，在末尾多匹配了一个可选的'\n'")

(defconst my/org-logbook-drawer-re
  (concat "^[ \t]*:LOGBOOK:[ \t]*\n"
          "\\(.*\n\\)*"
	  "[ \t]*:END:[ \t]*\n?")
  "Matches an entire property drawer.从`my/org-property-drawer-re'抄过来的")

(defun my/get-current-headline-content ()
  "获取当前headline的“内容”，它除去了子headline、属性、LOGBOOK、TITLE。"
  (let ((content nil))
    (setq content (org-get-entry))
    (setq content (substring-no-properties content 0 (length content)))
    (setq content (substring               content 0 (s-index-of "\n*" content)))
    (setq content (s-replace-regexp my/org-property-drawer-re "" content))
    (setq content (s-replace-regexp my/org-logbook-drawer-re  "" content))
    content))

(defun my/get-current-headline-deadline-time ()
  "尝试获取当前headline的deadline时间。如果有返还字符串时间，如果没有返回nil"
  (let ((the-time nil))
    (setq the-time (org-get-deadline-time nil))
    (when the-time
      (setq the-time (format-time-string "%Y-%m-%d %H:%M:%S" the-time))
      the-time)))

(setq frequency-org-2-habitica-table (make-hash-table :test 'equal))
(puthash "d" "daily"   frequency-org-2-habitica-table)
(puthash "w" "weekly"  frequency-org-2-habitica-table)
(puthash "m" "monthly" frequency-org-2-habitica-table)
(puthash "y" "yearly"  frequency-org-2-habitica-table)

(defun my/get-current-headline-repeat ()
  "获取当前headline的repeat规则的信息。repeat的信息是(everyX frequency).

frequency指的是频率单位，如`daily', `weekly'.
everyX指的是频率"
  (let ((the-repeat nil)
        (frequency nil)
        (everyX nil))
    (setq the-repeat (org-get-repeat))
    
    (when the-repeat
      (setq everyX (string-to-number the-repeat))
      
      (setq frequency (substring (s-reverse the-repeat) 0 1))
      (setq frequency (gethash frequency frequency-org-2-habitica-table))
      `(,everyX ,frequency))))

(defun my/get-current-headline-habitica-id ()
  "获取当前headline的habitica-id"
  (org-entry-get (point) "HABITICATE-ID" nil t))

(setq difficulty-org-2-habitica-table (make-hash-table :test 'equal))
(puthash "Trivial" 0.5 difficulty-org-2-habitica-table)
(puthash "Easy"    1   difficulty-org-2-habitica-table)
(puthash "Medium"  1.5 difficulty-org-2-habitica-table)
(puthash "Hard"    2   difficulty-org-2-habitica-table)
(defun my/get-current-headling-habitica-priority ()
  "获取当前headline的priority"
  (let ((tags (nth 5 (org-heading-components))))
    (if tags
        (let ((difficulty nil))
          (setq difficulty (->> (s-split ":" tags)
                                (mapcar
                                 (lambda (tag) (gethash tag difficulty-org-2-habitica-table 0)))
                                (seq-max)))
          (when (= 0 difficulty)
            (setq difficulty 1))
          (number-to-string difficulty))
      "1")))

(defun mistkafka/habitica/get-current-headline-info ()
  "收集当前headline的信息，等待进一步处理。"
  (let ((headline-info (make-hash-table :test 'equal))
        (heading-components     (org-heading-components)))
    (puthash "title"
             (nth 4 heading-components)
             headline-info)
    (puthash "content"
             (my/get-current-headline-content)
             headline-info)
    (puthash "todo-keyword"
             (nth 2 heading-components)
             headline-info)
    (puthash "level"
             (nth 0 heading-components)
             headline-info)
    (puthash "habitica-id"
             (my/get-current-headline-habitica-id)
             headline-info)
    (puthash "habitica-priority"
             (my/get-current-headling-habitica-priority)
             headline-info)
    (puthash "deadline-time"
             (my/get-current-headline-deadline-time)
             headline-info)
    (puthash "repeat-info"
             (my/get-current-headline-repeat)
             headline-info)
    headline-info))

(defun mistkafka/habitica/cover-headline-info-2-habitica-task (headline-info)
  "将HEADLINE-INFO转成habitica的task."
  (let ((task-info (make-hash-table :test 'equal))
        (repeat-info (gethash "repeat-info" headline-info))
        (type nil)
        (deadline-time (gethash "deadline-time" headline-info)))
    
    (if repeat-info
        (setq type "daily")
      (setq type "todo"))

    ;; required fileds
    (puthash "text"
             (gethash "title" headline-info)
             task-info)
    (puthash "type"
             type
             task-info)
    
    ;; option fields
    (puthash "notes"
             (gethash "content" headline-info)
             task-info)

    (puthash "priority"
             (gethash "habitica-priority" headline-info "1")
             task-info)
    
    (when deadline-time
      (if (string= type "todo")
          (puthash "date" deadline-time task-info)
        (puthash "startDate" deadline-time task-info)))

    (when repeat-info
      (puthash "frequency"
               (nth 1 repeat-info)
               task-info)
      (puthash "everyX"
               (nth 0 repeat-info)
               task-info))
    task-info))

(defun mistkafka/habitica/make-task-done (task-id)
  "将TASK-ID对应的任务标记为一次完成。"
  (let ((url (format "/tasks/%s/score/up" task-id))
        (data '(("scoreNotes" . ""))))
    (mistkafka/habitica/request url "POST" data)))

(defun mistkafka/habitica/del-task (task-id)
  "讲TASK-ID对应的任务删除。"
  (let ((url (format "/tasks/%s" task-id)))
    (mistkafka/habitica/request url "DELETE")))

(defun mistkafka/habitica/create-task (headline-info)
  (let* ((task-info (mistkafka/habitica/cover-headline-info-2-habitica-task headline-info))
         (response (mistkafka/habitica/request "/tasks/user" "POST" task-info)))
    response))

(defun mistkafka/habitica/create-habitica-task-for-current-headline (&optional headline-info silence-error)
  "为当前任务创建一个habitica任务"
  (interactive)
  (unless headline-info
    (setq headline-info (mistkafka/habitica/get-current-headline-info)))
  (if (and (string= "TODO" (gethash "todo-keyword" headline-info))
           (not (gethash "habitica-id" headline-info)))
      (let* ((res (mistkafka/habitica/create-task headline-info))
             (res-data (alist-get 'data res))
             (id   (alist-get 'id res-data))
             (type (alist-get 'type res-data)))
        (org-set-property "HABITICA-ID" id)
        (org-set-property "HABITICA-TYPE" type)
        (message "Success: 关联habitica任务！"))
    (unless silence-error
      (message "Skip: 任务不处于'TODO'状态，或已经关联了habitica!"))))

(defun mistkafka/habitica/make-habitica-task-done-for-current-headline (&optional headline-info silence-error)
  "将当前headline的标记为完成一次。"
  
  (interactive)
  
  (unless headline-info
    (setq headline-info (mistkafka/habitica/get-current-headline-info)))
  
  (if (and (or (string= "DONE" (gethash "todo-keyword" headline-info))
               (string= "NEXT" (gethash "todo-keyword" headline-info)))
           (gethash "habitica-id" headline-info))
      (progn
        (mistkafka/habitica/make-task-done (gethash "habitica-id" headline-info))
        (message "Success: 对应habitica任务 Up Score!"))
    (unless silence-error
      (message "Skip: 任务不处于'DONE'状态，或未关联habitica!"))))

(defun mistkafka/habitica/del-habitica-task-for-current-headline (&optional headline-info silence-error)
  "将当前headline对应的habitica task删除。"
  
  (interactive)
  
  (unless headline-info
    (setq headline-info (mistkafka/habitica/get-current-headline-info)))
  
  (if (and (string= "CANCELLED" (gethash "todo-keyword" headline-info))
           (gethash "habitica-id" headline-info))
      (progn
        (mistkafka/habitica/del-task (gethash "habitica-id" headline-info))
        (org-delete-property "HABITICATE-ID")
        (message "Success: 成功删除habitica任务！")
        )
    (unless silence-error
      (message "Skip: 任务不处于'CANCELLED'状态，或没有关联habitica!"))))

(defun mistkafka/habitica/headline-state-change-callback ()
  "org-after-todo-state-change-hook的callback，用来处理任务的增删改。
依次执行CRUD对应的command. 每个command内部会做validate，判断是否需要执行。"
  (let ((headline-info (mistkafka/habitica/get-current-headline-info)))
    (mistkafka/habitica/del-habitica-task-for-current-headline       headline-info t)
    (mistkafka/habitica/create-habitica-task-for-current-headline    headline-info t)
    (mistkafka/habitica/make-habitica-task-done-for-current-headline headline-info t)
    ))

(defun mistkafka/habitica/cron ()
  (interactive)
  (mistkafka/habitica/request "/cron" "POST" nil))

(add-hook 'org-after-todo-state-change-hook 'mistkafka/habitica/headline-state-change-callback)

(provide 'mistkafka-habitica)
