;;;  -*- lexical-binding: t; -*-

(require 'promise)
(require 'request)

(cl-defun promise-request (url
			   &key
			   (type "GET")
			   (params nil)
			   (data nil)
			   (files nil)
			   (parser nil)
			   (headers nil)
			   (timeout request-timeout)
			   ;; (status-code nil)
			   ;; (sync nil)
			   (response (make-request-response))
			   ;; (complete nil)
			   (unix-socket nil))
  "通过promise-new转发request.
这里选择把一个个keyword手动抄写过去。不知道为什么，使用过于复杂的“解构” `resolve'总是会不见."
  (promise-new
   (lambda (resolve _reject)
     (request url
	      :success (cl-function (lambda (&key data &allow-other-keys)
				      (funcall resolve data)))
	      :error   (cl-function (lambda (&key response &allow-other-keys)
				      (funcall _reject response)))

	      :type type
	      :params params
	      :data data
	      :files files
	      :parser parser
	      :headers headers
	      :timeout timeout
	      :response response
	      :unix-socket unix-socket))))

(provide 'promise-request)
