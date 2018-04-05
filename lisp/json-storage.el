(require 'dash)
(require 'json)

(defun json-storage/create-storage (path)
  "根据PATH创建一个storage，并从这个PATH中读值来初始化这个storage，最后返回这个storage。

storage的path表示存储value的文件路径；
storage的value表示storage的值（即缓存）；
storage的is-expire表示value是否过期。"

  (let (
	(storage (make-hash-table :test 'equal))
	)
    (puthash "path" path storage)
    (puthash "is-expire" t storage)
    (puthash "value" (json-storage/get-value storage) storage)
    storage))

(defun json-storage/get-value (storage)

  (when (gethash "is-expire" storage)
    (puthash "value" (json-storage/get-value-from-file (gethash "path" storage)) storage)
    (puthash "is-expire" nil storage))
  
  (gethash "value" storage))

(defun json-storage/get-value-from-file (path)
  (with-temp-buffer
    (let (
	  (json-object-type 'hash-table)
	  (json-array-type 'list)
	  (json-key-type 'string)
	  file-content)
      (insert-file-contents path)
      (setq file-content (buffer-string))
      (when (string= file-content "")
	(setq file-content "{}"))
      (json-read-from-string file-content)))
  )

(defun json-storage/update-value (storage value)

  (with-temp-file (gethash "path" storage)
    (puthash "value" value storage)
    (delete-region (point-min) (point-max))
    (let (
	  (json-object-type 'hash-table)
	  (json-array-type 'list)
	  (json-key-type 'string))
      (insert (json-encode-hash-table value)))
    storage))

(provide 'json-storage)
