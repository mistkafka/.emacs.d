(require 'cl-macs)
(require 'json)

(cl-defun json-storage/create-storage (&key path schema-alist)
  (let (
	(storage (make-hash-table :test 'equal))
	value-in-file
	(has-apply-default-value nil)
	)
    (puthash "path" path storage)
    (setq value-in-file (json-storage/get-json-from-file path))
    (cl-loop for schema in schema-alist
	     for key = (car schema)
	     for init-value = (cdr schema)
	     do (unless (gethash key value-in-file)
		  (setq has-apply-default-value t)
		  (puthash key init-value value-in-file))
	     )
    (puthash "value" value-in-file storage)
    (when has-apply-default-value
      (json-storage/save-to-file storage))
    storage
    ))

(defun json-storage/get-json-from-file (path)
  (with-temp-buffer
    (let (
	  (json-object-type 'hash-table)
	  (json-array-type 'vector)
	  (json-key-type 'string)
	  (file-content ""))
      
      (when (file-exists-p path)
	(insert-file-contents path)
	(setq file-content (buffer-string)))
      
      (if (string= file-content "")
	  (make-hash-table :test 'equal)
	(json-read-from-string file-content))
      )
    )
  )

(defun json-storage/save-to-file (storage)
  (with-temp-file (gethash "path" storage)
    (delete-region (point-min) (point-max))
    (let (
	  (json-object-type 'hash-table)
	  (json-array-type 'vector)
	  (json-key-type 'string)
	  (value (gethash "value" storage)))
      (insert (json-encode-hash-table value)))
    (json-pretty-print-buffer-ordered)
    )
  storage
  )

(defun json-storage/get-value (key storage)
  (let (
	 (storage-value (gethash "value" storage))
	 )
    (gethash key storage-value)
    ))

(defun json-storage/set-value (key value storage)
  (let (
	(storage-value (gethash "value" storage))
	)
    (puthash key value storage-value)
    (json-storage/save-to-file storage)
    ))

(provide 'json-storage)
