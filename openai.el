(require 'json "json.el" 'noerror)

(defvar openai-api-key
  (-last-item
   (process-lines "gpg" "--decrypt" "/home/benj/repos/codex-play2/token.gpg" ))
  "Credentials for openai")


(url-retrieve "" (json-encode object))


(defun openai-predict-callback(status)
  (goto-char url-http-end-of-headers)
  (let* ((data (buffer-string))
         (json-object-type 'hash-table)
         (json-array-type 'list)
         (json-key-type 'string))
    (message "Result: %S" (json-read-from-string data))))
