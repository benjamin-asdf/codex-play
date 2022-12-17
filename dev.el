(setq openai-api-key
      (or (when (file-exists-p "token.gpg" )
            (-last-item
             (process-lines "gpg" "--decrypt" "token.gpg")))
          (string-trim (shell-command-to-string "pass ben-open-ai"))))


(meow-leader-define-key `("." .
                          ,(let ((m (make-sparse-keymap)))
                           (define-key m (kbd "e") #'openai-api-davinci-edit)
                           m)
                         ;; openai-api-map
                         ))
