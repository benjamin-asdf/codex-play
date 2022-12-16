(setq openai-api-key
      (or (when (file-exists-p "token.gpg" )
            (-last-item
             (process-lines "gpg" "--decrypt" "token.gpg")))
          (string-trim (shell-command-to-string "pass ben-open-ai"))))
