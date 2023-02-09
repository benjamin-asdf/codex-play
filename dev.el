(setq openai-api-key
      (or (when (file-exists-p "token.gpg" )
            (-last-item
             (process-lines "gpg" "--decrypt" "token.gpg")))
          (string-trim (shell-command-to-string "pass ben-open-ai"))))

(meow-leader-define-key
 `("." .
   ,(let ((m (make-sparse-keymap)))
      (define-key m (kbd "e") #'openai-api-davinci-edit)
      (define-key m (kbd "t") #'openai-api-complete-text-small)
      (define-key m (kbd "l") #'openai-api-explain-region)
      (define-key m (kbd "i") (defun mm/insert-todo ()
                                (interactive)
                                (insert "TODO: ")
                                (comment-line 1)))
      m)))

;; (insert
;;    (completing-read
;;     "AI completions: "
;;     (with-current-buffer (openai-api-retrieve-sync
;;                           `((model . ,(assoc-default
;;                                        'text-davinci
;;                                        openai-api-completion-models))
;;                             (prompt . "Dog\nSnake\nBird\n")
;;                             (max_tokens . 100)
;;                             (temperature . 0)
;;                             (top_p . 1)
;;                             (frequency_penalty . 0)
;;                             (presence_penalty . 0)
;;                             ;; (stop . ["\n"])
;;                             ))
;;       (pop-to-buffer (current-buffer))
;;       (mapcar openai-api-balance-parens-fn (openai-api-choices)))))

Dog
Snake
Bird
Cat
Fish
Lizard
