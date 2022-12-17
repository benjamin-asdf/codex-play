;; -*- lexical-binding: t; -*-

;;; Commentary:

;; This package provides bindings for openai apis
;;
;; - edit
;; - completions
;;
;; You need to set `openai-api-key`


;; right now it is your problem to manually
;; fix the code from the result buff
;; but I don't want to override your buffer file
;; maybe some scratch experience
;; with hist

;;; Code:


(defcustom openai-api-key
  nil
  "Credentials for openai.

This can be set to a string or a function that returns a string.

If you use auth-sources (eg. pass) then you can set this to a
function that calls `auth-source-user-or-password' with the
appropriate parameters."
  :type 'string
  :group 'openai)

(defcustom openai-api-response-buffer-as-input
  t
  "Whether or not subsequent `openai-api-davinci-edit-send-it`
calls will use the content of the response buffer.
The default is `t`."
  :type 'boolean
  :group 'openai)

(defvar openai-api-urls `(:edit "https://api.openai.com/v1/edits"
                          :completion "https://api.openai.com/v1/completions"))

(defcustom openai-api-eval-spinner-type
  'progress-bar-filled
  "Appearance of the instruction edit spinner.

See `spinner-types' variable."
  :type '(choice (vector :tag "Custom")
                 (symbol :tag "Predefined" :value 'progress-bar-filled)
                 (list :tag "List"))
  :group 'openai-api)

(defcustom openai-api-show-eval-spinner t
  "When true, show the evaluation spinner in the mode line."
  :type 'boolean
  :group 'openai-api)

(defcustom openai-api-eval-spinner-delay 1
  "Amount of time, in seconds, after which the evaluation spinner will be shown."
  :type 'integer
  :group 'openai-api)

(defconst openai-api-edit-buffer "*ai-instructions*"
  "Buffer to use for the openai edit instructions prompt")

(defvar-local openai-api-edit-target-buffer nil)

(defvar openai-api-edit-instructions-history nil)

(defcustom openai-api-balance-parens-fn
  (if (fboundp 'lispy--balance) #'lispy--balance
    #'openai-api-balance-parens)
  "Function to balance parens.")

(defvar openai-api-edit-models
  '((code-davinci . "code-davinci-edit-001")
    (text-davinci . "text-davinci-edit-001")))

(defvar openai-api-completion-models
  '((text-davinci . "text-davinci-003")))

(defun openai-api-balance-parens (str)
  (let ((parens-count 0)
	(brackets-count 0)
	(braces-count 0)
	(new-str ""))
    (dolist (char (string-to-list str))
      (cond ((eq char ?\()
	     (setq parens-count (1+ parens-count))
	     (setq new-str (concat new-str (char-to-string char))))
	    ((eq char ?\))
	     (setq parens-count (1- parens-count))
	     (setq new-str (concat new-str (char-to-string char))))
	    ((eq char ?\[)
	     (setq brackets-count (1+ brackets-count))
	     (setq new-str (concat new-str (char-to-string char))))
	    ((eq char ?\])
	     (setq brackets-count (1- brackets-count))
	     (setq new-str (concat new-str (char-to-string char))))
	    ((eq char ?\{)
	     (setq braces-count (1+ braces-count))
	     (setq new-str (concat new-str (char-to-string char))))
	    ((eq char ?\})
	     (setq braces-count (1- braces-count))
	     (setq new-str (concat new-str (char-to-string char))))
	    (t
	     (setq new-str (concat new-str (char-to-string char))))))
    (while (> parens-count 0)
      (setq new-str (concat new-str ")"))
      (setq parens-count (1- parens-count)))
    (while (< parens-count 0)
      (setq new-str (concat "(" new-str))
      (setq parens-count (1+ parens-count)))
    (while (> brackets-count 0)
      (setq new-str (concat new-str "]"))
      (setq brackets-count (1- brackets-count)))
    (while (> braces-count 0)
      (setq new-str (concat new-str "}"))
      (setq braces-count (1- braces-count)))
    new-str))

(defun opanai-api-split-words (string)
  "Split STRING into a list of substrings, each containing one more word."
  (let ((words (split-string string " " t)))
    (cl-loop for i from 1 to (length words)
             collect (mapconcat 'identity (cl-subseq words 0 i) " "))))

(defun openai-api-choices ()
  "Return a list of choices from the current buffer."
  (goto-char (point-min))
  (re-search-forward "\n\n")
  (let ((data (json-read)))
    (cl-map
     'list
     (lambda (d)
       (assoc-default 'text d))
     (assoc-default 'choices data))))

(defun openai-api-retrieve (data cb &optional cbargs endpoint)
  "Retrieve DATA from the openai API.

CB is a function to call with the results.

CBARGS is a list of arguments to pass to CB.

ENDPOINT is the API endpoint to use."
  (let ((url-request-method "POST")
        (url-request-extra-headers `(("Content-Type" . "application/json")
                                     ("Authorization" . ,(concat
                                                          "Bearer "
                                                          openai-api-key))))
        (url-request-data (json-encode data)))
    (url-retrieve
     (plist-get openai-api-urls (or endpoint :completion))
     cb
     cbargs)))

(defun openai-api-retrieve-sync (data &optional endpoint)
  "Retrieve DATA from the openai API.

ENDPOINT is the API endpoint to use."
  (let*
      ((url-request-method "POST")
       (url-request-extra-headers `(("Content-Type" . "application/json")
                                    ("Authorization" . ,(concat
                                                         "Bearer "
                                                         openai-api-key))))
       (endpoint
        (plist-get
         openai-api-urls
         (or (assoc-default 'endpoint data)
             (let ((model (assoc-default 'model data)))
               (cond
                ((rassoc model openai-api-completion-models) :completion)
                ((rassoc model openai-api-edit-models) :edit)
                (t  :completion))))))
       (data (assq-delete-all :endpoint data))
       (url-request-data (json-encode data)))
    (url-retrieve-synchronously endpoint)))


;; each stragie is a function of input ->
;; list of requests to make

;; (defvar openai-api-stragies
;;   `((cushman-small .
;;                    (lambda (params)
;;                      (list
;;                       `((model . "code-cushman-001")
;;                         (max_tokens . 14)
;;                         (temperature . 0)
;;                         (prompt . ,(plist-get parms :input)))))))
;;   )

;; sync.. ?

(defun openai-api-sync (strategies)
  (cl-loop
   for strat in strategies
   append (with-current-buffer (openai-api-retrieve-sync strat)
            ;; (pop-to-buffer (current-buffer))
            (mapcar openai-api-balance-parens-fn (openai-api-choices)))))


;; how many chars should be cutoff
;; how often do we ask
;; separate commands for different use cases


;; retry 3-5 times
;; take the ones with "stop"

(defun openai-api-completions ()
  "Try to use a fast model for completions of smaller size."
  (interactive)
  (let* ((beg (min
               (save-excursion
                 (backward-paragraph 1)
                 (point))
               (save-excursion
                 (beginning-of-defun)
                 (point))))
         (end (point))
         (prompt (buffer-substring beg end)))
    (insert
     (completing-read
      "code-cushman-001: "
      (with-current-buffer
          (openai-api-retrieve-sync
           `((model . "code-cushman-001")
             (max_tokens . 30)
             (temperature . 0)
             (prompt . ,prompt)))
        (mapcar openai-api-balance-parens-fn
                (mapcan
                 #'opanai-api-split-words
                 (openai-api-choices))))))))

(defun openai-api-buffer-backwards-dwim ()
  (if
      (region-active-p
       (buffer-substring
        (region-beginning)
        (region-end)))
      (let* ((beg (save-excursion
                    (cl-loop for i from (point) downto (1+ (point-min))
                             do (forward-char -1)
                             finally return (point))))
             (end (point))
             (input))
        (buffer-substring beg end))))

(defun openai-api-completion-completing-read-1 (strategies)
  (insert
   (completing-read
    "AI completions: "
    (openai-api-sync strategies))))

(defun openai-api-complete-text-small ()
  (interactive)
  (openai-api-completion-completing-read-1
   (list
    `((model . ,(assoc-default
                 'text-davinci
                 openai-api-completion-models))
      (prompt . ,(openai-api-buffer-backwards))
      (max_tokens . 100)
      (temperature . 0)
      (top_p . 1)
      (frequency_penalty . 0)
      (presence_penalty . 0)))))

(defun openai-api-completion-completing-read ()
  (interactive)
  (let* ((beg
          (save-excursion
            (cl-loop for i from (point) downto (1+ (point-min))
                     do (forward-char -1)
                     finally return (point))))
         (end (point))
         (prompt (buffer-substring beg end)))
    (insert
     (completing-read
      "code-davinci-002: "
      (with-current-buffer
          (openai-api-retrieve-sync
           `((model . "code-davinci-002")
             (max_tokens . ,(* 4 256))
             (temperature . 0)
             (prompt . ,prompt)))
        (mapcar
         openai-api-balance-parens-fn
         (cl-remove-duplicates  (mapcan
                                 (lambda (s) (split-string s "\n\n"))
                                 (openai-api-choices)))))))))

(defun openai-api-complete-code-thoroughly ()
  "Ask openai for completions.
The model is quote:
The Most capable Codex model.
Particularly good at translating natural language to code."
  (interactive)
  (openai-api-completion-completing-read-1
   `((model .  "code-davinci-002")
     (max_tokens . ,(* 4 256))
     (temperature . 0)
     (prompt . ,(openai-api-buffer-backwards)))))

(defun openai-api-edit-response-finnish ()
  "Finish editing and insert response into the target buffer."
  (interactive)
  (let ((s (buffer-string)))
    (with-current-buffer openai-api-edit-target-buffer
      (goto-char (point-max))
      (insert s))))

(defun openai-api-spinner-start (buffer)
  "Start the evaluation spinner in BUFFER.
Do nothing if `openai-api-show-eval-spinner' is nil."
  (when openai-api-show-eval-spinner
    (with-current-buffer buffer
      (spinner-start openai-api-eval-spinner-type nil
                     openai-api-eval-spinner-delay))))

;; and then we can try to merge the resp and the target buffer
;; with options to add to top or bottom, or try the merge

(defun opanai-api-latest-used-buffer (buffs)
  (car
   (mapcar #'cdr (sort (mapcan
                        (lambda (b)
                          (with-current-buffer b
                            (if-let ((w (get-buffer-window b)))
                                (list (cons (window-use-time w)
                                            (current-buffer))))))
                        buffs)
                       (lambda (a b) (> (car a) (car b)))))))

(defun openai-api-edit-resp-buffer-p (buffer)
  (with-current-buffer buffer openai-api-edit-target-buffer))

;; maybe make you select buffer with prefix command

(defun openai-api-read-instruction ()
  (read-from-minibuffer
   "Instruction: "
   nil
   nil
   nil
   'openai-api-edit-instructions-history
   (car openai-api-edit-instructions-history)))

(defun openai-api-davinci-edit (&optional instruction target-buffer model)
  "Send INSTRUCTION to OpenAI API.

INSTRUCTION is a string.

TARGET-BUFFER is a buffer.

The response is displayed in a buffer named
*openai-edit-TARGET-BUFFER-NAME*."
  (interactive (list
                (openai-api-read-instruction)
                (or openai-api-edit-target-buffer
                    (current-buffer)
                    ;; (get-buffer-create (read-buffer "Target: "))
                    )))
  (let* ((model (assoc-default
                 (or model 'code-davinci)
                 openai-api-edit-models))
         (called-from-resp-buffer (openai-api-edit-resp-buffer-p
                                   (current-buffer)))
         (resp-buffer (cond (called-from-resp-buffer
                             (current-buffer))
                            (t
                             (generate-new-buffer (concat "*openai-edit-" (buffer-name target-buffer) "*")))))
         ;; maybe to do a region select thing
         ;; assume user knows how to narrow region
         (input (with-current-buffer
                    (if called-from-resp-buffer
                        (current-buffer)
                      target-buffer)
                  (buffer-string))))
    (with-current-buffer resp-buffer
      (openai-api-spinner-start resp-buffer)
      (pop-to-buffer (current-buffer))
      (openai-api-retrieve
       `((model . ,model)
         (temperature . 0)
         (input . ,input)
         (instruction . ,instruction))
       (lambda (state)
         (unwind-protect
             (if (plist-get state :error)
                 (progn
                   (pop-to-buffer
                    (current-buffer))
                   (error
                    "Error when sending edit instructions: %s"
                    (plist-get state :error-message)))
               (let ((response (openai-api-choices)))
                 (with-current-buffer
                     resp-buffer
                   (let ((inhibit-read-only t))
                     (erase-buffer)
                     (dolist (choice
                              (mapcar
                               openai-api-balance-parens-fn
                               response))
                       (insert choice))
                     (funcall
                      (with-current-buffer
                          target-buffer
                        major-mode)))
                   (setf openai-api-edit-target-buffer target-buffer)
                   (pop-to-buffer (current-buffer)))))
           (with-current-buffer
               resp-buffer
             (when spinner-current
               (spinner-stop)))))
       nil
       :edit))))

(defun openai-api-edit-text ()
  "Use davinci for edit."
  (interactive)
  (openai-api-davinci-edit
   (openai-api-read-instruction) (current-buffer) 'text-davinci))
