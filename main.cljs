(ns
    main
    (:require
     [clojure.string :as str]
     [promesa.core :as p]
     [applied-science.js-interop :as j]
     ["node:child_process" :as proc]
     [goog.string :as gstring]
     ["openai$default" :as openai]))

(def token (str/trim (str (proc/execFileSync "pass" #js["ben-open-ai"]))))

(def configuration (openai/Configuration. #js {:apiKey token}))

(def oa (openai/OpenAIApi. configuration))

(defn completion [opts]
  (. oa createCompletion (clj->js opts)))


(p/let [res (completion
             {:model "text-davinci-002"
              :prompt (gstring/format
                       " Suggest three names for an animal that is a superhero.

Animal: Cat
Names: Captain Sharpclaw, Agent Fluffball, The Incredible Feline
Animal: Dog
Names: Ruff the Protector, Wonder Canine, Sir Barks-a-Lot
Animal: %s
Names: "
                       "Snake")
              :temperature 0.6})]
  (def res res))
(j/get-in res [:data :choices])


#js
[#js {:text "\n\nSly as a Fox, The Green Avenger, The Python", :index 0, :logprobs nil, :finish_reason "stop"}]

;; https://beta.openai.com/docs/guides/code/introduction
;; a user should have read this to know more profoundly what they are doing

(p/let
    [res
     (completion
      {:model "code-davinci-002"
       :prompt ";; a emacs lisp command that moves an inner let to an outer let
;; example
;; (let [foo 10]
;;   (let [bar 11])
;;   (+ bar foo))
;; becomes
;; (let [foo 10
;;       bar 11]
;;   (+ bar foo))
"
       :temperature 0
       :stop "\n"
       :max_tokens (* 2 256)})]
    (println (j/get-in res [:data :choices]))
    (def resp (j/get-in res [:data :choices])))


(p/let
    [res
     (completion
      {:model "code-davinci-002"
       :prompt ";; define a elisp function to query OpenAI davinci codex at https://api.openai.com/v1/engines/davinci/completions for completion suggestions\n;; without a hardcoded api token\n;; do not type the part in (json-encode), just make a symbol called 'data'"
       :temperature 0
       ;; :stop "\n"
       :max_tokens (* 7 256)})]
    (println
     (j/get-in res [:data :choices]))
    (def
      resp
      (j/get-in res [:data :choices])))
(p/let
    [res
     (completion
      {:model "code-davinci-002"
       :prompt "(ns openai
  (:require
   [babashka.curl :as curl]
   [clojure.string :as str]
   [clojure.java.shell :as shell]))

(def token (str/trim (:out (shell/sh \"pass\"
                           \"ben-open-ai\"))))


curl https://api.openai.com/v1/edits \\\n  -H 'Content-Type: application/json' \\\n  -H 'Authorization: Bearer YOUR_API_KEY' \\\n  -d '{
  \"model\": \"text-davinci-edit-001\",
  \"input\": \"What day of the wek is it?\",
  \"instruction\": \"Fix the spelling mistakes\"
}'

;; write this curl command using babashka curl
;; token is api token
"
       :temperature 0
       ;; :stop "\n"
       :max_tokens (* 7 256)})]
    (println
     (j/get-in res [:data :choices]))
    (def
      resp
      (j/get-in res [:data :choices])))





(p/let
    [res
     (p/catch
         (completion
          {:model "code-davinci-edit-001"
           :prompt ";; the below code returns an api error \"We could not parse the JSON body of your request.\"
;; can you fix the code

(defvar openai-token (string-trim-left (shell-command-to-string (concat \"pass\" \" \" \"ben-open-ai\"))))

(defun openai-davinci-completions (data)
  (let ((url-request-method \"POST\")
        (url-request-extra-headers
         `((\"Content-Type\" . \"application/json\")
           (\"Authorization\" . ,(concat \"Bearer \" openai-token))))
        (url-request-data
         (json-encode data)))
    (with-current-buffer
        (url-retrieve-synchronously \"https://api.openai.com/v1/completions\")
      (goto-char url-http-end-of-headers)
      (json-read))))

(defun openai-davinci-completions-prompt (prompt choices)
  (let* ((completion-ignore-case t)
         (answer (completing-read prompt choices)))
    (if (equal answer \"\")
        (car choices)
      answer)))

(defun openai-davinci-completions-choices (data)
  (let* ((response (openai-davinci-completions data))
         (choices (mapcar (lambda (c) (cdr (assoc 'text c))) (cdr (assoc 'choices response)))))
    (openai-davinci-completions-prompt \"Completions: \" choices)))


(openai-davinci-completions
 '((model . \"text-davinci-003\")
   (prompt . \"Say this is a test\")
   (temperature . 0)
   (max_tokens . 1000)))
"
           :temperature 0
           ;; :stop "\n"
           :max_tokens (* 7 256)})
         (fn [e] (println e)))]
    (println
     (j/get-in res [:data :choices]))
    (def
      resp
      (j/get-in res [:data :choices])))
