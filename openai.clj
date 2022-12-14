(ns openai
  (:require
   [clojure.java.shell :as shell]
   [clojure.string :as str]
   [babashka.fs :as fs]
   [clojure.set :as set]
   [babashka.curl :as curl]
   [cheshire.core :as json]))

(def api-key
  (or
   (when (fs/exists? "token.gpg")
     (str/trim (:out (shell/sh "gpg" "--decrypt" "token.gpg"))))
   (str/trim (:out (shell/sh "pass" "ben-open-ai")))))

(def api {:edit "https://api.openai.com/v1/edits" :completion "https://api.openai.com/v1/completions"})

(defn get-models []
  (curl/get
   "https://api.openai.com/v1/models"
   {:headers {"Content-Type" "application/json"
              "Authorization" (str "Bearer " api-key)}}))

(comment
  (get-models)
  *1)

(defn openai-completion [{:keys [model temperature] :as opts}]
  (-> (curl/post (api :completion)
                 {:headers {"Content-Type" "application/json"
                            "Authorization" (str "Bearer " api-key)}
                  :body (json/encode (merge {:temperature 0 :max_tokens (* 4 256)} opts))})
      :body json/decode
      (get "choices")
      (get 0)))

(openai-completion
 {:model "code-cushman-001"
  :prompt "curl https://api.openai.com/v1/completions \\\n-H \"Content-Type: application/json\" \\\n-H \"Authorization: Bearer YOUR_API_KEY\" \\\n-d '{\"model\": \"text-davinci-003\", \"prompt\": \"Say this is a test\", \"temperature\": 0, \"max_tokens\": 7}'

; an emacs lisp function that implements this api call"})
(openai-completion
 {:model "code-davinci-002"
  :prompt ";; returns all points within a given manhatten distance\n(defn "})
(openai-completion
 {:model "text-davinci-003"
  :prompt "The science of past biology is called"})

;; (openai-completion {:model "code-davinci-002" :prompt })

(defn openai-edit [{:keys [model temperature]
                    :or {temperature 0}
                    :as opts}]
  (-> (curl/post (api :edit)
                 {:headers {"Content-Type" "application/json"
                            "Authorization" (str "Bearer " api-key)}
                  :body (json/encode opts)})
      :body json/decode
      (get "choices")
      (get 0)))

(openai-edit
 {:model "text-davinci-edit-001"
  :instruction "Fix typos"
  :input "Monday is the first day of the wek"})

{"text" "Monday is the first day of the week\n",
 "index" 0}

(openai-edit
 {:model "code-davinci-edit-001"
  :instruction "Remove redundant let, preserve bindings from all let. Fix whitespace."
  :input  (prn-str '(let [foo 10] (let [bar 11] (+ foo bar))))})

(get *1 "text")
#_

(let [foo 10
      bar 11]
  (+ foo bar))
(openai-edit
 {:model "code-davinci-edit-001"
  :instruction "Use destructing instead of saying first and second sensor."
  :input  (prn-str

           '(defn
              beacon->coverage
              [[sensor beacon]]

              (for
                  [x
                   (range
                    (dec (first sensor))
                    (inc (first sensor)))
                   y
                   (range
                    (dec (second sensor))
                    (inc (second sensor)))
                   :let
                   [d (distance sensor [x y])]
                   :when
                   (<= d (distance sensor beacon))]
                  [x y]))

)})

(openai-edit
 {:model "code-davinci-edit-001"
  :instruction "Emacs lisp. Supplante the rest of the args"
  :input  (prn-str
           '(defcustom openai-api-edit-persistent-message t))})

{"text" "(defn beacon->coverage [[[sx sy] beacon]] (for [x (range (dec sx) (inc sx)) y (range (dec sy) (inc sy)) :let [d (distance [sx sy] [x y])] :when (<= d (distance [sx sy] beacon))] [x y]))\n", "index" 0}


(openai-completion
 {:model "code-cushman-001"
  :prompt "(ns Y2022.day16\n  (:require [clojure.string :as str]))\n\n;; graph\n;; nodes\n(defn dfs [])\n\n"})


{"text" "\n;;; day16.clj ends here", "index" 0, "logprobs" nil, "finish_reason" "stop"}
