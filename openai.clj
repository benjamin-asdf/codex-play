(ns openai
  (:require
   [clojure.java.shell :as shell]
   [clojure.string :as str]
   [babashka.fs :as fs]
   [clojure.set :as set]
   [babashka.curl :as curl]
   [cheshire.core :as json]))

(def api-key (when (fs/exists? "token.gpg")
             (str/trim (:out (shell/sh "gpg" "--decrypt" "token.gpg")))))

(defn openai-completion [{:keys [model temperature max-tokens]
                          :or {temperature 0
                               max-tokens (* 4 256)}
                          :as opts}]
  (-> (curl/post (api :edit)
                 {:headers {"Content-Type" "application/json"
                            "Authorization" (str "Bearer " api-key)}
                  :body (json/encode opts)})
      :body json/decode
      (get "choices")
      (get 0)))

(def api {:edit "https://api.openai.com/v1/edits" :completion "https://api.openai.com/v1/completions"})

(openai-completion {:model "code-davinci-edit-001" : "Say this is a test"})

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

{"text" "Monday is the first day of the week\n", "index" 0}

(openai-edit
 {:model "code-davinci-edit-001"
  :instruction "Remove redundant let, preserve bindings from all let. Fix whitespace."
  :input  (prn-str '(let [foo 10] (let [bar 11] (+ foo bar))))})

(get *1 "text")

=>
(let [foo 10
      bar 11]
  (+ foo bar))
