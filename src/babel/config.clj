(ns babel.config
  (:require
   [clojure.data.json :as json]
   [clojure.string :as string]
   [hiccup.core :refer (html)]
   [dag_unify.core :refer [unify]]))

(declare short-language-name-to-edn)
(declare short-language-name-to-long)
(declare short-language-name-from-match)

;; sqlnames: 'english','espanol' <- note: no accent on 'n' ,'italiano', ..
(defn sqlname-from-match [match-string]
  (cond (nil? match-string)
        (str "(no sqlname for language: (nil) detected).")

        (re-find #"espanol" (string/lower-case match-string))
        "espanol"
        (re-find #"español" (string/lower-case match-string))
        "espanol"
        (re-find #"spanish" (string/lower-case match-string))
        "espanol"

        (re-find #"english" (string/lower-case match-string))
        "english"

        (re-find #"french" (string/lower-case match-string))
        "français"
        (re-find #"français" (string/lower-case match-string))
        "français"
        (re-find #"francais" (string/lower-case match-string))
        "français"

        (re-find #"italiano" (string/lower-case match-string))
        "italiano"
        (re-find #"italian" (string/lower-case match-string))
        "italiano"

        :else
        (str "(no sqlname for language:" match-string " detected.")))

(defn language-to-root-keyword [short-language-name]
  (sqlname-from-match (short-language-name-to-long short-language-name)))

(defn language-to-spec-path [short-language-name]
  "Take a language name like 'it' and turn it into an array like: [:root :italiano :italiano]."
  (let [language-keyword-name (language-to-root-keyword short-language-name)
        language-keyword (keyword language-keyword-name)]
    [:root language-keyword language-keyword]))

(defn language-to-root-spec [short-language-name root]
  "Take a language name like 'it' and a verb root and turn it into a map like: {:root {:italiano {:italiano <root>}}}."
  (let [language-keyword-name (language-to-root-keyword short-language-name)
        language-keyword (keyword language-keyword-name)]
    {:root {language-keyword {language-keyword root}}}))

;; TODO: throw exception rather than 'unknown for unknown languages.
(defn short-language-name-to-long [lang]
  (cond (= lang "it") "Italian"
        (= lang "en") "English"
        (= lang "es") "Spanish"
        (= lang "fr") "French"
        true (str "unknown:" lang)))

+;; TODO: throw exception rather than "(no shortname for language)"
(defn short-language-name-from-match [match-string]
   (cond (nil? match-string)
         (str "(no shortname for language: (nil) detected).")

         (re-find #"espanol" (string/lower-case match-string))
         "es"
         (re-find #"español" (string/lower-case match-string))
         "es"

         (re-find #"english" (string/lower-case match-string))
         "en"

         (re-find #"french" (string/lower-case match-string))
         "fr"
         (re-find #"français" (string/lower-case match-string))
         "fr"
         (re-find #"francais" (string/lower-case match-string))
         "fr"

         (re-find #"italiano" (string/lower-case match-string))
         "it"
         (re-find #"italian" (string/lower-case match-string))
         "it"

         :else
         (str "(no shortname for language:" match-string " detected.")))

;; TODO: throw exception rather than "unknown language"
(defn short-language-name-to-edn [lang]
  (cond (= lang "en") :english
        (= lang "es") :espanol
        (= lang "fr") :français
        (= lang "it") :italiano
        true (str "unknown lang: " lang)))
  
