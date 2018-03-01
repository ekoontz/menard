(ns babel.reader
  [:refer-clojure :exclude [get-in resolve]]
  [:require
   [babel.config :as config]
   [babel.directory :refer [models]]
   [babel.generate :refer [generate]]
   [babel.korma :as korma :refer [convert-keys-from-string-to-keyword init-db read-array]]
   [cljstache.core :as cljstache]
   [clojure.core.async :refer [>! alts!! timeout chan go]]
   [clojure.string :as string]
   [clojure.data.json :as json :refer [read-str write-str]]
   [clojure.java.io :as io]
   [clojure.tools.logging :as log]
   [compojure.core :as compojure :refer [GET PUT POST DELETE ANY]]
   [dag_unify.core :as unify :refer [dissoc-paths get-in ref? strip-refs unify]]
   [korma.core :as db]
   [ring.util.response :as resp]])

(declare generate-question-and-correct-set)
(declare json-read-str)
(def ^:const wait-ms-for-question 3000)

(defn render-page
  "Pass in the template name (a string, sans its .mst filename extension), 
  the data for the page (a map), and a list of partials (keywords) 
  corresponding to like-named template filenames (e.g. [:header :footer]
  correspond to the template files header.mst and footer.mst, respectively.)"
  [template data partials]
  (try 
    (cljstache/render-resource
     (str "public/mst/" template ".mst")
     data
     (reduce (fn [accum pt] ;; "pt" is the name (as a keyword) of the partial.
               (assoc accum pt (slurp (io/resource (str "public/mst/"
                                                        (name pt)
                                                        ".mst")))))
             {}
             partials))
    (catch Exception e
      (str "could not render template: '" template "' - check for existence of: <p><h2>public/mst/"
           template ".mst</h2>"))))


;; Thanks for (defn wait) to Christian Meichsner on https://stackoverflow.com/a/30903731 
(defn wait [ms f & args]
  (let [c (chan)]
    (go (>! c (apply f args)))
    (first (alts!! [c (timeout ms)]))))

;; web service that translates babel.reader's API and an HTTP client.
;; http://localhost:3001/reader?target_spec={%22root%22:{%22italiano%22:{%22italiano%22:%22scrivere%22}}%20%22synsem%22:%20{%22sem%22:%20{%22tense%22:%22past%22,%20%22aspect%22:%22perfect%22}}}
(def routes
  (compojure/routes
   (GET "/" request
        (let [source (get (:query-params request) "source" "en")
              source-locale (get (:query-params request) "source_locale" "US")
              target "it"
              target-locale "IT"
              target-spec (json-read-str
                           (if-let [target-spec (get (:query-params request) "target-spec")]
                             target-spec "{}"))
              gcacs
              (try
                (wait wait-ms-for-question
                      (fn []
                        (generate-question-and-correct-set
                         target-spec
                         source source-locale
                         target target-locale)))
                (catch Exception e
                  {:return-a-500 true
                   :exception (str e)}))]
          (cond (nil? gcacs)

                ;; Avoid any logging here since it will further
                ;; slow down system since it's already overloaded.
                {:status 503
                 :headers {"Content-Type" "application/json;charset=utf-8"}
                 :body (write-str {:message "Capacity overload: please try again later."
                                   :spec target-spec})}

                (= true (:return-a-500 gcacs))
                (do
                  (log/error (str "Internal error: returning 500. Exception was:"
                                  (:exception gcacs)))
                  {:status 500
                   :headers {"Content-Type" "application/json;charset=utf-8"}
                   :body (write-str {:message "Unexpected internal error. Sorry: hopefully a human will work on it."})})
                
                true
                (let [source (:source gcacs)
                      targets (:targets gcacs)]
                  (log/info (str "returning normal response:"
                                 "'" source "'"
                                 " -> " targets "; spec:" target-spec))
                  {:status 200
                   :body
                   (write-str
                    {:source source
                     :source-locale source-locale
                     :targets targets
                     :target-local target-locale
                     :target-spec target-spec})}))))))

(defn generate-question-and-correct-set [target-spec
                                         source-language source-locale
                                         target-language target-locale
                                         & [lexical-filter-fn]]
  (let [source-timing? false
        source-timing-fn (if source-timing? #(time %)
                             (fn [x] x))
        target-timing? false
        target-timing-fn (if target-timing? #(time %)
                             (fn [x] x))
        
        basic-spec {:synsem {:subcat []
                             :sem {:subj {:city false}}
                             :cat :verb}
                    :modified false}
        target-spec (unify target-spec basic-spec)
        target-language-str target-language
        target-root-keyword (config/short-language-name-to-edn target-language)
        source-language (keyword source-language)
        target-language (keyword target-language)
        ;; TODO: catch possible deref NPE exception that can happen when model is not yet loaded.
        target-model @(get models target-language)

        lexical-filter-fn (if lexical-filter-fn
                            lexical-filter-fn
                            (fn [lexeme]
                              true))
        target-model
        (merge target-model
               {:index-fn
                (fn [spec]
                  (->>
                   ((:index-fn target-model) spec)
                   (filter lexical-filter-fn)))})
        
        target-expression
        (target-timing-fn (babel.generate/generate target-spec target-model))
        source-spec
        (unify/strip-refs ;; TODO: consider removing strip-refs; not clear if there is any reason why we need to do it.
         (unify/unify
          {:synsem {:sem (unify/get-in target-expression [:synsem :sem])}}
          (cond
            (= true (unify/get-in target-expression [:comp :synsem :pronoun]))
            {:comp {:phrasal false
                    :synsem {:pronoun true}}}
            true :top)
          basic-spec))

        ;; TODO: catch possible deref NPE exception that can happen when model is not yet loaded.
        source-model @(get models source-language)
        source-expression
        (source-timing-fn (generate source-spec source-model))]
    (let [pairing
          {:target ((:morph target-model) target-expression) ;; TODO: hard-wired to Italian.
           :pred (unify/strip-refs
                  (unify/get-in target-expression [:synsem :sem :pred]))
           :tense (unify/get-in target-expression [:synsem :sem :tense])
           :sem (unify/get-in target-expression [:synsem :sem])
           :subj (unify/get-in target-expression [:synsem :sem :subj :pred])
           :source (if source-expression
                     ((:morph @(get models source-language))
                      source-expression
                      :from-language target-language-str))}]
      (if (:source pairing)
        (str (:target pairing) " => " (:source pairing))
        (str " FAILED: " (dissoc pairing :source)))
      {:source (:source pairing)
       :targets [(:target pairing)]
       :target-spec target-spec
       :target-roots [(get-in target-expression
                              [:root target-root-keyword target-root-keyword])]
       :target-semantics (strip-refs
                          (get-in target-expression [:synsem :sem]))})))
    
(defn get-lexeme [canonical language & [ spec ]]
  "get a lexeme from the database given the canonical form, given a
  language and optionally additional filter specification"
  ;; TODO: does not support filter yet.
  (let [results (db/exec-raw [(str "SELECT structure
                                      FROM lexeme
                                     WHERE canonical=?
                                       AND language=?")
                              [canonical language]]
                             :results)]
    (map (fn [result]
           (json-read-str (.getValue (:structure result))))
         results)))

(defn generate-all [spec language]
  "find all sentences in the expression table matching 'spec' in a given language."
  (let [spec (unify spec
                    {:synsem {:subcat '()}})

        ;; normalize for JSON lookup
        json-input-spec (if (= :top spec)
                          {}
                          spec)
        
        json-spec (json/write-str (strip-refs json-input-spec))
        ]
    (log/debug (str "looking for expressions in language: " language " with spec: " spec))
    (log/debug (str "SQL: "
                   (str "SELECT surface FROM expression WHERE language='" language "' AND structure @> "
                        "'" json-spec "'")))

    (let [results (db/exec-raw [(str "SELECT structure
                                        FROM expression 
                                       WHERE language=? 
                                         AND active=true
                                         AND structure @> "
                                     "'" json-spec "'")
                                [language]]
                               :results)]
      (map (fn [result]
             (json-read-str (.getValue (:structure result))))
           results))))

(defn read-all [spec language]
  (let [spec (dissoc (unify spec
                            {:synsem {:subcat '()}})
                     :unify/serialized)

        ;; normalize for JSON lookup
        json-input-spec (if (= :top spec)
                          {}
                          spec)
        
        json-spec (json/write-str (strip-refs json-input-spec))
        ]
    (log/debug (str "looking for expressions in language: " language " with spec: " spec))
    (let [results (db/exec-raw [(str "SELECT DISTINCT surface,structure
                                        FROM expression 
                                       WHERE active=true 
                                         AND language=? 
                                         AND structure @> "
                                     "'" json-spec "' ORDER BY SURFACE ASC")
                                [language]]
                               :results)]
      (log/debug (str "Number of expressions that that need to be translated: " (.size results)))
      (map (fn [result]
             {:surface (:surface result)
              :structure (json-read-str (.getValue (:structure result)))})
           results))))

(defn read-one [spec language {no-older-than :no-older-than}]
  (log/debug (str "read-one: no-older-than: " no-older-than))
  (let [no-older-than (or no-older-than nil)
        spec (unify spec
                    {:synsem {:subcat '()}})

        ;; normalize for JSON lookup
        json-input-spec (if (= :top spec)
                          {}
                          spec)
        
        json-spec (json/write-str (strip-refs json-input-spec))]
    (log/debug (str "looking for all expressions in language: " language " with spec: " spec))

    (let [results (db/exec-raw [(str "SELECT count(*)
                                        FROM expression 
                                       WHERE language=? 
                                         AND ((?::timestamp IS NULL) OR 
                                              (expression.created > ?::timestamp))
                                         AND active=true
                                         AND structure @> ?::jsonb "
                                     " LIMIT 1 ")
                                [language no-older-than no-older-than (json/write-str spec)]]
                               :results)]
      (log/debug (str "results for spec:" spec " : " (string/join "," results))))

    (let [results (db/exec-raw [(str "SELECT surface,structure
                                        FROM expression 
                                       WHERE language=? 
                                         AND ((?::timestamp IS NULL) OR 
                                              (expression.created > ?::timestamp))
                                         AND active=true
                                         AND structure @> ?::jsonb "
                                     " LIMIT 1 ")
                                [language no-older-than no-older-than (json/write-str spec)]]
                               :results)]
      (if (not (empty? results))
        (log/debug (str "existing results:" (string/join "," results))))
      (first (map (fn [result]
                    {:surface (:surface result)
                     :structure (json-read-str (.getValue (:structure result)))})
                  results)))))

(defn contains [spec]
  "Find the sentences in English that match the spec, and the set of Italian sentences that each English sentence contains."
    (let [spec (if (= :top spec)
                 {}
                 spec)
          json-spec (json/write-str (strip-refs spec))
          ;; TODO: use '?' below, not string concatenation
          results (db/exec-raw [(str "SELECT DISTINCT * 
                                        FROM (SELECT english.surface   AS en,
                                                      italiano.surface AS it,               
                                   italiano.structure->'synsem'->'sem' AS italian_semantics,
                                    english.structure->'synsem'->'sem' AS english_semantics         
                                                FROM expression AS italiano
                                          INNER JOIN expression AS english                                 
                                                  ON english.structure @> '" json-spec "'
                                                 AND italiano.language = 'it' AND italiano.active=true
                                                 AND english.language  = 'en' AND english.active=true
                                                 AND (italiano.structure->'synsem'->'sem') @> 
                                                     (english.structure->'synsem'->'sem')) AS pairs 
                                    ORDER BY pairs.en")
                                []]
                               :results)]
      results))

;; (map #(str (get-in % [:en]) " / " (get-in % [:it]) " | ") (contains {:synsem {:sem {:pred :mangiare :subj {:pred :noi}}}}))

(defn get-meaning [input-map]
  "create a language-independent syntax+semantics that can be translated efficiently. The :cat specification helps speed up generation by avoiding searching syntactic constructs that are different from the desired input."
  (if (seq? input-map)
    (map get-meaning
         input-map)
    {:synsem {:cat (get-in input-map [:synsem :cat] :top)
              :sem (get-in input-map [:synsem :sem] :top)
              :subcat (get-in input-map [:synsem :subcat] :top)}}))

(defn zipmap-with-fn [the-keys the-vals acc]
  (if (not (empty? the-keys))
    (let [key (first the-keys)
          val (first the-vals)]
      (if (not (= :nothing (get acc key :nothing)))
        (zipmap-with-fn (rest the-keys) (rest the-vals)
                        (merge
                         acc
                         {key (cons val
                                    (get acc key))}))
					 (zipmap-with-fn (rest the-keys) (rest the-vals)
							 (merge
							  acc
							  {key (list val)}))))
    acc))

(defn group-by-canonical-form [lexicon]
  (zipmap-with-fn (map :surface lexicon)
                  (map :structure lexicon)
                  {}))

(defn read-lexicon [language]
  (let [results (db/exec-raw [(str "SELECT canonical,structure FROM lexeme WHERE language=?")
                              [language]]
                             :results)]
    (group-by-canonical-form
     (map (fn [x]
            {:surface (:canonical x)
             :structure (json-read-str (.getValue (:structure x)))})
          results))))

;; TODO: document why we need this
;; TODO: verbcoach should use this, not config/json-read-str
;; TODO: move this to babel/globals.cljc or something similar
(declare json-value-converter)

(defn json-read-str [json]
  (json/read-str json
                 :key-fn keyword
                 :value-fn json-value-converter))

(defn json-value-converter [k v]
  (cond
    (and (or (= k :english)
             (= k :espanol)
             (= k :français)
             (= k :italiano)
             (= k "english")
             (= k "espanol")
             (= k "français")
             (= k "italiano")
             (= k "latin")
             (= k :participle)
             (= k :past)
             (= k :past-participle)
             (= k :present)
             (= k :1sing)
             (= k :2sing)
             (= k :3sing)
             (= k :note))
         (not (map? v)))
    (str v)
    
    (and (string? v)
         (= (nth v 0) \:))
    (keyword (string/replace-first v ":" ""))
    
    (string? v)
    (keyword v)

    (= (type v)
       org.postgresql.jdbc.PgArray)
    (vec (.getArray v))

    
    :else v))

  
