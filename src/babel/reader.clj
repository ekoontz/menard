(ns babel.reader
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
   [dag_unify.core :as u :refer [dissoc-paths ref? pprint strip-refs unify]]
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
              q-and-a
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
          ;; at this point q-and-a is either:
          ;; - nil: the (wait) timed out; meaning the
          ;; system is either over-capacity (CPU-starved)
          ;; or the underlying algorithm is simply not
          ;; efficient with the supplied inputs.
          ;; - {:return-a-500 true} The generation attempt
          ;; threw an exception of some kind, the value of
          ;; which is available in the :exception key.
          (cond (nil? q-and-a)

                ;; Avoid any logging here since it will further
                ;; slow down system since it's already overloaded.
                {:status 503
                 :headers {"Content-Type" "application/json;charset=utf-8"}
                 :body (write-str {:message "Capacity overload: please try again later."
                                   :spec target-spec})}

                (= true (:return-a-500 q-and-a))
                (do
                  (log/error (str "Internal error: returning 500. Exception was:"
                                  (:exception q-and-a)))
                  {:status 500
                   :headers {"Content-Type" "application/json;charset=utf-8"}
                   :body (write-str {:message "Unexpected internal error. Sorry: hopefully a human will work on it."})})
                
                true
                (let [source (:source q-and-a)
                      targets (:targets q-and-a)]
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
                             :sem {:subj {:prop {:city false}}}
                             :cat :verb}
                    :modified false}
        target-spec (unify target-spec basic-spec)
        target-language-str target-language
        target-root-keyword (config/short-language-name-to-edn target-language)
        source-language (keyword source-language)
        target-language (keyword target-language)
        ;; TODO: catch possible deref NPE exception that can happen when model is not yet loaded.
        target-model @@(get models target-language)

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
        (target-timing-fn
         (binding [babel.generate/truncate? true]
           (babel.generate/generate target-spec target-model)))
        
        source-spec
        (u/strip-refs ;; TODO: consider removing strip-refs; not clear if there is any reason why we need to do it.
         (u/unify
          {:synsem {:sem (u/get-in target-expression [:synsem :sem])}}

          basic-spec))

        ;; TODO: catch possible deref NPE exception that can happen when model is not yet loaded.
        source-model @@(get models source-language)
        source-expression
        (source-timing-fn
         (binding [babel.generate/truncate? true]
           (generate source-spec source-model)))]
    (let [pairing
          {:target ((:morph target-model) target-expression)
           :pred (u/strip-refs
                  (u/get-in target-expression [:synsem :sem :pred]))
           :tense (u/get-in target-expression [:synsem :sem :tense])
           :sem (u/get-in target-expression [:synsem :sem])
           :subj (u/get-in target-expression [:synsem :sem :subj :pred])
           :source (if source-expression
                     ((:morph @@(get models source-language))
                      source-expression
                      :show-notes true
                      :from-language target-language-str))}]
      (if (:source pairing)
        (str (:target pairing) " => " (:source pairing))
        (str " FAILED: " (dissoc pairing :source)))
      {:source (:source pairing) :source-spec source-spec
       :targets [(:target pairing)]
       :target-spec (u/strip-refs target-spec)
       :target-roots [(u/get-in target-expression
                                [:root target-root-keyword target-root-keyword])]
       :target-semantics (strip-refs
                          (u/get-in target-expression [:synsem :sem]))})))
    
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

(defn get-meaning [input-map]
  "create a language-independent syntax+semantics that can be translated efficiently. The :cat specification helps speed up generation by avoiding searching syntactic constructs that are different from the desired input."
  (if (seq? input-map)
    (map get-meaning
         input-map)
    {:synsem {:cat (u/get-in input-map [:synsem :cat] :top)
              :sem (u/get-in input-map [:synsem :sem] :top)
              :subcat (u/get-in input-map [:synsem :subcat] :top)}}))

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
             (= k :plur)
             (= k :present)
             (= k :1sing)
             (= k :2sing)
             (= k :3sing)
             (= k :note))
         (not (map? v)))
    (str v)
    
    (and (string? v)
         (empty? v))
    v

    (and (string? v)
         (= (nth v 0) \:))
    (keyword (string/replace-first v ":" ""))
    
    (string? v)
    (keyword v)

    (= (type v)
       org.postgresql.jdbc.PgArray)
    (vec (.getArray v))

    
    :else v))


