;; TODO: consider moving this code into babel.generate itself.
(ns babel.engine ^{:doc "API wrapper around babel.generate"}
  (:refer-clojure :exclude [get-in])
  (:require
   #?(:clj [clojure.data.json :as json])
   [clojure.string :as string]
   #?(:clj [clojure.tools.logging :as log])
   #?(:cljs [babel.logjs :as log]) 
   #?(:clj [compojure.core :as compojure :refer [GET PUT POST DELETE ANY]])
   [babel.generate :as generate]
   #?(:clj [babel.html :refer [tablize]])
   [babel.ug :refer (head-principle)]
   [dag_unify.core :refer [fail? get-in strip-refs unify unifyc]]
   #?(:clj [hiccup.page :refer [include-css]])
   #?(:clj [hiccup.core :refer [html]])))

(declare lookup)
(declare generate-from-request)
(declare resolve-model)

#?(:clj
   (def routes
     (compojure/routes
      (GET "/lookup" request
           (lookup request))

      (GET "/generate" request
           (generate-from-request request)))))

(defn exception [error-string]
  #?(:clj
     (throw (Exception. (str ": " error-string))))
  #?(:cljs
     (throw (js/Error. error-string))))

;; TODO: (defn generate [...] (take 1 (generate-all ...)))
;; TODO: this should just call (take 1 (generate-all ..))
;;(fo (generate :top medium {:max-total-depth 2}))
(defn generate [spec language-model & {:keys [add-subcat do-enrich max-total-depth truncate-children]
                                       :or {add-subcat true
                                            do-enrich true
                                            max-total-depth generate/max-total-depth
                                            truncate-children true}}]
  (log/debug (str "engine/generate with spec: " (strip-refs spec) "; max-total-depth: " max-total-depth "; enrich: " do-enrich "; truncate-children: " truncate-children))
  (let [language-model (if (future? language-model)
                         @language-model
                         language-model)
        grammar (:grammar language-model)]
    (cond (:generate-fn language-model)
          ((:generate-fn language-model) spec)
          (empty? grammar)
          (do
            (log/error (str "grammar is empty."))
            (exception (str "grammar is empty.")))
          true
          (let [spec (if (or (= false add-subcat)
                             (fail? (unify spec {:synsem {:subcat '()}}))
                             (not (= :none (get-in spec [:synsem :subcat] :none))))
                       spec

                       ;; else:
                       (unify spec
                              {:synsem {:subcat '()}}))

                debug (log/debug (str "pre-enrich spec: " spec))

                ;; if a generate-only lexicon is supplied by the language model, use that.
                lexicon (if (get-in language-model [:generate :lexicon])
                          (get-in language-model [:generate :lexicon])
                          (get-in language-model [:lexicon]))

                post-enrich-spec (if (and do-enrich (:enrich language-model))
                                   ((:enrich language-model)
                                    spec
                                    lexicon)
                                   spec)

                spec (if (not (empty? post-enrich-spec))
                       post-enrich-spec
                       (list spec))

                debug (log/debug (str "post-enrich spec: "
                                      (string/join ";" spec)))
                
                check-for-empty-spec (if (empty? spec)
                                       (do (log/error (str "post-enrich spec is empty!"))
                                           (exception (str "post-enrich spec is empty!"))))
                debug (if (seq? spec)
                        (count
                         (map #(log/debug (str "post-enrich spec: " %))
                              spec))
                        (log/debug (str "post-enrich spec: " spec)))
                ]
            (first (map (fn [each-spec]
                          (generate/generate each-spec language-model
                                             :truncate-children truncate-children
                                             :max-total-depth max-total-depth))
                        (if (or (seq? spec) (vector? spec))
                          spec
                          [spec])))))))

#?(:clj
(defn generate-from-request [request]
  "respond to an HTTP client's request with a generated sentence, given the client's desired spec, language name, and language model name."
  ;; TODO: 'pred' request param here is deprecated: use spec's {:synsem {:sem {:pred}}} instead.
  (let [pred (keyword (get-in request [:params :pred] :top))
        spec (get-in request [:params :spec])
        spec (if spec (json/read-str spec
                                     :key-fn keyword
                                     :value-fn (fn [k v]
                                                 (cond (string? v)
                                                       (keyword v)
                                                       :else v)))
                 :top)

        lang (get-in request [:params :lang])
        model (resolve-model (get-in request [:params :model]) lang)
        debug (get-in request [:params :debug] false)
        unified (unify {:synsem {:sem {:pred pred}}}
                       spec)]
    (log/info (str "generate with pred: " pred "; lang: " lang))
    (let [expression (generate unified model)
          semantics (strip-refs (get-in expression [:synsem :sem]))
          results (merge
                   {:spec spec
                    (keyword lang) ((:fo model) expression)
                    :semantics semantics})]
      (log/info (str "generated expression: " ((:fo model) expression) " with pred: " (get-in expression [:synsem :sem :pred])))
      (log/debug (str "semantics of expression: " semantics))

      (if (not (= "true" (get-in request [:params :debug])))
        ;; non-debug mode:
        {:status 200
         :headers {"Content-Type" "application/json;charset=utf-8"
                   "Cache-Control" "no-cache, no-store, must-revalidate"
                   "Pragma" "no-cache"
                   "Expires" "0"}
         :body (json/write-str results)}

        ;; debug mode:
        {:status 200
         :headers {"Content-Type" "text/html;charset=utf-8"
                   "Cache-Control" "no-cache, no-store, must-revalidate"
                   "Pragma" "no-cache"
                   "Expires" "0"}
         :body (html
                [:head
                 [:title "generate: debug"]
                 (include-css "/css/fs.css")
               (include-css "/css/layout.css")
               (include-css "/css/quiz.css")
               (include-css "/css/style.css")
               (include-css "/css/debug.css")
               ]
              [:body
               [:div

                [:div.major

                 [:h2 "input"]

                 [:table
                  [:tr
                   [:th "spec"]
                   [:td
                    (tablize (json/read-str (get-in request [:params :spec])
                                            :key-fn keyword
                                            :value-fn (fn [k v]
                                                        (cond (string? v)
                                                              (keyword v)
                                                              :else v))))
                    ]]]]


               [:div.major
                [:h2 "Generated"]

                (tablize expression)

                ]

               [:div.major
                [:h2 "output"]
                (tablize results)]
                
               [:div#request {:class "major"}
                [:h2 "request"]
                (tablize request)]

               ]])})))))

#?(:clj
   (defn resolve-model [model lang]
     (cond
       (= model "en-small")
       (eval (symbol "babel.english/small"))

       (= model "it-small")
       (eval (symbol "babel.italiano/small"))

       ;; defaults if no model is given
       (= lang "en")
       (eval (symbol "babel.english/small"))

       (= lang "it")
       (eval (symbol "babel.italiano/small"))

       true ;; TODO: throw exception "no language model" if we got here.
       (eval (symbol "babel.english/small")))))

#?(:clj
   (defn lang-to-lexicon [language]
     (cond
       (= language "en")
       (eval (symbol "babel.english/lexicon"))
       true
       (eval (symbol "babel.italiano/lexicon")))))

#?(:clj
   (def possible-preds [:top]))

#?(:clj
   (defn lookup [request]
     (let [lang (get-in request [:params :lang] "en") ;; if no lang specified, use english.
           lexicon (lang-to-lexicon lang)
           spec (if (not (= :null (get-in request [:params :spec] :null)))
                  (json/read-str (get-in request [:params :spec])
                                 :key-fn keyword
                                 :value-fn (fn [k v]
                                             (cond (string? v)
                                                   (keyword v)
                                                   :else v)))
                  :fail)

           spec (unifyc spec {:synsem {:aux false
                                       :infl :infinitive
                                       :sem {:intensified false}}})

           intermediate
           (into {}
                 (for [[k v] lexicon]
                   (let [filtered-v
                         (filter #(not (fail? (unifyc % spec)))
                                 v)]
                     (if (not (empty? filtered-v))
                       [k filtered-v]))))

           results
           {(keyword lang)
            (string/join "," (sort (keys intermediate)))}]
       
       (if (not (= "true" (get-in request [:params :debug])))
         ;; non-debug mode:
         {:status 200
          :headers {"Content-Type" "application/json;charset=utf-8"
                    "Cache-Control" "no-cache, no-store, must-revalidate"
                    "Pragma" "no-cache"
                    "Expires" "0"}
          :body (json/write-str results)}

         ;; debug mode:
         {:status 200
          :headers {"Content-Type" "text/html;charset=utf-8"
                    "Cache-Control" "no-cache, no-store, must-revalidate"
                    "Pragma" "no-cache"
                    "Expires" "0"}
          :body (html
                 [:head
                  [:title "lookup: debug"]
                  (include-css "/css/fs.css")
                  (include-css "/css/layout.css")
                  (include-css "/css/quiz.css")
                  (include-css "/css/style.css")
                  (include-css "/css/debug.css")
                  ]
                 [:body
                  [:div

                   [:div.major

                    [:h2 "input"]

                    [:table

                     [:tr
                      [:th "requested-spec"]
                      [:td
                       (tablize (json/read-str (get-in request [:params :spec])
                                               :key-fn keyword
                                               :value-fn (fn [k v]
                                                           (cond (string? v)
                                                                 (keyword v)
                                                                 :else v))))]]
                     [:tr 
                      [:th "spec"]

                      [:td (tablize spec)]
                      ]
                     ]
                    ]
                   ]
                  [:div.major
                   [:h2 "intermediate"]
                   [:table
                    (map #(html [:tr 
                                   [:th.intermediate %]
                                   [:td.intermediate (map (fn [each-val]
                                                            (html [:div.intermediate (tablize each-val)]))
                                                          (get intermediate %))]])
                         (keys intermediate))]]

                  [:div.major
                   [:h2 "output"]
                   [:pre (json/write-str results)]]

                  [:div#request {:class "major"}
                   [:h2 "request"]
                   (tablize request)]

                  ])}))))

(defn get-meaning [input-map]
  "create a language-independent syntax+semantics that can be translated efficiently. The :cat specification helps speed up generation by avoiding searching syntactic constructs that are different from the desired input."
  (if (seq? input-map)
    (map get-meaning
         input-map)
    {:synsem {:cat (get-in input-map [:synsem :cat] :top)
              :sem (get-in input-map [:synsem :sem] :top)
              :subcat (get-in input-map [:synsem :subcat] :top)}}))

(defn expression [model spec & {:keys [truncate-children]
                                :or {truncate-children true}}]
  (let [no-language (if (nil? model)
                             (throw (Exception. "No target language model was supplied.")))

        sentence (generate spec model :truncate-children truncate-children)

        check (if (nil? sentence)
                (let [message (str "Could not generate a sentence for spec: " spec " for language: " (:language model)
                                   " with model named: " (:name model))]
                  (log/error message)
                  (throw (Exception. message))))

        sentence (merge sentence {:spec spec})

        sentence (if (:morph-walk-tree model)
                   (merge ((:morph-walk-tree model) sentence)
                          sentence)
                   (do (log/warn (str "there is no morph-walk-tree function for the model:"
                                      (:name model) " of language: "
                                      (:language model)))
                       sentence))
        sentence (let [subj (get-in sentence
                                    [:synsem :sem :subj] :notfound)]
                   (cond (not (= :notfound subj))
                         (do
                           (log/debug (str "subject constraints: " subj))
                           (unify sentence
                                  {:synsem {:sem {:subj subj}}}))
                         true
                         sentence))]
    sentence))

