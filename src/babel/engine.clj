(ns babel.engine
  (:refer-clojure :exclude [get-in merge])
  (:use [hiccup core page])
  (:require
   [clojure.data.json :as json]
   [clojure.string :as string]
   [clojure.tools.logging :as log]
   [compojure.core :as compojure :refer [GET PUT POST DELETE ANY]]
   [dag-unify.core :refer [fail? get-in merge strip-refs unify unifyc]]

   [babel.forest :as forest]
   [babel.html :refer [tablize]]
   [babel.ug :refer (head-principle)]))

(declare lookup)
(declare generate-from-request)
(declare resolve-model)

(def routes
  (compojure/routes
   (GET "/lookup" request
       (lookup request))

  (GET "/generate" request
       (generate-from-request request))))

;; TODO: use a option map/destructuring thing.
(defn generate [spec language-model & [{do-enrich :do-enrich}]]
  (let [do-enrich (if do-enrich do-enrich true)
        spec (unify spec
                    {:synsem {:subcat '()}})
        language-model (if (future? language-model)
                         @language-model
                         language-model)

        debug (log/debug (str "pre-enrich spec: " spec))

        spec (if (and do-enrich (:enrich language-model))
               ((:enrich language-model)
                spec
                (:lexicon language-model))
               spec)
        debug (if (seq? spec)
                (.size
                 (map #(log/debug (str "post-enrich spec: " %))
                      spec))
                (log/debug (str "post-enrich spec: " spec)))
        ]
    (forest/generate spec 
                     (:grammar language-model)
                     (:lexicon language-model)
                     (:index language-model)
                     (:morph language-model))))

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

               ]])}))))

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
   (eval (symbol "babel.english/small"))))

(def possible-preds [:top])

(defn lang-to-lexicon [language]
  (cond
   (= language "en")
   (eval (symbol "babel.english/lexicon"))
   true
   (eval (symbol "babel.italiano/lexicon"))))

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
              (for [[k v] @lexicon]
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

               ])})))

(defn get-meaning [input-map]
  "create a language-independent syntax+semantics that can be translated efficiently. The :cat specification helps speed up generation by avoiding searching syntactic constructs that are different from the desired input."
  (if (seq? input-map)
    (map get-meaning
         input-map)
    {:synsem {:cat (get-in input-map [:synsem :cat] :top)
              :sem (get-in input-map [:synsem :sem] :top)
              :subcat (get-in input-map [:synsem :subcat] :top)}}))
