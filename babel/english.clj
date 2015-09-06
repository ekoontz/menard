(ns babel.english
  (:refer-clojure :exclude [get-in]))

(require '[clojure.tools.logging :as log])

(require '[babel.cache :refer (build-lex-sch-cache create-index spec-to-phrases)])
(require '[babel.forest :as forest])
(require '[babel.english.grammar :as gram])
(require '[babel.english.lexicon :as lex])
(require '[babel.english.morphology :as morph])
(require '[babel.lexiconfn :refer (compile-lex map-function-on-map-vals unify)])
(require '[babel.over :refer [over]])
(require '[babel.parse :as parse])
(require '[babel.english.pos :as epos :refer (intransitivize transitive transitivize verb-subjective)])
(require '[babel.ug :refer :all])
(require '[dag-unify.core :as unify :refer (dissoc-paths get-in strip-refs)])

(def get-string morph/get-string)
(def grammar gram/grammar)
(def lexicon-source lex/lexicon-source)

(def lexicon (future (-> (compile-lex lex/lexicon-source 
                                      morph/exception-generator 
                                      morph/phonize morph/english-specific-rules)
                         ;; make an intransitive version of every verb which has an
                         ;; [:sem :obj] path.
                         intransitivize

                         ;; if verb does specify a [:sem :obj], then fill it in with subcat info.
                         transitivize)))

(defn lookup [token & [use-lexicon]]
  "return the subset of lexemes that match this token from the lexicon."
  (let [lexicon (if use-lexicon use-lexicon @lexicon)]
    (morph/analyze token #(get lexicon %))))

(def en lookup)

(def index nil)
;; TODO: trying to print index takes forever and blows up emacs buffer:
;; figure out how to change printable version to show only keys and first value or something.
(def index (future (create-index grammar (flatten (vals @lexicon)) head-principle)))

(defn parse [string & [parse-lexicon]]
  (let [lexicon (if parse-lexicon parse-lexicon @lexicon)]
    (parse/parse string lexicon #(lookup % lexicon) grammar)))

(defn sentence [ & [spec]]
  (let [spec (if spec spec :top)]
    (forest/generate spec grammar index (flatten (vals lexicon)))))

(declare small)

(defn generate [ & [spec model]]
  (if (seq? spec)
    (map generate spec)
    (let [spec (if spec spec :top)
          model (if model model small)
          model (if (future? model) @model model)]
      (forest/generate spec
                       (:grammar model)
                       (:lexicon model)
                       (:index model)))))

;; TODO: copied from italiano.clj: factor out to forest/.
(defn generate-all [ & [spec {use-grammar :grammar
                              use-index :index
                              use-lexicon :lexicon}]]
  (let [spec (if spec spec :top)
        use-grammar (if use-grammar use-grammar grammar)
        use-index (if use-index use-index index)
        use-lexicon (if use-lexicon use-lexicon lexicon)]
    (log/info (str "using grammar of size: " (.size use-grammar)))
    (log/info (str "using index of size: " (.size @use-index)))
    (if (seq? spec)
      (mapcat generate-all spec)
      (forest/generate-all spec use-grammar
                           (flatten (vals @use-lexicon))
                           use-index))))

(def small
  (future
    (let [grammar
          (filter #(or (= (:rule %) "s-conditional-nonphrasal-head")
                       (= (:rule %) "s-present-nonphrasal-head")
                       (= (:rule %) "s-future-nonphrasal-head")
                       (= (:rule %) "s-imperfetto-nonphrasal-head")
                       (= (:rule %) "s-past-nonphrasal-head")
                       (= (:rule %) "s-aux"))
                  grammar)

          lexicon
          (into {}
                (for [[k v] @lexicon]
                  (let [filtered-v
                        (filter #(or (= (get-in % [:synsem :cat]) :verb)
                                     (= (get-in % [:synsem :propernoun]) true)
                                     (= (get-in % [:synsem :pronoun]) true))
                                v)]
                    (if (not (empty? filtered-v))
                      [k filtered-v]))))
          ]
      {:name "small"
       :language "en"
       :morph morph/fo
       :grammar grammar
       :lexicon lexicon
       :for {:es ;; a lexicon specific to when we want to use Español as a target.
             (into {}
                   (for [[k v] lexicon]
                     (let [filtered-v
                           (filter #(or (= :unset (get-in % [:target]))
                                        (= :es (get-in % [:target])))
                                   v)]
                       (if (not (empty? filtered-v))
                         [k filtered-v]))))

             :it  ;; a lexicon specific to when we want to use Italiano as a target.
             (into {}
                   (for [[k v] lexicon]
                     (let [filtered-v
                           (filter #(or (= :unset (get-in % [:target]))
                                        (= :it (get-in % [:target])))
                                   v)]
                       (if (not (empty? filtered-v))
                         [k filtered-v]))))}
       :index (create-index grammar (flatten (vals lexicon)) head-principle)})))

(def small-plus-vp-pronoun
  (future
    (let [grammar
          (filter #(or (= (:rule %) "s-conditional-nonphrasal-head")
                       (= (:rule %) "s-conditional-phrasal-head")
                       (= (:rule %) "s-present-nonphrasal-head")
                       (= (:rule %) "s-present-phrasal-head")
                       (= (:rule %) "s-future-nonphrasal-head")
                       (= (:rule %) "s-future-phrasal-head")
                       (= (:rule %) "s-imperfetto-nonphrasal-head")
                       (= (:rule %) "s-imperfetto-phrasal-head")
                       (= (:rule %) "s-past-nonphrasal-head")
                       (= (:rule %) "s-past-phrasal-head")
                       (= (:rule %) "s-aux")
                       (= (:rule %) "vp-past")
                       (= (:rule %) "vp-pronoun"))
                  grammar)

          lexicon
          (into {}
                (for [[k v] @lexicon]
                  (let [filtered-v
                        (filter #(or (= (get-in % [:synsem :cat]) :verb)
                                     (= (get-in % [:synsem :propernoun]) true)
                                     (= (get-in % [:synsem :pronoun]) true))
                                v)]
                    (if (not (empty? filtered-v))
                      [k filtered-v]))))
          ]
      {:name "small-plus-vp-pronoun"
       :language "en"
       :morph morph/fo
       :grammar grammar
       :lexicon lexicon
       :for {:es ;; a lexicon specific to when we want to use Español as a target.
             (into {}
                   (for [[k v] lexicon]
                     (let [filtered-v
                           (filter #(or (= :unset (get-in % [:target]))
                                        (= :es (get-in % [:target])))
                                   v)]
                       (if (not (empty? filtered-v))
                         [k filtered-v]))))

             :it  ;; a lexicon specific to when we want to use Italiano as a target.
             (into {}
                   (for [[k v] lexicon]
                     (let [filtered-v
                           (filter #(or (= :unset (get-in % [:target]))
                                        (= :it (get-in % [:target])))
                                   v)]
                       (if (not (empty? filtered-v))
                         [k filtered-v]))))}
       :index (create-index grammar (flatten (vals lexicon)) head-principle)})))

(def medium
  (future
    (let [lexicon
          (into {}
                (for [[k v] @lexicon]
                  (let [filtered-v v]
                    (if (not (empty? filtered-v))
                      [k filtered-v]))))]
      {:name "medium"
       :grammar grammar
       :morph morph/fo
       :lexicon lexicon
       :index (create-index grammar (flatten (vals lexicon)) head-principle)})))

(defn inflection [inflection]
  "turn a keyword describing an inflection (e.g. past, present, future) into a specification."
  ;; TODO: gen.js should use this rather than its own implementation.
  (cond 
   (= inflection :conditional)
   {:synsem {:infl :conditional}}
   
   (= inflection :future)
   {:synsem {:infl :futuro}}

   (= inflection :imperfect)
   {:synsem {:infl :imperfetto}}

   (= inflection :passato)
   {:synsem {:sem {:aspect :perfect
                   :tense :past}}}
        
   (= inflection :present)
   {:synsem {:infl :present
             :sem {:tense :present}}}
        
   ;; no constraints: generate anything.
   true
   :top))

(def vp-pronoun (first (filter #(= "vp-pronoun" (get % :rule)) (:grammar @small-plus-vp-pronoun))))
(def s-past-nonphrasal-head (first (filter #(= "s-past-nonphrasal-head" (get % :rule)) (:grammar @small-plus-vp-pronoun))))
(def s-past-phrasal-head (first (filter #(= "s-past-phrasal-head" (get % :rule)) (:grammar @small-plus-vp-pronoun))))
