(ns menard.test.translate
  (:require [menard.english :as en]
            [menard.español :as es]
            [menard.exception :refer [exception]]
            [menard.lexiconfn :as l]
            [menard.translate.spec :refer [es-to-en-spec]]
            [menard.nederlands :as nl]
            [menard.nederlands.complete :as complete]            
            [menard.translate.spec :refer [nl-to-en-spec]]
            [dag_unify.core :as u]
            [dag_unify.serialization :refer [deserialize serialize]]
            [clojure.test :refer [deftest is]]
            #?(:clj [clojure.tools.logging :as log])
            #?(:cljs [cljslog.core :as log])))

(deftest parse-yo-quiero
  (is (seq (es/parse "yo quiero"))))

(defn es-to-en-str [es-str]
  (-> es-str es/parse first es-to-en-spec en/generate en/morph))

(deftest yo-quiero
  (is (= (es-to-en-str "yo quiero")
         "I want")))

(defn nl-to-en-str [nl-str]
  (log/info (str "nl-to-en-str input: " nl-str "; parse: " (->> nl-str nl/parse first nl/syntax-tree)))
  (log/info (str "nl-to-en-str input: " nl-str "; en-spec: " (->> nl-str nl/parse first nl-to-en-spec serialize)))
  (log/info (str "nl-to-en-str input: " nl-str "; en-generated: " (->> nl-str nl/parse first nl-to-en-spec en/generate en/syntax-tree)))
  (-> nl-str nl/parse first nl-to-en-spec en/generate en/morph))

;; "they need"
(def plural-third-person-alternatives
  (let [all-emojis (:all menard.morphology/emoji-set-2)
        all-emoji-pairs
        (->> all-emojis
             (mapcat (fn [emo1]
                       (map (fn [emo2]
                              (str emo1 emo2))
                            all-emojis))))]
    (set
     (map (fn [emoji-pair]
            (str "they " emoji-pair " need the money"))
          all-emoji-pairs))))

(deftest nodig
  (log/info (str "nodig tests.."))
  (is (= (nl-to-en-str "ik heb het geld nodig")
         "I need the money"))
  (is (contains? plural-third-person-alternatives
                 (nl-to-en-str "ze hebben het geld nodig"))))

(def informal-alternatives
  (set
   (map (fn [emoji]
          (str "you " emoji " need them"))
        (-> menard.morphology/emoji-set-2 :informal))))

(def formal-alternatives
  (set
   (map (fn [emoji]
          (str "you " emoji " need yourself"))
        (-> menard.morphology/emoji-set-2 :formal))))

;; "we need ourselves"))
(def plural-first-person-alternatives
  (let [all-emojis (:all menard.morphology/emoji-set-2)
        all-emoji-pairs
        (->> all-emojis
             (mapcat (fn [emo1]
                       (map (fn [emo2]
                              (str emo1 emo2))
                            all-emojis))))]
    (set
     (map (fn [emoji-pair]
            (str "we " emoji-pair " need ourselves"))
          all-emoji-pairs))))

(deftest pronoun-nodig
  (log/info (str "nodig+pronoun tests.."))
  (is (contains? informal-alternatives
                 (nl-to-en-str "jij hebt hun nodig")))
  (is (= (nl-to-en-str "zij heeft zich nodig") "she needs herself"))
  (is (contains? plural-first-person-alternatives
                 (nl-to-en-str "wij hebben ons nodig")))
  (is (contains? formal-alternatives
                 (nl-to-en-str "u hebt u nodig"))))

;; If true, generates Dutch, then parses it, so we test
;; parsing as well as generation.
(def intermediate-parsing? true)

;; for additional debugging
(def show-english-spec? false)

(defn sort-parses [parses]
  (->> parses
       ;; prefer parses where:
       ;; 1. subcat is empty (e.g. noun phrases rather than nbars)
       ;; 2. without a top-level :mod feature (e.g. prefer intensifier-phrase rather than intensifier-bar)
       (sort (fn [x y] (cond
                         (and (vector? (u/get-in x [:subcat]))
                              (empty? (u/get-in x [:subcat]))
                              (vector? (u/get-in y [:subcat]))
                              (not (empty? (u/get-in y [:subcat] []))))
                         true

                         (and (true? (u/get-in x [:mods-nested?]))
                              (false? (u/get-in y [:mods-nested?])))
                         true
                         
                         (and (= :verb (u/get-in x [:cat]))
                              (= :verb (u/get-in y [:cat]))
                              (= ::none (u/get-in x [:mod] ::none))
                              (not (= ::none (u/get-in y [:mod] ::none))))
                         true

                         :else false)))))

(defn transfer-fn [i model]
  (let [model-name (:name model "untitled")
        generate (fn [spec]
                   (binding [menard.generate/allow-lexeme-backtracking? true]
                     (nl/generate spec model)))]
    (->> (-> nl/expressions
             (nth i)
             ((fn [spec]
                (log/debug (str "trying to generate Dutch expression spec: " spec " with "
                                "model: " model-name))
                spec))
             generate
             ((fn [generated]
                (is (not (nil? generated)))
                (if (nil? generated)
                  (log/error (str "failed to generate Dutch expression: i=" i)))
                generated))
             ((fn [structure]
                {:structure structure
                 :surface (nl/morph structure)}))
             ((fn [{surface :surface
                    structure :structure}]
                (log/debug (str surface " : " (nl/syntax-tree structure)))
                (if intermediate-parsing?
                  (do
                    (log/debug (str "calling nl/parse on:'" surface "'"))
                    (-> (->> (nl/parse surface)

                             ;; remove partial parses, if any:
                             (filter #(not (= true (:menard.parse/partial? %))))

                             ;; prefer parses where:
                             ;; 1. subcat is empty (e.g. noun phrases rather than nbars)
                             ;; 2. more semantic information (where the sem value is larger in string length terms)
                             sort-parses
                             
                             ;; and take only one parse to test against:
                             (take 1))
                        first
                        ((fn [parsed-structure]
                           (if (not (nil? parsed-structure))
                             {:surface surface
                              :syntax-tree (nl/syntax-tree structure)
                              :structure parsed-structure}
                             {:surface surface
                              :syntax-tree (nl/syntax-tree structure)
                              :structure structure})))))
                  ;; intermediate-parsing? is false:
                  {:surface surface
                   :syntax-tree (nl/syntax-tree structure)
                   :structure structure})))
             ((fn [{surface :surface
                    structure :structure
                    syntax-tree :syntax-tree}]
                (log/debug (str "checking result of nl/parse: " (nl/syntax-tree structure)))
                (if (nil? structure)
                  (log/warn (str "couldn't nl/parse: '" surface "' input tree was: " syntax-tree)))
                (is (not (nil? structure)))
                [structure])))
         (map (fn [tree] {:nl-st (nl/syntax-tree tree) :sem (u/get-in tree [:sem]) :nl (nl/morph tree) :en-spec (nl-to-en-spec tree)}))
         (map (fn [{nl :nl nl-st :nl-st en-spec :en-spec sem :sem}]
                (log/debug (str "nl-st: " nl-st))
                (log/debug (str "en-spec: " (l/pprint en-spec)))
                {:nl-st nl-st
                 :sem sem
                 :nl nl
                 :en-spec en-spec
                 :en (do
                       (log/debug (str "trying to generate with en-spec: " (serialize en-spec)))
                       (let [expression (-> en-spec en/generate)]
                         (when (nil? expression)
                           (log/error (str "couldn't en/generate: " (serialize en-spec)))
                           (log/error (str "nl was: " nl-st)))
                         (when (not (nil? expression))
                           (log/debug (str "successfully generated " (en/syntax-tree expression) " from en-spec: " (serialize en-spec) " and source NL: " nl-st)))
                         (is (not (nil? expression)))
                         expression))}))
         (filter (fn [{en :en}] (not (nil? en))))
         (take 1)
         (map (fn [{nl :nl en :en en-spec :en-spec nl-st :nl-st sem :sem}]
                (let [en-st (en/syntax-tree en)
                      en (en/morph en)
                      retval {:i i
                              :sem sem
                              :en-spec en-spec
                              :model (:name model)
                              :nl (str "\"" nl "\"")
                              :nl-st (str "\"" nl-st "\"")
                              :en (str "\"" en "\"")}
                      retval (if show-english-spec?
                               (assoc retval :en-spec (serialize en-spec))
                               retval)]
                  (log/info (str i ": " nl " -> " en))
                  (is (seq nl))
                  (is (seq en))
                  retval)))
         doall)))

(deftest transfer
  (let [start 0
        end (count nl/expressions)
        do-this-many 20]

    (binding [menard.generate/log-all-rules? false]
      (->>
       (range start end)
       (map (fn [i]
              (doall
               (take do-this-many
                     (repeatedly #(transfer-fn i @complete/model))))))
       doall))))

