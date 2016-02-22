(ns babel.italiano.writer
  (:refer-clojure :exclude [get-in]))

(require '[babel.engine :as engine])
(require '[babel.italiano.grammar :refer [small]])
(require '[babel.italiano.lexicon :refer [lexicon]])
(require '[babel.italiano.morphology :as morph])
(require '[babel.writer :as writer :refer [process write-lexicon]])
(require '[clojure.tools.logging :as log])
(require '[dag_unify.core :refer (fail? get-in strip-refs unify)])
(require '[clojure.string :refer [join]])

(defn expression [spec]
  (engine/expression small spec))

(defn fo [spec]
  (morph/fo spec))

(defn rewrite-lexicon []
  (write-lexicon "it" lexicon))

(defn generate-one-verb [root-form & [count]]
  (log/info (str "generating examples with root-form:" root-form))
  (writer/generate-from-spec
   small
   {:root {:italiano {:italiano root-form}}}
   [{:synsem {:sem {:tense :conditional}}}
    {:synsem {:sem {:tense :future}}}
    {:synsem {:sem {:tense :present}}}
    {:synsem {:sem {:aspect :progressive
                    :tense :past}}}
    {:synsem {:sem {:aspect :perfect
                    :tense :past}}}]
   [{:gender :masc}
    {:gender :fem}]
   [:1st :2nd :3rd]
   [:sing :plur]
   count))

(defn tutti [ & [count lexeme]]
  (let [count (if count (Integer. count) 10)
        ;; subset of the lexicon: only verbs which are infinitives and that can be roots:
        ;; (i.e. those that have a specific (non- :top) value for [:synsem :sem :pred])
        lexemes (if lexeme (list (get lexicon lexeme))
                    (vals lexicon))
        root-verbs 
        (zipmap
         (keys lexicon)
         (map (fn [lexeme-set]
                (filter (fn [lexeme]
                          (and
                           (= (get-in lexeme [:synsem :cat]) :verb)
                           (= (get-in lexeme [:synsem :infl]) :top)
                           (not (= :top (get-in lexeme [:synsem :sem :pred] :top)))))
                        lexeme-set))
              lexemes))

        tutti
        (reduce concat
                (map (fn [key]
                       (get root-verbs key))
                     (sort (keys root-verbs))))]

    (write-lexicon "it" lexicon)
    (log/info (str "done writing lexicon."))
    (log/info (str "generating examples with this many verbs:"
                   (.size tutti)))
    (.size (pmap (fn [verb]
                   (log/trace (str "verb: " (strip-refs verb)))
                   (let [root-form (get-in verb [:italiano :italiano])]
                     (generate-one-verb root-form count)))
                 tutti))))

