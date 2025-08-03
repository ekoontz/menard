(ns menard.test.translate.es-en
  (:require [dag_unify.core :as u :refer [unify]]

            ;; TODO: load a subset of the default "complete" model
            ;; rather than the whole thing:
            [menard.english :as en]

            [menard.español :as es]
            [menard.lexiconfn :as l]
            [menard.morphology.emojis :as em]
            [menard.translate.es-en :as translate]
            [clojure.test :refer [deftest is]]
            #?(:clj [clojure.tools.logging :as log])
            #?(:cljs [cljslog.core :as log])))


(def developer-mode? false)

;; if you made changes to these, you can uncomment them to reload them
;; so that in turn the below model will be reloaded with these changes:

(when developer-mode?
  (load "../../../../src/menard/subcat")
  (load "../../../../src/menard/español")
  (load "../../../../src/menard/español/tenses")
  
  
  ;; reload the models every time to help with debugging the model:
  (load "../../../../src/menard/español/curated_verbs")
  ;; TODO: load a subset of the default "complete" model
  ;; rather than the whole thing:
  (load "../../../../src/menard/english/complete"))
  
(deftest subj-pred
  (is (or  (= "I want" (translate/es-to-en "yo quiero"))
           (= "I like" (translate/es-to-en "yo quiero"))
           (= "I love" (translate/es-to-en "yo quiero")))))

(deftest roundtrip-english-past-progressive
  (let [input "he used to be able to sleep"]
    (is (= input
           (->> (en/parse input)
                (map #(u/get-in % [:sem]))
                (map (fn [sem] {:sem sem :cat :verb :subcat []}))
                (map en/generate)
                (map en/morph)
                first)))))

(deftest roundtrip-english-future
  (let [input "he will be able to sleep"]
    (is (= input
           (->> (en/parse input)
                (map #(u/get-in % [:sem]))
                (map (fn [sem] {:sem sem :cat :verb :subcat []}))
                (map en/generate)
                (map en/morph)
                first)))))

(def have-fun-spec {:comp {:agr {:gender :fem
                                 :number :plur
                                 :person :3rd}}
                    :sem {:tense :present
                          :aspect :simple
                          :pred :have-fun}
                    :subcat []})

(deftest reflexive-roundtrip
  (let [input-sem (u/get-in have-fun-spec [:sem])]
    (->> (-> have-fun-spec es/generate
             es/morph es/parse)
         (map #(is "s-head-last" (u/get-in % [:rule])))
         vec)))

(deftest ustedes-se-duermen
  (is (= "[s-head-last(:present-simple){+} .ustedes +[vp-pronoun-c(:present-simple){+} .se(6) +duermen]]"
         (-> {:rule "s-head-last"
              :comp {:root "ustedes"
                     :agr {:number :plur,
                           :person :2nd
                           :formal? true}},
              :sem {:tense :present,
                    :aspect :simple},
              :head {:rule "vp-pronoun-c"
                     :head {:canonical "dormirse"}}}
             es/generate es/syntax-tree))))

(deftest ellos-cierran
  (is (= "[s-head-last(:present-simple) .ellos +cierran]"
         (-> {:rule "s-head-last"
              :comp {:root "ellos"
                     :phrasal? false ;; TODO: for now this is needed to prevent rat-holing, but hopefully it can be removed.
                     :agr {:number :plur
                           :person :3rd}}
              :sem {:tense :present
                    :aspect :simple}
              :head {:canonical "cerrar"}}
             es/generate es/syntax-tree))))

(def have-fun-spec {:comp {:agr {:gender :fem
                                 :number :plur
                                 :person :3rd}}
                    :sem {:tense :present
                          :aspect :simple
                          :pred :have-fun}
                    :subcat []})

(def preguntar-present-spec {:comp {:agr {:gender :fem
                                          :number :plur
                                          :person :3rd}}
                             :sem {:tense :present
                                   :aspect :simple
                                   :pred :ask-for}
                             :root "preguntar"
                             :subcat []})

(def preguntar-preterito-spec {:comp {:agr {:gender :fem
                                            :number :plur
                                            :person :3rd}}
                               :sem {:tense :past
                                     :aspect :perfect
                                     :pred :ask-for}
                               :root "preguntar"
                               :subcat []})
  
(deftest noun-phrases
  (is (= "the black cats"
         (translate/es-to-en "los gatos negros"))))

(def translate-es-spec
  {:reflexive? false
   :root  "llenar"
   :head {:rule "vp-aux-i"}
   :cat :verb
   :sem {:subj {:pred :they
                :gender :fem}
         :obj :none
         :tense :past
         :aspect :perfect}
   :subcat []
   :rule "s-head-last"
   :phrasal? true})

(deftest translate-test-1
  (let [es-generated (->> (repeatedly #(-> translate-es-spec es/generate))
                          (filter map?)
                          first)]
    (is (= (es/syntax-tree es-generated)
           "[s-head-last(:preterito-perfecto) .ellas +[vp-aux-i(:preterito-perfecto) +han(:explicit-subj-non-reflexive-intransitive) .llenado]]"))))

(deftest translate-test-2
  (let [es-generated (->> (repeatedly #(-> translate-es-spec es/generate))
                          (filter map?)
                          first)
        en-spec (-> es-generated translate/es-structure-to-en-structure)
        en-generated (-> en-spec en/generate)]
    (is (= (binding [menard.morphology/show-notes? false]
             (en/syntax-tree en-generated))
           "[s(:perfect) .they +[vp-aux +have(2) .filled]]"))))

(deftest translate-reflexives-1
  (let [yo-me-lavo (->> "yo me lavo" es/parse (filter #(= "s-head-last" (:rule %))))
        significant-parts-1 (->> yo-me-lavo (map #(select-keys % [:sem :reflexive?])) (map dag_unify.diagnostics/strip-refs) vec)]
    (is
     (= significant-parts-1
        [{:sem
          {:obj
           {:existential? false,
            :mod (),
            :ref {:human? true,
                  :context :informal
                  :number :sing},
            :pred :i},
           :subj {:existential? false,
                  :mod (),
                  :ref {:human? true,
                        :context :informal,
                        :number :sing},
                  :pred :i},
           :mod (),
           :pred :wash,
           :aspect :simple,
           :tense :present},
          :reflexive? true}]))
    (is (= ["[s(:present-simple){+} .I +[vp{+} +wash .myself]]"]
           (->> yo-me-lavo (map translate/es-structure-to-en-spec) (map en/generate) (map en/syntax-tree))))))

(deftest translate-reflexives-2
  (let [i-wash-myself (->> "I wash myself" en/parse (filter #(= "s" (:rule %))))
        significant-parts-2 (->> i-wash-myself (map #(select-keys % [:sem :reflexive?])) (map dag_unify.diagnostics/strip-refs) vec)]
    (is
     (= significant-parts-2
        [{:sem
          {:obj {:obj :none,
                 :existential? false,
                 :mod (),
                 :locative? false,
                 :ref {:human? true, :number :sing}},
           :subj {:existential? false,
                  :mod (),
                  :locative? false,
                  :ref {:human? true, :number :sing},
                  :pred :i},
           :mod (),
           :pred :wash,
           :aspect :simple,
           :tense :present},
          :reflexive? true}]))))

(deftest translate-reflexives-3
  (let [i-get-up (->> "I get up" en/parse (filter #(= "s" (:rule %))))
            significant-parts-3 (->> i-get-up (map #(select-keys % [:sem :reflexive?])) (map dag_unify.diagnostics/strip-refs) vec)]
        (is
         (= significant-parts-3
            [{:sem {:obj :none,
                    :iobj :none,
                    :subj {:existential? false,
                           :mod (),
                           :locative? false,
                           :ref {:human? true,
                                 :number :sing},
                      :pred :i},
                    :mod (),
                    :pred :get-up,
                    :aspect :simple,
                    :tense :present},
              :reflexive? false}])))
  
    (let [yo-me-despierto (->> "yo me despierto" es/parse (filter #(= "s-head-last" (:rule %))))
          significant-parts-4 (->> yo-me-despierto (map #(select-keys % [:sem :reflexive?])) (map dag_unify.diagnostics/strip-refs) vec)]
      (is (= significant-parts-4
             [{:sem {:obj :none,
                     :subj {:existential? false,
                            :mod (),
                            :ref {:human? true,
                                  :context :informal
                                  :number :sing},
                            :pred :i},
                     :mod (),
                     :pred :wake-up,
                     :aspect :simple,
                     :tense :present},
               :reflexive? true}]))
      (is (= ["[s(:present-simple) .I +[vp +wake .up]]"]
             (->> yo-me-despierto (map translate/es-structure-to-en-spec) (map en/generate) (map en/syntax-tree)))))

      (let [yo-me-levanto (->> "yo me levanto" es/parse (filter #(= "s-head-last" (:rule %))))
          significant-parts-5 (->> yo-me-levanto (map #(select-keys % [:sem :reflexive?])) (map dag_unify.diagnostics/strip-refs) vec)]
        (is
         (= significant-parts-5
            [{:sem {:obj :none,
                    :subj {:existential? false,
                           :mod (),
                           :ref {:human? true,
                                 :context :informal
                                 :number :sing},
                           :pred :i},
                    :mod (),
                    :pred :get-up,
                    :aspect :simple,
                    :tense :present},
              :reflexive? true}]))
        (is (= ["[s(:present-simple) .I +[vp +get .up]]"]
               (->> yo-me-levanto (map translate/es-structure-to-en-spec) (map en/generate) (map en/syntax-tree))))))

(deftest translation-from-target-language-spec
  (let [es-spec {:rule "s-head-last"
                 :sem {:subj {:existential? false
                              :mod []
                              :pred :Juana}
                       :mod []
                       :pred :wake-up
                       :aspect :simple
                       :tense :present}
                 :subcat [],
                 :phrasal? true,
                 :cat :verb,
                 :pronoun? nil}
        es-generated (-> es-spec es/generate)
        debug (log/debug (str "es-generated is: " (es/syntax-tree es-generated)))
        en-spec (translate/es-structure-to-en-structure es-generated)
        en-generated (-> en-spec en/generate)]
    (is (= (es/syntax-tree es-generated)
           "[s-head-last(:present-simple){+} .Juana +[vp-pronoun-c(:present-simple){+} .se(3) +despierta]]"))
    (is (= (en/syntax-tree en-generated)
           "[s(:present-simple) .Juana +[vp +wakes .up]]"))))

(defn timings []
  (let [es-spec {:head {:reflexive? false
                        :canonical "llenar"
                        :rule "vp-aux-non-reflexive"
                        :cat :verb}
                 :subcat []
                 :rule "s-head-last"
                 :phrasal? true
                 :cat :verb}
        es-generated (-> es-spec es/generate)
        en-spec (translate/es-structure-to-en-structure es-generated)
        en-generated (-> en-spec en/generate)]
    {:es (-> es-generated es/morph)
     :en (-> en-generated en/morph)}))

(defn do-timing []
  (take 10 (repeatedly #(time (println (timings))))))

(deftest direct-object-pronouns
  (let [es-spec
        {:root "ver"
         :subcat []
         :reflexive? false
         :head {:comp {:reflexive? false}}
         :sem {:subj {:pred :i}
               :tense :present
               :aspect :simple
               :obj {:pred :they
                     :gender :masc}}
         :rule "s-head-last"}]
    (is (= "[s-head-last(:present-simple) .yo +[vp-pronoun-c(:present-simple) .los +veo]]"
           (-> es-spec
               es/generate
               es/syntax-tree)))
    (is (= "[s(:present-simple) .I +[vp +see .them]]"
           (-> es-spec
               es/generate
               translate/es-structure-to-en-structure
               en/syntax-tree))))
  (doall
   (take 10
         (repeatedly #(let [es-spec
                            {:root "ver"
                             :subcat []
                             :reflexive? false
                             :head {:comp {:reflexive? false
                                           :pronoun? true}}
                             :agr {:person :1st}
                             :sem {:subj {:pred :i}
                                   :tense :present
                                   :aspect :simple}
                             :rule "s-head-last"}]
                        (let [es-generated (-> es-spec es/generate)
                              en-generated (-> es-generated translate/es-structure-to-en-structure)]
                          (log/info (str "es: " (-> es-generated es/syntax-tree)))
                          (log/info (str "en: " (-> en-generated en/syntax-tree)))
                          (is (not (nil? es-generated)))
                          (is (not (nil? en-generated)))))))))

(deftest reflexivity-tranfer
  (let [yo-me-lavo (->> "yo me lavo" es/parse (filter #(= "s-head-last" (:rule %))))
        significant-parts (->> yo-me-lavo (map #(select-keys % [:sem :reflexive?])) (map dag_unify.diagnostics/strip-refs) vec)]
    (is
     (= significant-parts
        [{:sem {:obj {:existential? false,
                      :mod (),
                      :ref {:human? true,
                            :context :informal
                            :number :sing},
                      :pred :i},
                :subj {:existential? false,
                       :mod (),
                       :ref {:human? true,
                             :context :informal                             
                             :number :sing},
                       :pred :i},
                :mod (),
                :pred :wash,
                :aspect :simple,
                :tense :present},
          :reflexive? true}])))
  (let [i-wash-myself (->> "I wash myself" en/parse (filter #(= "s" (:rule %))))
        significant-parts (->> i-wash-myself (map #(select-keys % [:sem :reflexive?])) (map dag_unify.diagnostics/strip-refs) vec)]
    (is
     (= significant-parts
        [{:sem
          {:obj
           {:obj :none,
            :existential? false,
            :mod (),
            :locative? false,
            :ref {:human? true, :number :sing}},
           :subj
           {:existential? false,
            :mod (),
            :locative? false,
            :ref {:human? true, :number :sing},
            :pred :i},
           :mod (),
           :pred :wash,
           :aspect :simple,
           :tense :present},
          :reflexive? true}]))

  (let [i-get-up (->> "I get up" en/parse (filter #(= "s" (:rule %))))
        significant-parts (->> i-get-up (map #(select-keys % [:sem :reflexive?])) (map dag_unify.diagnostics/strip-refs) vec)]
    (is
     (= significant-parts
        [{:sem
          {:obj :none,
           :iobj :none,
           :subj
           {:existential? false,
            :mod (),
            :locative? false,
            :ref {:human? true, :number :sing},
            :pred :i},
           :mod (),
           :pred :get-up,
           :aspect :simple,
           :tense :present},
          :reflexive? false}])))))

(deftest yo-he-visto
  (is (= (->> "yo he visto" es/parse (map translate/es-structure-to-en-structure) (map en/syntax-tree))
         '("[s(:perfect) .I +[vp-aux +have(2) .seen]]"))))

(deftest te-veo
  (let [te-veo (->> "te veo" es/parse (filter #(= [] (u/get-in % [:subcat]))))]
    (is (= (->> te-veo
                (map translate/es-structure-to-en-structure)
                (filter #(= :you (u/get-in % [:sem :obj :pred])))
                (map #(u/get-in % [:sem :obj]))
                (map dag_unify.diagnostics/strip-refs)
                vec)
           [{:obj :none,
             :gender :top
             :existential? false,
             :mod [],
             :locative? false,
             :ref {:a 2, :context :informal, :human? true, :number :sing}
             :pred :you}]))))

(deftest lo-veo-parsing
  (let [lo-veo (->> "lo veo"
                    es/parse
                    (filter #(= [] (u/get-in % [:subcat]))))
        i-see-you (->> lo-veo
                       (map translate/es-structure-to-en-structure)
                       (filter #(= :you (u/get-in % [:sem :obj :pred]))))
        i-see-it (->> lo-veo
                      (map translate/es-structure-to-en-structure)
                      (filter #(= :it (u/get-in % [:sem :obj :pred]))))
        i-see-him (->> lo-veo
                       (map translate/es-structure-to-en-structure)
                       (filter #(= :he (u/get-in % [:sem :obj :pred]))))]
    (is (= #{true}
           (->> i-see-you
                (map en/morph)
                (map #(clojure.string/replace % #"[A-Za-z ]+" ""))
                (map #(contains? (set menard.morphology/formal-masculine) %))
                set)))
    (is (= #{"I see you"}
           (->> i-see-you
                (map en/morph)
                (map #(clojure.string/replace % #" [^A-Za-z ]+$" ""))
                set)))
    (is (= #{"I see it"}
           (->> i-see-it
                (map en/morph)
                set)))
    (is (= #{"I see him"}
           (->> i-see-him
                (map en/morph)
                set)))))

(deftest generation-with-veo
  (is (= "te veo"
         (-> {:agr {:person :1st
                    :number :sing}
              :sem {:obj {:pred :you
                          :ref {:number :sing
                                :context :informal}}
                    :pred :see
                    :tense :present
                    :aspect :simple}
              :cat :verb
              :subcat []}
             es/generate
             es/morph))))

(deftest la-veo-parsing
  (let [la-veo (->> "la veo"
                    es/parse
                    (filter #(= [] (u/get-in % [:subcat]))))
        i-see-you (->> la-veo
                       (map translate/es-structure-to-en-structure)
                       (filter #(= :you (u/get-in % [:sem :obj :pred]))))
        i-see-it (->> la-veo
                      (map translate/es-structure-to-en-structure)
                      (filter #(= :it (u/get-in % [:sem :obj :pred]))))
        i-see-her (->> la-veo
                       (map translate/es-structure-to-en-structure)
                       (filter #(= :she (u/get-in % [:sem :obj :pred]))))]
    (is (= #{true}
           (->> i-see-you
                (map en/morph)
                (map #(clojure.string/replace % #"[A-Za-z ]+" ""))
                (map #(contains? (set menard.morphology/formal-feminine) %))
                set)))
    (is (= #{"I see you"}
           (->> i-see-you
                (map en/morph)
                (map #(clojure.string/replace % #" [^A-Za-z ]+$" "")) ;; remove emojis used to encode formality
                set)))
    (is (= #{"I see it"}
           (->> i-see-it
                (map en/morph)
                set)))
    (is (= #{"I see her"}
           (->> i-see-her
                (map en/morph)
                set)))))
