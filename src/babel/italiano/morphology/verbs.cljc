(ns babel.italiano.morphology.verbs
  (:refer-clojure :exclude [get-in])
  (:require
   #?(:clj [clojure.tools.logging :as log])
   #?(:cljs [babel.logjs :as log]) 
   [babel.morphology :as morph :refer [do-replace-on]]
   [clojure.string :as string]
   [dag_unify.core :refer [get-in unify]]
   #?(:clj [clojure.tools.logging :as log])
   #?(:cljs [babel.logjs :as log])))

(defn compile-patterns [unify-with patterns]
  (->> patterns
       (morph/compile-patterns unify-with)
       (map (fn [pattern]
              (merge pattern
                     {:boot-verb 
                      (:boot-verb pattern :top)})))))

(defn patterns-with-agr [patterns]
  (map (fn [pattern]
         (let [{agr :agr} pattern]
           (merge pattern
                  {:agr agr})))
       patterns))

(defn patterns-with-essere [patterns]
  (map (fn [pattern]
         (merge pattern
                {:essere (get-in pattern [:synsem :essere] :top)}))
       patterns))

;; <conditional>
(let [patterns
      (-> (clojure.java.io/resource "babel/italiano/morphology/verbs/conditional.edn")
          slurp
          read-string
          patterns-with-agr)]
  (defonce patterns-conditional
    (compile-patterns
     {:synsem {:infl :conditional}}
     patterns)))

(defn regular-conditional? [word]
  (and (= (get-in word [:infl]) :conditional)
       (get-in word [:italiano])))

(defn regular-conditional [word]
  (let [unifying-patterns
        (remove nil? (mapcat #(if (not (= :fail (unify (:u %) word)))
                                (:g %))
                             patterns-conditional))
        infinitive (get-in word [:italiano])]
    (first (do-replace-on infinitive unifying-patterns))))

(defn regular-conditional-with-future-stem? [word]
  ;; regular inflection of conditional with :future-stem
  (and (= (get-in word [:infl]) :conditional)
       (string? (get-in word '(:future-stem) :none))))

(defn regular-conditional-with-future-stem [word]
  (let [stem (get-in word '(:future-stem))
        person (get-in word '(:agr :person))
        number (get-in word '(:agr :number))
        drop-e (get-in word '(:italiano :drop-e) false)]
    (cond
      (and (= person :1st) (= number :sing))
      (str stem "ei")
      
      (and (= person :2nd) (= number :sing))
      (str stem "esti")
      
      (and (= person :3rd) (= number :sing))
      (str stem "ebbe")
      
      (and (= person :1st) (= number :plur))
      (str stem "emmo")
      
      (and (= person :2nd) (= number :plur))
      (str stem "este")
      
      (and (= person :3rd) (= number :plur))
      (str stem "ebbero"))))

(defn regular-conditional [word]
  (let [unifying-patterns
        (mapcat #(when (not (= :fail (unify word
                                            {:agr (:agr %)})))
                   (:g %))
                patterns-conditional)
        infinitive (get-in word [:italiano])]
    (let [conjugations (do-replace-on infinitive unifying-patterns)]
      (if (empty? conjugations)
        (throw (Exception. (str "no conjugation found for infinitive: " infinitive))))
      (if (not (empty? (rest conjugations)))
        (throw (Exception. (str "more than one conjugation found for infinitive:'" infinitive "': "
                                (string/join "," (vec (set conjugations)))
                                "; word: " (dag_unify.core/strip-refs word)
                                "; unifying-patterns: " (string/join "," (map dag_unify.core/strip-refs
                                                                              unifying-patterns))))))
      (first conjugations))))

;; </conditional>

;; <future>
(let [source
      (-> (clojure.java.io/resource "babel/italiano/morphology/verbs/future.edn")
          slurp
          read-string
          patterns-with-agr)]
  (defonce patterns-future
    (compile-patterns
     {:synsem {:infl :future}}
     source)))

(defn regular-future? [word]
  (and (= (get-in word [:infl]) :future)
       (get-in word [:italiano])))

(defn regular-future [word]
  (let [unifying-patterns
        (remove nil? (mapcat #(when (not (= :fail
                                            (unify word
                                                   {:agr (:agr %)})))
                                (:g %))
                             patterns-future))
        infinitive (get-in word [:italiano])]
    (first (do-replace-on infinitive unifying-patterns))))
;; </future>

;; <imperfect>
(let [source
      (-> (clojure.java.io/resource "babel/italiano/morphology/verbs/imperfetto.edn")
          slurp
          read-string
          patterns-with-agr)]
  (defonce patterns-imperfect
    (compile-patterns
     {:synsem {:infl :imperfect}}
     source)))

(defn regular-imperfect? [word]
  ;; regular imperfect sense
  (and (= (get-in word '(:infl)) :imperfect)
       (get-in word '(:italiano))))

(defn regular-imperfect [word]
  (let [unifying-patterns
        (remove nil? (mapcat #(when (not (= :fail
                                            (unify word
                                                   {:agr (:agr %)})))
                                (:g %))
                             patterns-imperfect))
        infinitive (get-in word [:italiano])]
    (first (do-replace-on infinitive unifying-patterns))))
;; </imperfect>

;; <present>
(let [source
      (-> (clojure.java.io/resource "babel/italiano/morphology/verbs/present.edn")
          slurp
          read-string
          patterns-with-agr)]
  (defonce patterns-present
    (compile-patterns
     {:synsem {:infl :present}}
     source)))
;; </present>

;; <passato>
(let [source
      (-> (clojure.java.io/resource "babel/italiano/morphology/verbs/passato.edn")
          slurp
          read-string
          patterns-with-agr
          patterns-with-essere)]
  (defonce patterns-passato
    (compile-patterns
     {:synsem {:infl :past}}
     source)))

(defn regular-passato [word]
  (let [unifying-patterns
        (remove nil? (mapcat #(when (not (= :fail
                                            (unify word
                                                   {:agr (:agr %)
                                                    :essere (:essere %)})))
                                (:g %))
                             patterns-passato))
        infinitive (get-in word [:italiano])]
    (first (do-replace-on infinitive unifying-patterns))))

;; </passato>

;; <subjunctive>
(let [source
      (-> (clojure.java.io/resource "babel/italiano/morphology/verbs/subjunctive.edn")
          slurp
          read-string
          patterns-with-agr)]
  (def patterns-subjunctive
    (compile-patterns
     {:synsem {:infl :past}}
     source)))
;; </subjunctive>

(defonce patterns-gerund
 (compile-patterns
  {:synsem {:infl :participle}}
  [{:agr :top
    :p [#"(.*)ando$" "$1are"
        #"(.*)ando$" "$1arsi"]}]))

(defonce patterns
  (map (fn [each]
         ;; unify with {:synsem {:cat :verb}} for all rules:
         {:p (:p each)
          :u (unify (:u each)
                    {:synsem {:cat :verb}})
          :g (:g each)})
       
       (concat
        ;; if more are added in the future, please
        ;; preserve alphabetical ordering.
        patterns-conditional
        patterns-future
        patterns-gerund
        patterns-imperfect
        patterns-passato
        patterns-present
        patterns-subjunctive)))

(defonce exceptions-rules
  [;; 1. passato exceptions
   {:path [:italiano :passato]
    :label "passato exception"
    :merge-fn
    (fn [val]
      {:italiano {:infl :past
                  :italiano (get-in val [:italiano :passato] :nothing)}})}
   
   ;; 1.5 imperfect
   {:path [:italiano :imperfect :1sing]
    :label "imperfect 1sing"
    :merge-fn
    (fn [val]
      {:italiano {:infl :imperfect
                  :italiano (get-in val [:italiano :imperfect :1sing] :nothing)
                  :agr {:number :sing
                        :person :1st}}})}
   {:path [:italiano :imperfect :2sing]
    :label "imperfect 2sing"
    :merge-fn
    (fn [val]
      {:italiano {:infl :imperfect
                  :italiano (get-in val [:italiano :imperfect :2sing] :nothing)
                  :agr {:number :sing
                        :person :2nd}}})}
   
   {:path [:italiano :imperfect :3sing]
    :label "imperfect 3sing"
    :merge-fn
    (fn [val]
      {:italiano {:infl :imperfect
                  :italiano (get-in val [:italiano :imperfect :3sing] :nothing)
                  :agr {:number :sing
                        :person :3rd}}})}
   
   {:path [:italiano :imperfect :1plur]
    :label "imperfect 1plur"
    :merge-fn
    (fn [val]
      {:italiano {:infl :imperfect
                  :italiano (get-in val [:italiano :imperfect :1plur] :nothing)
                  :agr {:number :plur
                        :person :1st}}})}
   
   {:path [:italiano :imperfect :2plur]
    :label "imperfect 2plur"
    :merge-fn
    (fn [val]
      {:italiano {:infl :imperfect
                  :italiano (get-in val [:italiano :imperfect :2plur] :nothing)
                  :agr {:number :plur
                        :person :2nd}}})}
   
   {:path [:italiano :imperfect :3plur]
    :label "imperfect 3plur"
    :merge-fn
    (fn [val]
      {:italiano {:infl :imperfect
                  :italiano (get-in val [:italiano :imperfect :3plur] :nothing)
                  :agr {:number :plur
                        :person :3rd}}})}
   
   ;; 2. present-tense exceptions
   {:path [:italiano :present :1sing]
    :label "present 1sing"
    :merge-fn
    (fn [val]
      {:italiano {:infl :present
                  :italiano (get-in val [:italiano :present :1sing] :nothing)
                  :agr {:number :sing
                        :person :1st}}})}
   {:path [:italiano :present :2sing]
    :label "present 2sing"
    :merge-fn
    (fn [val]
      {:italiano {:infl :present
                  :italiano (get-in val [:italiano :present :2sing] :nothing)
                  :agr {:number :sing
                        :person :2nd}}})}
   
   {:path [:italiano :present :3sing]
    :label "present 3sing"
    :merge-fn
    (fn [val]
      {:italiano {:infl :present
                  :italiano (get-in val [:italiano :present :3sing] :nothing)
                  :agr {:number :sing
                        :person :3rd}}})}
   
   {:path [:italiano :present :1plur]
    :label "present 1plur"
    :merge-fn
    (fn [val]
      {:italiano {:infl :present
                  :italiano (get-in val [:italiano :present :1plur] :nothing)
                  :agr {:number :plur
                        :person :1st}}})}
   
   {:path [:italiano :present :2plur]
    :label "present 2plur"
    :merge-fn
    (fn [val]
      {:italiano {:infl :present
                  :italiano (get-in val [:italiano :present :2plur] :nothing)
                  :agr {:number :plur
                        :person :2nd}}})}
   
   {:path [:italiano :present :3plur]
    :label "present 3plur"
    :merge-fn
    (fn [val]
      {:italiano {:infl :present
                  :italiano (get-in val [:italiano :present :3plur] :nothing)
                  :agr {:number :plur
                        :person :3rd}}})}
   
   ;; 3. future-tense exceptions
   {:path [:italiano :future :1sing]
    :label "future 1sing"
    :merge-fn
    (fn [val]
      {:italiano {:infl :future
                  :italiano (get-in val [:italiano :future :1sing] :nothing)
                  :agr {:number :sing
                        :person :1st}}})}
   {:path [:italiano :future :2sing]
    :label "future 2sing"
    :merge-fn
    (fn [val]
      {:italiano {:infl :future
                  :italiano (get-in val [:italiano :future :2sing] :nothing)
                  :agr {:number :sing
                        :person :2nd}}})}
   {:path [:italiano :future :3sing]
    :label "future 3sing"
    :merge-fn
    (fn [val]
      {:italiano {:infl :future
                  :italiano (get-in val [:italiano :future :3sing] :nothing)
                                                   :agr {:number :sing
                                                         :person :3rd}}})}
                                    {:path [:italiano :future :1plur]
                                     :label "future 1plur"
                                     :merge-fn
                                     (fn [val]
                                       {:italiano {:infl :future
                                                   :italiano (get-in val [:italiano :future :1plur] :nothing)
                                                   :agr {:number :plur
                                                         :person :1st}}})}
                                    {:path [:italiano :future :2plur]
                                     :label "future 2plur"
                                     :merge-fn
                                     (fn [val]
                                       {:italiano {:infl :future
                                                   :italiano (get-in val [:italiano :future :2plur] :nothing)
                                                   :agr {:number :plur
                                                         :person :2nd}}})}
                                    {:path [:italiano :future :3plur]
                                     :label "future 3plur"
                                     :merge-fn
                                     (fn [val]
                                       {:italiano {:infl :future
                                                   :italiano (get-in val [:italiano :future :3plur] :nothing)
                                                   :agr {:number :plur
                                                         :person :3rd}}})}
                                    
   ;; 4. conditional-tense exceptions
   {:path [:italiano :conditional :1sing]
    :label "conditional 1sing"
    :merge-fn
    (fn [val]
      {:italiano {:infl :conditional
                  :italiano (get-in val [:italiano :conditional :1sing] :nothing)
                  :agr {:number :sing
                        :person :1st}}})}
   {:path [:italiano :conditional :2sing]
    :label "conditional 2sing"
    :merge-fn
    (fn [val]
      {:italiano {:infl :conditional
                  :italiano (get-in val [:italiano :conditional :2sing] :nothing)
                  :agr {:number :sing
                        :person :2nd}}})}
   {:path [:italiano :conditional :3sing]
    :label "conditional 3sing"
    :merge-fn
    (fn [val]
      {:italiano {:infl :conditional
                  :italiano (get-in val [:italiano :conditional :3sing] :nothing)
                  :agr {:number :sing
                        :person :3rd}}})}
   {:path [:italiano :conditional :1plur]
    :label "conditional 1plur"
    :merge-fn
    (fn [val]
      {:italiano {:infl :conditional
                  :italiano (get-in val [:italiano :conditional :1plur] :nothing)
                  :agr {:number :plur
                        :person :1st}}})}
   {:path [:italiano :conditional :2plur]
    :label "conditional 2plur"
    :merge-fn
    (fn [val]
      {:italiano {:infl :conditional
                  :italiano (get-in val [:italiano :conditional :2plur] :nothing)
                  :agr {:number :plur
                        :person :2nd}}})}
   {:path [:italiano :conditional :3plur]
    :label "conditional 3plur"
    :merge-fn
    (fn [val]
      {:italiano {:infl :conditional
                  :italiano (get-in val [:italiano :conditional :3plur] :nothing)
                  :agr {:number :plur
                        :person :3rd}}})}])

;; TODO: remove (defn stem-analysis) once gerund rules are converted to .edn:
;; gerunds are the only place where stem-analysis is used.
(defn stem-analysis [word]
  (let [infinitive (if (get-in word [:infinitive]) ;; regular present tense
                     (get-in word [:infinitive])
                     (get-in word [:italiano]))
        ;; e.g.: lavarsi => lavare
        infinitive (if (re-find #"[aei]rsi$" infinitive)
                     (string/replace infinitive #"si$" "e")
                     infinitive)
        are-type (try (re-find #"are$" infinitive)
                      (catch Exception e
                        (throw (Exception. (str "Can't regex-find on non-string: " infinitive " from word: " word)))))
        ere-type (re-find #"ere$" infinitive)
        ire-type (re-find #"ire$" infinitive)
        boot-stem (cond (and (= true (get-in word [:boot-verb]))
                             (or (= (get-in word [:agr :number])
                                    :sing)
                                 (and (= (get-in word [:agr :person])
                                         :3rd)
                                      (= (get-in word [:agr :number])
                                         :plur))))
                        (get-in word [:boot-verb])
                        true
                        (string/replace infinitive #"[iae]re$" ""))
        
        ;; stem is stem without regard to :boot-stem-g. It is
        ;; used for gerunds, e.g.: fornire -> 'io fornisco'
        ;; but 'io fornendo', not 'io forniscendo'.
        stem (string/replace infinitive #"[iae]re$" "")

        last-stem-char-is-i (re-find #"i[iae]re$" infinitive)
        last-stem-char-is-e (re-find #"e[iae]re$" infinitive)
        is-care-or-gare? (re-find #"[cg]are$" infinitive)
        person (get-in word '(:agr :person))
        number (get-in word '(:agr :number))]
    {:infinitive infinitive
     :are-type are-type
     :ere-type ere-type
     :ire-type ire-type
     :boot-stem boot-stem
     :stem stem
     :last-stem-char-is-i last-stem-char-is-i
     :last-stem-char-is-e last-stem-char-is-e
     :is-care-or-gare? is-care-or-gare?
     :person person
     :number number}))

(defn suffix-of [word]
  "compute the final character given a lexical entry and agreement info in :agr."
  (let [suffix (cond
                 
                 (and (= (get-in word '(:obj-agr :gender)) :fem)
                      (= (get-in word '(:obj-agr :number)) :sing))
                 "a"
                 
                 (and (= (get-in word '(:obj-agr :gender)) :fem)
                      (= (get-in word '(:obj-agr :number)) :plur))
                 "e"

                 (= (get-in word '(:obj-agr :number)) :plur)
                 "i"
                 
                 (and (= (get-in word '(:agr :gender)) :fem)
                      (= (get-in word '(:agr :number)) :sing)
                      (= (get-in word '(:essere)) true))
                 "a"
                 
                 (and (= (get-in word '(:agr :gender)) :fem)
                      (= (get-in word '(:agr :number)) :plur)
                      (= (get-in word '(:essere)) true))
                 "e"
                 
                 (and (= (get-in word '(:agr :number)) :plur)
                      (= (get-in word '(:essere)) true))
                 "i"
                 
                 true
                 "o"
                 
                 )]
    suffix))

(defn irregular-future? [word]
  (and
   (= (get-in word '(:infl)) :future)
   (map? (get-in word '(:future)))))

(defn irregular-future [word]
  (let [infinitive (get-in word '(:italiano)) ;; future irregular
        ;; e.g.: lavarsi => lavare
        infinitive (if (re-find #"[aei]rsi$" infinitive)
                     (string/replace infinitive #"si$" "e")
                     infinitive)
        person (get-in word '(:agr :person))
        number (get-in word '(:agr :number))]
    (cond
      (and (= person :1st) (= number :sing))
      (get-in word '(:future :1sing))
      (and (= person :2nd) (= number :sing))
      (get-in word '(:future :2sing))
      (and (= person :3rd) (= number :sing))
      (get-in word '(:future :3sing))
      (and (= person :1st) (= number :plur))
      (get-in word '(:future :1plur))
      (and (= person :2nd) (= number :plur))
      (get-in word '(:future :2plur))
      (and (= person :3rd) (= number :plur))
      (get-in word '(:future :3plur))
      
      (and (= (get-in word '(:infl)) :future)
           (string? (get-in word '(:italiano))))
      (str (get-in word '(:italiano)) " (future)")
      
      true ;; failthrough: should usually not get here:
      ;; TODO: describe when it might be ok, i.e. why log/warn not log/error.
      (do (log/warn (str "get-string-1 could not match: " word))
          word))))

;; future: 2) future-stem specified
(defn regular-future-with-future-stem? [word]
  (and (= (get-in word '(:infl)) :future)
       (get-in word '(:future-stem))))

(defn regular-future-with-future-stem [word]
  (let [stem (get-in word '(:future-stem))
        number (get-in word [:agr :number])
        person (get-in word [:agr :person])
        patterns patterns-future]
    (-> patterns
        (map (fn [pattern]
               (let [[from to] (nth (:g pattern))]
                 from)))
        (remove nil?))
    (cond
      (and (= person :1st) (= number :sing))
      (str stem "ò")
      
      (and (= person :2nd) (= number :sing))
      (str stem "ai")
      
      (and (= person :3rd) (= number :sing))
      (str stem "à")
      
      (and (= person :1st) (= number :plur))
      (str stem "emo")
      
      (and (= person :2nd) (= number :plur))
      (str stem "ete")
      
      (and (= person :3rd) (= number :plur))
      (str stem "anno"))))

(defn irregular-imperfect-1sing? [word]
  (and
   (= (get-in word '(:infl)) :imperfect)
   (= :sing (get-in word '(:agr :number)))
   (= :1st (get-in word '(:agr :person)))
   (string? (get-in word '(:imperfect :1sing)))))

(defn irregular-imperfect-1sing [word]
  (get-in word [:imperfect :1sing]))

(defn irregular-imperfect-2sing? [word]
  (and
   (= (get-in word '(:infl)) :imperfect)
   (= :sing (get-in word '(:agr :number)))
   (= :2nd (get-in word '(:agr :person)))
   (string? (get-in word '(:imperfect :2sing)))))

(defn irregular-imperfect-2sing [word]
  (get-in word [:imperfect :2sing]))

(defn irregular-imperfect-3sing? [word]
  (and
   (= (get-in word '(:infl)) :imperfect)
   (= :sing (get-in word '(:agr :number)))
   (= :3rd (get-in word '(:agr :person)))
   (string? (get-in word '(:imperfect :3sing)))))

(defn irregular-imperfect-3sing [word]
  (get-in word [:imperfect :3sing]))

(defn irregular-imperfect-1plur? [word]
  (and
   (= (get-in word '(:infl)) :imperfect)
   (= :plur (get-in word '(:agr :number)))
   (= :1st (get-in word '(:agr :person)))
   (string? (get-in word '(:imperfect :1plur)))))

(defn irregular-imperfect-1plur [word]
  (get-in word [:imperfect :1plur]))

(defn irregular-imperfect-2plur? [word]
  (and
   (= (get-in word '(:infl)) :imperfect)
   (= :plur (get-in word '(:agr :number)))
   (= :2nd (get-in word '(:agr :person)))
   (string? (get-in word '(:imperfect :2plur)))))

(defn irregular-imperfect-2plur [word]
  (get-in word [:imperfect :2plur]))

(defn irregular-imperfect-3plur? [word]
  (and
   (= (get-in word '(:infl)) :imperfect)
   (= :plur (get-in word '(:agr :number)))
   (= :3rd (get-in word '(:agr :person)))
   (string? (get-in word '(:imperfect :3plur)))))

(defn irregular-imperfect-3plur [word]
  (get-in word [:imperfect :3plur]))

(defn passato-as-head? [word]
  ;; "fare [past]" + "bene" => "fatto bene"
  (and (= (get-in word '(:cat)) :verb)
       (= (get-in word '(:infl)) :past)
       (string? (get-in word '(:a :passato)))))

(defn passato-as-head [word]
  (str (get-in word '(:a :passato)) " "
       (get-in word '(:b))))

(defn irregular-passato? [word]
  ;; TODO: do not use brackets: if there's an error about there being
  ;; not enough information, throw an exception explicitly.
  ;; return the irregular form in square brackets, indicating that there's
  ;; not enough information to conjugate the verb.
  (and (= :past (get-in word '(:infl)))
       (get-in word '(:passato))
       (get-in word '(:essere) true)
       (or (= :notfound (get-in word '(:agr :number) :notfound))
           (= :top (get-in word '(:agr :number))))))

(defn irregular-passato [word]
  ;; not enough information.
  (log/warn (str "not enough agreement specified to conjugate: " (get-in word '(:passato)) " (irreg past)]"))
  (get-in word '(:passato)))

(defn passato-stem? [word]
  (and (= :past (get-in word '(:infl)))
       (get-in word '(:passato-stem))))

(defn passato-stem [word]
  ;; conjugate irregular passato: option 1) using :passato-stem
  (let [irregular-passato (get-in word '(:passato-stem))]
    (str irregular-passato (suffix-of word))))

(defn irregular-passato? [word]
  ;; conjugate irregular passato: option 2) using :passato
  (and (= :past (get-in word '(:infl)))
       (get-in word '(:passato))))

(defn irregular-passato [word]
  (let [irregular-passato (get-in word '(:passato))
        butlast (nth (re-find #"(.*).$" irregular-passato) 1)]
    (str butlast (suffix-of word))))

(defn regular-passato? [word]
  (and (= :past (get-in word '(:infl)))
       (string? (get-in word '(:italiano)))))

(defn irregular-present-1sing? [word]
  ;; <irregular present tense>
  (let [person (get-in word '(:agr :person))
        number (get-in word '(:agr :number))]
    (and (= (get-in word '(:infl)) :present)
         (= person :1st) (= number :sing)
         (string? (get-in word '(:present :1sing))))))

(defn irregular-present-1sing [word]
  (get-in word '(:present :1sing)))

(defn irregular-present-2sing? [word]
  ;; <irregular present tense>
  (let [person (get-in word '(:agr :person))
        number (get-in word '(:agr :number))]
    (and (= (get-in word '(:infl)) :present)
         (= person :2nd) (= number :sing)
         (string? (get-in word '(:present :2sing))))))

(defn irregular-present-2sing [word]
  (get-in word '(:present :2sing)))

(defn irregular-present-3sing? [word]
  ;; <irregular present tense>
  (let [person (get-in word '(:agr :person))
        number (get-in word '(:agr :number))]
    (and (= (get-in word '(:infl)) :present)
         (= person :3rd) (= number :sing)
         (string? (get-in word '(:present :3sing))))))

(defn irregular-present-3sing [word]
  (get-in word '(:present :3sing)))

(defn irregular-present-1plur? [word]
  ;; <irregular present tense>
  (let [person (get-in word '(:agr :person))
        number (get-in word '(:agr :number))]
    (and (= (get-in word '(:infl)) :present)
         (= person :1st) (= number :plur)
         (string? (get-in word '(:present :1plur))))))

(defn irregular-present-1plur [word]
  (get-in word '(:present :1plur)))

(defn irregular-present-2plur? [word]
  ;; <irregular present tense>
  (let [person (get-in word '(:agr :person))
        number (get-in word '(:agr :number))]
    (and (= (get-in word '(:infl)) :present)
         (= person :2nd) (= number :plur)
         (string? (get-in word '(:present :2plur))))))

(defn irregular-present-2plur [word]
  (get-in word '(:present :2plur)))

(defn irregular-present-3plur? [word]
  ;; <irregular present tense>
  (let [person (get-in word '(:agr :person))
        number (get-in word '(:agr :number))]
    (and (= (get-in word '(:infl)) :present)
         (= person :3rd) (= number :plur)
         (string? (get-in word '(:present :3plur))))))

(defn irregular-present-3plur [word]
  (get-in word '(:present :3plur)))

(defn regular-present? [word]
  (and
   (= (get-in word '(:infl)) :present)
   (string? (get-in word '(:italiano)))))

(defn regular-present [word]
  (log/debug (str "(regular-present " (dag_unify.core/strip-refs word) ")"))
  (let [unifying-patterns
        (remove nil? (mapcat #(when
                                  (not (= :fail
                                          (unify word
                                                 {:agr (:agr %)
                                                  :boot-verb (:boot-verb % :top)})))
                                (:g %))
                             patterns-present))
        infinitive (get-in word [:italiano])]
    (first (do-replace-on infinitive unifying-patterns))))

;; <irregular gerund inflection>
(defn irregular-gerund? [word]
  (and
   (= (get-in word [:infl]) :participle)
   (string? (get-in word [:italiano]))
   (string? (get-in word [:gerund]))))

(defn irregular-gerund [word]
  (get-in word [:gerund]))
;; </irregular gerund inflection>

;; <default gerund inflection>
(defn regular-gerund? [word]
  (and
   (= (get-in word [:infl]) :participle)
   (string? (get-in word [:italiano]))))

(defn regular-gerund [word]
  (let [stem-analysis (stem-analysis word)
        infinitive (:infinitive stem-analysis)
        stem (:stem stem-analysis)]
    (log/debug (str "cnonjugating present participle; analysis:" stem-analysis))
    (cond (= "are" (:are-type stem-analysis))
          (str stem "ando")
          (= "ere" (:ere-type stem-analysis))
          (str stem "endo")
          (= "ire" (:ire-type stem-analysis))
          (str stem "endo")
          true
          (do
            (log/warn (str "no specific conjugation found for word with stem-analysis:" stem-analysis " - returning infinitive"))
            infinitive))))
;; </default gerund inflection>

(defn conjugate [word]
  (cond
    (irregular-future? word)
    (irregular-future word)
    
    (regular-future-with-future-stem? word)
    (regular-future-with-future-stem word)
    
    (regular-future? word)
    (regular-future word)
    
    (regular-conditional-with-future-stem? word)
    (regular-conditional-with-future-stem word)
    
    (regular-conditional? word)
    (regular-conditional word)
    
    (irregular-imperfect-1sing? word)
    (irregular-imperfect-1sing word)
    
    (irregular-imperfect-2sing? word)
    (irregular-imperfect-2sing word)

    (irregular-imperfect-3sing? word)
    (irregular-imperfect-3sing word)
    
    (irregular-imperfect-1plur? word)
    (irregular-imperfect-1plur word)
    
    (irregular-imperfect-2plur? word)
    (irregular-imperfect-2plur word)
    
    (irregular-imperfect-3plur? word)
    (irregular-imperfect-3plur word)
    
    (regular-imperfect? word)
    (regular-imperfect word)
    
    (passato-as-head? word)
    (passato-as-head word)
    
    (irregular-passato? word)
    (irregular-passato word)
    
    (passato-stem? word)
    (passato-stem word)
    
    (irregular-passato? word)
    (irregular-passato word)
    
    (regular-passato? word)
    (regular-passato word)
    
    (irregular-present-1sing? word)
    (irregular-present-1sing word)
    
    (irregular-present-2sing? word)
    (irregular-present-2sing word)
    
    (irregular-present-3sing? word)
    (irregular-present-3sing word)
    
    (irregular-present-1plur? word)
    (irregular-present-1plur word)
    
    (irregular-present-2plur? word)
    (irregular-present-2plur word)
    
    (irregular-present-3plur? word)
    (irregular-present-3plur word)
    
    (regular-present? word)
    (regular-present word)
    
    (irregular-gerund? word)
    (irregular-gerund word)
    
    (regular-gerund? word)
    (regular-gerund word)

    ))

