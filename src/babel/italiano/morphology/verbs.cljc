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

(defn regular-conditional [word]
  (let [unifying-patterns
        (mapcat #(when (not (= :fail (unify word
                                            {:agr (:agr %)})))
                   (:g %))
                patterns-conditional)
        ;; if the word has a future-stem, use it; otherwise, use :italiano, which
        ;; is expected to be the infinitive for verbs.
        infinitive (get-in word [:future-stem]
                           (get-in word [:italiano]))]
    (let [conjugations (do-replace-on infinitive unifying-patterns)]
      (if (empty? conjugations)
        (throw (Exception. (str "no conjugation found for infinitive: " infinitive " with unifying patterns: "
                                (string/join "," unifying-patterns)))))
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

(defn regular-future [word]
  (let [unifying-patterns
        (remove nil? (mapcat #(when (not (= :fail
                                            (unify word
                                                   {:agr (:agr %)})))
                                (:g %))
                             patterns-future))
        ;; if the word has a future-stem, use it; otherwise, use :italiano, which
        ;; is expected to be the infinitive for verbs.
        infinitive (get-in word [:future-stem]
                           (get-in word [:italiano]))]
    (log/debug (str "infinitive: " infinitive "; unifying patterns: " (string/join "," unifying-patterns)))
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
  (defonce patterns-subjunctive
    (compile-patterns
     {:synsem {:infl :past}}
     source)))
;; </subjunctive>

;; <gerund>
(let [source
      (-> (clojure.java.io/resource "babel/italiano/morphology/verbs/gerund.edn")
          slurp
          read-string)]
  (defonce patterns-gerund
    (compile-patterns
     {:synsem {:infl :participle}}
     source)))
;; </gerund>

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

(defn irregular [word exception-map]
  (let [person (get-in word [:agr :person])
        number (get-in word [:agr :number])]
    (cond
      (and (= person :1st) (= number :sing)
           (get-in exception-map [:1sing]))
      (get-in exception-map [:1sing])
      (and (= person :2nd) (= number :sing)
           (get-in exception-map [:2sing]))
      (get-in exception-map [:2sing])
      (and (= person :3rd) (= number :sing)
           (get-in exception-map [:3sing]))
      (get-in exception-map [:3sing])
      (and (= person :1st) (= number :plur)
           (get-in exception-map [:1plur]))
      (get-in exception-map [:1plur])
      (and (= person :2nd) (= number :plur)
           (get-in exception-map [:2plur]))
      (get-in exception-map [:2plur])
      (and (= person :3rd) (= number :plur)
           (get-in exception-map [:3plur]))
      (get-in exception-map [:3plur])
      
      true nil)))

(defn passato-as-head? [word]
  ;; "fare [past]" + "bene" => "fatto bene"
  (and (= (get-in word '(:cat)) :verb)
       (= (get-in word '(:infl)) :past)
       (string? (get-in word '(:a :passato)))))

(defn passato-as-head [word]
  (str (get-in word '(:a :passato)) " "
       (get-in word '(:b))))

(defn irregular-passato? [word]
  (and (= :past (get-in word '(:infl)))
       (get-in word '(:passato))
       (get-in word '(:essere) true)
       (or (= :notfound (get-in word '(:agr :number) :notfound))
           (= :top (get-in word '(:agr :number))))))

(defn irregular-passato [word]
  ;; not enough information.
  (log/warn (str "not enough agreement specified to conjugate: " (get-in word '(:passato)) " (irreg past)]"))
  (get-in word '(:passato)))

(defn irregular-passato? [word]
  ;; conjugate irregular passato: option 2) using :passato
  (and (= :past (get-in word '(:infl)))
       (get-in word '(:passato))))

(defn suffix-of [word]
  "compute the final character given a lexical entry and agreement info in :agr."
  ;; TODO: convert to .edn
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

(defn irregular-passato [word]
  (let [irregular-passato (get-in word '(:passato))
        butlast (nth (re-find #"(.*).$" irregular-passato) 1)]
    (str butlast (suffix-of word))))

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

(defn regular-gerund [word]
  (let [unifying-patterns (mapcat :g
                                  patterns-gerund)
        infinitive (get-in word [:italiano])]
    (first (do-replace-on infinitive unifying-patterns))))

(defn conjugate [word]
  (cond
    (and
     (= (get-in word [:infl]) :future)
     (map? (get-in word [:future])))
    (or (irregular word (get-in word [:future]))
        (regular-future word))
    
    (= (get-in word [:infl]) :future)
    (regular-future word)
    
    (= (get-in word [:infl]) :conditional)
    (regular-conditional word)

    (and
     (= (get-in word [:infl]) :imperfect)
     (map? (get-in word [:imperfect])))
    (or (irregular word (get-in word [:imperfect]))
        (regular-imperfect word))

    (= (get-in word [:infl]) :imperfect)
    (regular-imperfect word)
    
    (passato-as-head? word)
    (passato-as-head word)
    
    (irregular-passato? word)
    (irregular-passato word)
    
    (= :past (get-in word [:infl]))
    (regular-passato word)
    
    (and
     (= (get-in word [:infl]) :present)
     (map? (get-in word [:present])))
    (or (irregular word (get-in word [:present]))
        (regular-present word))

    (= (get-in word [:infl]) :present)
    (regular-present word)
    
    (irregular-gerund? word)
    (irregular-gerund word)
    
    (= (get-in word [:infl]) :participle)
    (regular-gerund word)
    ))


