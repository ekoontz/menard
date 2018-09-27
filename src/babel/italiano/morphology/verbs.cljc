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
         (let [agr (get-in pattern [:agr] :top)]
           (merge pattern
                  {:agr agr})))
       patterns))

(defn patterns-with-essere [patterns]
  (map (fn [pattern]
         (merge pattern
                {:essere (get-in pattern [:synsem :essere] :top)}))
       patterns))

(defn compile-verb-morphology []
  (let [tenses-map
        {"conditional" {:synsem {:infl :conditional}}
         "future"      {:synsem {:infl :future}}
         "imperfetto"  {:synsem {:infl :imperfetto}}
         "present"     {:synsem {:infl :present}}
         "subjunctive" {:synsem {:infl :subjunctive}}
         "gerund"      {:synsem {:infl :gerund}}
         "passato"     {:synsem {:infl :passato}}}]

    (reduce
     merge
     (map (fn [tense]
            (let [patterns
                  (-> (str "babel/italiano/morphology/verbs/" tense ".edn")
                      clojure.java.io/resource
                      slurp
                      read-string
                      patterns-with-agr)]
              {tense
               (compile-patterns              
                (get tenses-map tense)
                patterns)}))
          (keys tenses-map)))))

(def verb-morph-map (compile-verb-morphology))

(defonce patterns-conditional
  (get verb-morph-map "conditional"))
(defonce patterns-future
  (get verb-morph-map "future"))
(defonce patterns-gerund
  (get verb-morph-map "gerund"))
(defonce patterns-imperfetto
  (get verb-morph-map "imperfetto"))
(defonce patterns-present
  (get verb-morph-map "present"))
(defonce patterns-passato
  (get verb-morph-map "passato"))
(defonce patterns-subjunctive
  (get verb-morph-map "subjunctive"))

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
        patterns-imperfetto
        patterns-passato
        patterns-present)))

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
    (first (do-replace-on infinitive unifying-patterns))))

(defn regular-future [word]
  (let [infinitive (get-in word [:future-stem]
                           (get-in word [:italiano]))
        unifying-patterns
        (remove nil? (mapcat #(when (not (= :fail
                                            (unify word
                                                   {:agr (:agr %)})))
                                (:g %))
                             patterns-future))]
      ;; if the word has a future-stem, use it; otherwise, use :italiano, which
      ;; is expected to be the infinitive for verbs.
      (log/debug (str "infinitive: " infinitive "; unifying patterns: " (string/join "," unifying-patterns)))
      (first (do-replace-on infinitive unifying-patterns))))

(defn regular-imperfetto [word]
  (let [unifying-patterns
        (remove nil? (mapcat #(when (not (= :fail
                                            (unify word
                                                   {:agr (:agr %)})))
                                (:g %))
                             patterns-imperfetto))
        infinitive (get-in word [:italiano])]
    (first (do-replace-on infinitive unifying-patterns))))

(defn regular-passato [word]
  (let [unifying-patterns
        (remove nil? (mapcat
                      #(when (not (= :fail
                                     (unify word
                                            {:agr (:agr % :top)
                                             :essere (get-in % [:u :synsem :essere] :top)})))
                                (:g %))
                             patterns-passato))
        infinitive (get-in word [:italiano])]
    (first (do-replace-on infinitive unifying-patterns))))

(defonce exceptions-rules
  [;; 1. passato exceptions
   {:path [:italiano :passato]
    :label "passato exception"
    :merge-fn
    (fn [val]
      {:italiano {:infl :passato
                  :italiano (get-in val [:italiano :passato] :nothing)}})}
   
   ;; 1.5 imperfetto
   {:path [:italiano :imperfetto :1sing]
    :label "imperfetto 1sing"
    :merge-fn
    (fn [val]
      {:italiano {:infl :imperfetto
                  :italiano (get-in val [:italiano :imperfetto :1sing] :nothing)
                  :agr {:number :sing
                        :person :1st}}})}
   {:path [:italiano :imperfetto :2sing]
    :label "imperfetto 2sing"
    :merge-fn
    (fn [val]
      {:italiano {:infl :imperfetto
                  :italiano (get-in val [:italiano :imperfetto :2sing] :nothing)
                  :agr {:number :sing
                        :person :2nd}}})}
   
   {:path [:italiano :imperfetto :3sing]
    :label "imperfetto 3sing"
    :merge-fn
    (fn [val]
      {:italiano {:infl :imperfetto
                  :italiano (get-in val [:italiano :imperfetto :3sing] :nothing)
                  :agr {:number :sing
                        :person :3rd}}})}
   
   {:path [:italiano :imperfetto :1plur]
    :label "imperfetto 1plur"
    :merge-fn
    (fn [val]
      {:italiano {:infl :imperfetto
                  :italiano (get-in val [:italiano :imperfetto :1plur] :nothing)
                  :agr {:number :plur
                        :person :1st}}})}
   
   {:path [:italiano :imperfetto :2plur]
    :label "imperfetto 2plur"
    :merge-fn
    (fn [val]
      {:italiano {:infl :imperfetto
                  :italiano (get-in val [:italiano :imperfetto :2plur] :nothing)
                  :agr {:number :plur
                        :person :2nd}}})}
   
   {:path [:italiano :imperfetto :3plur]
    :label "imperfetto 3plur"
    :merge-fn
    (fn [val]
      {:italiano {:infl :imperfetto
                  :italiano (get-in val [:italiano :imperfetto :3plur] :nothing)
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
                        :person :1st}}})
    :u {:infl :present
        :agr {:number :sing
              :person :1st}}}
   
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
    :u {:infl :present
        :agr {:number :plur
              :person :1st}}
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

