(ns babel.espanol.morphology.verbs
  (:refer-clojure :exclude [future get-in merge resolve])
  (:require [babel.espanol.morphology.nouns :as nouns]
            [clojure.string :as string]
            [clojure.string :refer (trim)]
            #?(:clj [clojure.tools.logging :as log])
            #?(:cljs [babel.logjs :as log])
            [dag_unify.core :refer (copy dissoc-paths fail? get-in merge ref? strip-refs unifyc)]))

(defn exception [error-string]
  #?(:clj
     (throw (Exception. error-string)))
  #?(:cljs
     (throw (js/Error. error-string))))

;; TODO: replace with a runtime flag.
;; issue is that logging requires partial morphological evaluation,
;; whereas generation and batch jobs should immediately raise an exception.
;; Correct solution should catch in logging code.
(def suppress-morph-exceptions true)

(defn reflexive-to-infinitive [reflexive-infinitive]
  "e.g.: quedarse -> quedar"
  (string/replace reflexive-infinitive #"se$" ""))

(defn conditional [word  & [ {usted :usted
                              vosotros :vosotros
                              ustedes :ustedes}]]
  (let [infinitive (reflexive-to-infinitive (get-in word '[:espanol]))
        ar-type (try (re-find #"ar$" infinitive)
                     (catch Exception e
                       (exception (str "Can't regex-find on non-string: " infinitive " from word: " word))))
        er-type (re-find #"er$" infinitive)
        ir-type (re-find #"ir$" infinitive)
        stem (string/replace infinitive #"[iae]r$" "")
        ;; conditional shares usage of :future-stem with future.
        stem (if (get-in word [:future-stem])
               (get-in word [:future-stem])
               stem)
        last-stem-char-is-i (re-find #"ir$" infinitive)
        last-stem-char-is-e (re-find #"er$" infinitive)
        is-care-or-gare? (re-find #"[cg]ar$" infinitive)
        vosotros (if vosotros vosotros true)
        ustedes (if ustedes ustedes false)
        person (get-in word '(:agr :person))
        number (get-in word '(:agr :number))]

    (cond
     (and (= person :1st) (= number :sing) ar-type)
     (str stem "aría")
     (and (= person :1st) (= number :sing) er-type)
     (str stem "ería")
     (and (= person :1st) (= number :sing) ir-type)
     (str stem "iría")

     (and (= person :2nd) (= number :sing) ar-type)
     (str stem "arías")
     (and (= person :2nd) (= number :sing) er-type)
     (str stem "erías")
     (and (= person :2nd) (= number :sing) ir-type)
     (str stem "irías")

     (and (= person :2nd) (= number :sing) ar-type (= usted true))
     (str stem "aría")
     (and (= person :2nd) (= number :sing) er-type (= usted true))
     (str stem "ería")
     (and (= person :2nd) (= number :sing) ir-type (= usted true))
     (str stem "iría")

     (and (= person :3rd) (= number :sing) ar-type)
     (str stem "aría")
     (and (= person :3rd) (= number :sing) er-type)
     (str stem "ería")
     (and (= person :3rd) (= number :sing) ir-type)
     (str stem "iría")

     (and (= person :1st) (= number :plur) ar-type)
     (str stem "aríamos")

     (and (= person :1st) (= number :plur) er-type)
     (str stem "eríamos")

     (and (= person :1st) (= number :plur) ir-type)
     (str stem "iríamos")

     ;; <second person plural conditional>

     (and (= person :2nd) (= number :plur) ar-type vosotros)
     (str stem "aríais")

     (and (= person :2nd) (= number :plur) er-type vosotros)
     (str stem "eríais")

     (and (= person :2nd) (= number :plur) ir-type vosotros)
     (str stem "iríais")

     (and (= person :2nd) (= number :plur) ar-type ustedes)
     (str stem "arían")

     (and (= person :2nd) (= number :plur) er-type ustedes)
     (str stem "erían")

     (and (= person :2nd) (= number :plur) ir-type ustedes)
     (str stem "irían")

     ;; </second person plural conditional>

     ;; <third person plural conditional>
     (and (= person :3rd) (= number :plur)
          ar-type)
     (str stem "arían")

     (and (= person :3rd) (= number :plur)
          er-type)
     (str stem "erían")

     (and (= person :3rd) (= number :plur)
          ir-type)
     (str stem "irían")

     ;; </third person plural conditional>

     ;; agreement is underspecified, but an infinitive form (the :espanol key) exists, so just return that infinitive form.
     (and (= (get-in word [:agr]) :top)
          (string? (get-in word [:espanol])))
     (get-in word [:espanol])
     
     :else
     (exception (str "get-string-1: conditional regular inflection: don't know what to do with input argument: " (strip-refs word))))))

;; TODO: mask clojure/core's future to prevent warnings "WARNING: future already refers to: #'clojure.core/future"
(defn future [word & [ {usted :usted
                        vosotros :vosotros
                        ustedes :ustedes}]]
  (let [infinitive (reflexive-to-infinitive (get-in word '(:espanol)))
        ar-type (try (re-find #"ar$" infinitive)
                     (catch Exception e
                       (exception (str "Can't regex-find on non-string: " infinitive " from word: " word))))
        er-type (re-find #"er$" infinitive)
        ir-type (re-find #"ir$" infinitive)
        stem (string/replace infinitive #"[iae]r$" "")
        stem (if (get-in word [:future-stem])
               (get-in word [:future-stem])
               stem)
        last-stem-char-is-i (re-find #"ir$" infinitive)
        last-stem-char-is-e (re-find #"er$" infinitive)
        is-care-or-gare? (re-find #"[cg]ar$" infinitive)
        vosotros (if vosotros vosotros true)
        ustedes (if ustedes ustedes false)
        person (get-in word '(:agr :person))
        number (get-in word '(:agr :number))]

    (cond
     (and (= person :1st) (= number :sing) ar-type)
     (str stem "aré")
     (and (= person :1st) (= number :sing) er-type)
     (str stem "eré")
     (and (= person :1st) (= number :sing) ir-type)
     (str stem "iré")

     (and (= person :2nd) (= number :sing) ar-type)
     (str stem "aras")
     (and (= person :2nd) (= number :sing) ir-type)
     (str stem "iras")
     (and (= person :2nd) (= number :sing) er-type)
     (str stem "eras")
     
     (and (= person :2nd) (= number :sing) ar-type (= usted true))
     (str stem "ará")
     (and (= person :2nd) (= number :sing) ir-type (= usted true))
     (str stem "irá")
     (and (= person :2nd) (= number :sing) er-type (= usted true))
     (str stem "erá")

     (and (= person :3rd) (= number :sing) ar-type)
     (str stem "ará")
     (and (= person :3rd) (= number :sing) ir-type)
     (str stem "irá")
     (and (= person :3rd) (= number :sing) er-type)
     (str stem "erá")

     (and (= person :1st) (= number :plur) ar-type)
     (str stem "aremos")

     (and (= person :1st) (= number :plur) er-type)
     (str stem "eremos")

     (and (= person :1st) (= number :plur) ir-type)
     (str stem "iremos")

     ;; <second person plural future>

     (and (= person :2nd) (= number :plur) ar-type vosotros)
     (str stem "arais")

     (and (= person :2nd) (= number :plur) er-type vosotros)
     (str stem "erais")

     (and (= person :2nd) (= number :plur) ir-type vosotros)
     (str stem "irais")

     (and (= person :2nd) (= number :plur) ar-type ustedes)
     (str stem "aran")

     (and (= person :2nd) (= number :plur) er-type ustedes)
     (str stem "eran")
     
     (and (= person :2nd) (= number :plur) ir-type ustedes)
     (str stem "iran")

     ;; </second person plural future>

     ;; <third person plural future>
     (and (= person :3rd) (= number :plur)
          ar-type)
     (str stem "aran")

     (and (= person :3rd) (= number :plur)
          er-type)
     (str stem "eran")

     (and (= person :3rd) (= number :plur)
          ir-type)
     (str stem "iran")
     
     ;; </third person plural future>

     ;; agreement is underspecified, but an infinitive form (the :espanol key) exists, so just return that infinitive form.
     (and (= (get-in word [:agr]) :top)
          (string? (get-in word [:espanol])))
     (get-in word [:espanol])
     
     :else
     (let [message (str "get-string-1: future regular inflection: don't know what to do with input argument: " (strip-refs word))]
       (if (= suppress-morph-exceptions true)
         (do (log/warn message)
             "??")
         (exception message))))))

(defn imperfect [word & [ {usted :usted
                           vosotros :vosotros
                           ustedes :ustedes}]]
  (let [infinitive (reflexive-to-infinitive (get-in word [:espanol]))
        ar-type (try (re-find #"ar$" infinitive)
                     (catch Exception e
                       (exception (str "Can't regex-find on non-string: " infinitive " from word: " word))))
        er-type (re-find #"er$" infinitive)
        ir-type (re-find #"ir$" infinitive)
        stem (string/replace infinitive #"[iae]r$" "")
        last-stem-char-is-i (re-find #"ir$" infinitive)
        last-stem-char-is-e (re-find #"er$" infinitive)
        is-care-or-gare? (re-find #"[cg]ar$" infinitive)
        vosotros (if vosotros vosotros true)
        ustedes (if ustedes ustedes false)
        person (get-in word '(:agr :person))
        number (get-in word '(:agr :number))]
    (cond
     (and (= person :1st) (= number :sing) ar-type)
     (str stem "aba")

     (and (= person :1st) (= number :sing) (or ir-type er-type))
     (str stem "ía")

     (and (= person :2nd) (= number :sing) ar-type)
     (str stem "abas")

     (and (= person :2nd) (= number :sing) (or ir-type er-type))
     (str stem "ías")

     (and (= person :2nd) (= number :sing) ar-type (= usted true))
     (str stem "aba")

     (and (= person :2nd) (= number :sing) (or ir-type er-type) (= usted true))
     (str stem "ía")

     (and (= person :3rd) (= number :sing) ar-type)
     (str stem "aba")

     (and (= person :3rd) (= number :sing) (or ir-type er-type))
     (str stem "ía")

     (and (= person :1st) (= number :plur) ar-type)
     (str stem "ábamos")

     (and (= person :1st) (= number :plur) er-type)
     (str stem "íamos")

     (and (= person :1st) (= number :plur) ir-type)
     (str stem "íamos")

     ;; <second person plural imperfecto>

     (and (= person :2nd) (= number :plur) ar-type vosotros)
     (str stem "abais")

     (and (= person :2nd) (= number :plur) er-type vosotros)
     (str stem "íais")

     (and (= person :2nd) (= number :plur) ir-type vosotros)
     (str stem "íais")

     (and (= person :2nd) (= number :plur) ar-type ustedes)
     (str stem "aban")

     (and (= person :2nd) (= number :plur) er-type ustedes)
     (str stem "ían")

     (and (= person :2nd) (= number :plur) ir-type ustedes)
     (str stem "ían")

     ;; </second person plural imperfecto>

     ;; <third person plural imperfecto>
     (and (= person :3rd) (= number :plur)
             ar-type)
     (str stem "aban")

     (and (= person :3rd) (= number :plur)
          er-type)
     (str stem "ían")

     (and (= person :3rd) (= number :plur)
          ir-type)
     (str stem "ían")
     
     ;; </third person plural imperfecto>

     ;; agreement is underspecified, but an infinitive form (the :espanol key) exists, so just return that infinitive form.
     (and (= (get-in word [:agr]) :top)
          (string? (get-in word [:espanol])))
     (get-in word [:espanol])

     :else
     (let [word
           ;; for debugging if exception is thrown, where _word_ is printed out as part of the error message
           (merge word
                    {:agr {:ir-type ir-type
                           :ar-type ar-type
                           :er-type er-type
                           :vosotros vosotros
                           :ustedes ustedes}})]

       (let [message (str "get-string-1: imperfect regular inflection: don't know what to do with input argument: " (strip-refs word))]
         (if (= suppress-morph-exceptions true)
           (do (log/warn message)
               "??")
           (exception message)))))))

(defn present [word & [ {usted :usted
                         vosotros :vosotros
                         ustedes :ustedes}]]
  (let [infinitive (reflexive-to-infinitive (get-in word '(:espanol)))
        ar-type (try (re-find #"ar$" infinitive)
                     (catch Exception e
                       (exception (str "Can't regex-find on non-string: " infinitive " from word: " word))))
        er-type (re-find #"er$" infinitive)
        ir-type (re-find #"ir$" infinitive)
        
        stem (cond (and (get-in word [:boot-stem])
                        (or (= (get-in word [:agr :number])
                               :sing)
                            (and (= (get-in word [:agr :person])
                                    :3rd)
                                 (= (get-in word [:agr :number])
                                    :plur))))
                   (get-in word [:boot-stem])
                   true
                   (string/replace infinitive #"[iae]r$" ""))
        
        last-stem-char-is-i (re-find #"ir$" infinitive)
        last-stem-char-is-e (re-find #"er$" infinitive)
        is-care-or-gare? (re-find #"[cg]ar$" infinitive)
        person (get-in word '(:agr :person))
        number (get-in word '(:agr :number))]

    (cond
     
     (and (= person :1st) (= number :sing))
     (str stem "o")

     (and (= person :2nd) (= number :sing) ar-type (= false usted))
     (str stem "as")

     (and (= person :2nd) (= number :sing) ar-type usted)
     (str stem "a")
     
     (and (= person :2nd) (= number :sing) (or ir-type er-type) (= false usted))
     (str stem "es")

     (and (= person :2nd) (= number :sing) (or ir-type er-type) usted)
     (str stem "e")

     (and (= person :3rd) (= number :sing) ar-type)
     (str stem "a")
     (and (= person :3rd) (= number :sing) (or ir-type er-type))
     (str stem "e")

     (and (= person :1st) (= number :plur) ar-type)
     (str stem "amos")

     (and (= person :1st) (= number :plur) er-type)
     (str stem "emos")

     (and (= person :1st) (= number :plur) ir-type)
     (str stem "imos")
       
     ;; <second person plural present>

     (and (= person :2nd) (= number :plur) ar-type vosotros)
     (str stem "ais")

     (and (= person :2nd) (= number :plur) er-type vosotros)
     (str stem "eis")

     (and (= person :2nd) (= number :plur) ir-type vosotros)
     (str stem "ís")

     (and (= person :2nd) (= number :plur) ar-type ustedes)
     (str stem "an")

     (and (= person :2nd) (= number :plur) er-type ustedes)
     (str stem "en")

     (and (= person :2nd) (= number :plur) ir-type ustedes)
     (str stem "en")

     ;; </second person plural present>

     ;; <third person plural present>
     (and (= person :3rd) (= number :plur)
          ar-type)
     (str stem "an")
     (and (= person :3rd) (= number :plur)
          er-type)
     (str stem "en")
     (and (= person :3rd) (= number :plur)
          ir-type)
     (str stem "en")

     ;; </third person plural present>
        
     ;; agreement is underspecified, but an infinitive form (the :espanol key) exists, so just return that infinitive form.
     (and (= (get-in word [:agr]) :top)
          (string? (get-in word [:espanol])))
     (get-in word [:espanol])

     :else
     (let [message (str "get-string-1: present regular inflection: don't know what to do with input argument: " (strip-refs word))]
       (if (= suppress-morph-exceptions true)
         (do (log/warn message)
             "??")
         (exception message))))))

(declare irregular-preterito)
(declare regular-preterito)
;; TODO: word should have usted/vosotros/etc inside it, not passed along here.
(defn preterito [word & [ {usted :usted
                           vosotros :vosotros
                           ustedes :ustedes}]]
  (let [irregular (get-in word [:preterito])]
    (if (map? irregular)
      (irregular-preterito word
                           {:usted usted
                            :vosotros vosotros
                            :ustedes ustedes})
      ;; else
      (regular-preterito word
                         {:usted usted
                          :vosotros vosotros
                          :ustedes ustedes}))))

(defn irregular-preterito [word {usted :usted
                                 vosotros :vosotros
                                 ustedes :ustedes}]
  (cond
   (and (= :1st (get-in word [:agr :person]))
        (= :sing (get-in word [:agr :number]))
        (get-in word [:preterito :1sing]))
   (get-in word [:preterito :1sing])
   (and (= :2nd (get-in word [:agr :person]))
        (= :sing (get-in word [:agr :number]))
        (get-in word [:preterito :2sing]))
   (get-in word [:preterito :2sing])
   (and (= :3rd (get-in word [:agr :person]))
        (= :sing (get-in word [:agr :number]))
        (get-in word [:preterito :3sing]))
   (get-in word [:preterito :3sing])
   (and (= :1st (get-in word [:agr :person]))
        (= :plur (get-in word [:agr :number]))
        (get-in word [:preterito :1plur]))
   (get-in word [:preterito :1plur])
   (and (= :2nd (get-in word [:agr :person]))
        (= :plur (get-in word [:agr :number]))
        (get-in word [:preterito :2plur]))
   (get-in word [:preterito :2plur])
   (and (= :3rd (get-in word [:agr :person]))
        (= :plur (get-in word [:agr :number]))
        (get-in word [:preterito :3plur]))
   (get-in word [:preterito :3plur])
   true (regular-preterito word {:usted usted
                                 :vosotros vosotros
                                 :ustedes ustedes})))
   
(defn regular-preterito [word {usted :usted
                               vosotros :vosotros
                               ustedes :ustedes}]
  (let [infinitive (reflexive-to-infinitive (get-in word '(:espanol)))
        ar-type (try (re-find #"ar$" infinitive)
                     (catch Exception e
                       (exception (str "Can't regex-find on non-string: " infinitive " from word: " word))))
        er-type (re-find #"er$" infinitive)
        ir-type (re-find #"ir$" infinitive)
        
        ;; default stem: will be used except under certain conditions, as described in next check.
        stem (string/replace infinitive #"[iae]r$" "")
        
        stem (if (get-in word [:preterito-stem])
               (get-in word [:preterito-stem])
               stem)
        
        last-stem-char-is-i (re-find #"ir$" infinitive)
        last-stem-char-is-e (re-find #"er$" infinitive)
        is-care-or-gare? (re-find #"[cg]ar$" infinitive)
        vosotros (if vosotros vosotros true)
        ustedes (if ustedes ustedes false)
        person (get-in word '(:agr :person))
        number (get-in word '(:agr :number))]
    
    (cond
     (and (= person :1st) (= number :sing) ar-type)
     (str stem "é")
     
     (and (= person :1st) (= number :sing) (or ir-type er-type))
     (str stem "í")
     
     (and (= person :2nd) (= number :sing) ar-type (= usted false))
     (str stem "aste")
     
     (and (= person :2nd) (= number :sing) (or ir-type er-type) (= usted false))
     (str stem "iste")
     
     (and (= person :2nd) (= number :sing) ar-type (= usted true))
     (str stem "ó")
     
     (and (= person :2nd) (= number :sing) (or ir-type er-type) (= usted true))
     (str stem "ió")

     (and (= person :3rd) (= number :sing) ar-type)
     (str stem "ó")

     (and (= person :3rd) (= number :sing) (or ir-type er-type))
     (str stem "ió")

     (and (= person :1st) (= number :plur) ar-type)
     (str stem "amos")

     (and (= person :1st) (= number :plur) er-type)
     (str stem "imos")

     (and (= person :1st) (= number :plur) ir-type)
     (str stem "imos")

     ;; <second person plural preterite>

     (and (= person :2nd) (= number :plur) ar-type vosotros)
     (str stem "asteis")
     
     (and (= person :2nd) (= number :plur) er-type vosotros)
     (str stem "isteis")
     
     (and (= person :2nd) (= number :plur) ir-type vosotros)
     (str stem "isteis")

     (and (= person :2nd) (= number :plur) ar-type ustedes)
     (str stem "aron")

     (and (= person :2nd) (= number :plur) er-type ustedes)
     (str stem "ieron")

     (and (= person :2nd) (= number :plur) ir-type ustedes)
     (str stem "ieron")

     ;; </second person plural preterite>

     ;; <third person plural preterite>
     (and (= person :3rd) (= number :plur)
          ar-type)
     (str stem "aron")
     (and (= person :3rd) (= number :plur)
          er-type)
     (str stem "ieron")
     (and (= person :3rd) (= number :plur)
          ir-type)
     (str stem "ieron")

     ;; </third person plural preterite>

     ;; agreement is underspecified, but an infinitive form (the :espanol key) exists, so just return that infinitive form.
     (and (= (get-in word [:agr]) :top)
          (string? (get-in word [:espanol])))
     (get-in word [:espanol])
     
     :else
     (let [message (str "get-string-1: conditional regular inflection: don't know what to do with input argument: " (strip-refs word))]
       (if (= suppress-morph-exceptions true)
         (do (log/warn message)
             "??")
         (exception message))))))

