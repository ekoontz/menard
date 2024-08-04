(ns menard.nederlands.compile
  (:require #?(:clj [clojure.tools.logging :as log])
            #?(:cljs [cljslog.core :as log])
            [dag_unify.core :as u :refer [unify]]
            [menard.lexiconfn :as l]))

(defn get-inflection-of [lexeme morphology]
  (when lexeme
    (log/debug (str "getting inflection of: " (menard.serialization/pprint lexeme)))
    (->> morphology
         (map (fn [rule]
                {:u (reduce unify
                            [lexeme (:u rule)
                             {:cat :noun
                              :exception false
                              :agr {:number :plur}}])
                 :m (re-find (-> rule :g first)
                             (:canonical lexeme))}))
         (filter (fn [x] (and (not (= :fail (:u x)))
                                 (not (nil? (:m x))))))
         (map (fn [result]
                (-> result :u :inflection)))
         first)))

(defn mark-irregular-verbs [lexeme]
  (if (or (seq (->> (u/get-in lexeme [:exceptions])
                    (filter #(= :past-simple (u/get-in % [:infl])))))
          (not (= false (u/get-in lexeme [:strong?] false))))
    (unify lexeme {:irregular-past-simple? true})
    (unify lexeme {:irregular-past-simple? false})))

(defn compile-lexicon [lexicon morphology-rules filter-fn]
  (log/info (str "running (compile-lexicon).."))
  (let [retval
        (-> lexicon
            (l/apply-to-every-lexeme
             (fn [lexeme]
               (if (and (= :noun (u/get-in lexeme [:cat]))
                        (not (= true (u/get-in lexeme [:propernoun?])))
                        (not (= true (u/get-in lexeme [:pronoun?]))))
                 (let [inflection (get-inflection-of lexeme morphology-rules)]
                   (cond
                     inflection
                     (unify lexeme
                            {:inflection inflection})
                     (and (= :noun (u/get-in lexeme [:cat]))
                          (= true (u/get-in lexeme [:sem :countable?]))
                          (not (= true (u/get-in lexeme [:propernoun?])))
                          (not (= true (u/get-in lexeme [:pronoun?])))
                          (false? (u/get-in lexeme [:inflected?] false)))
                     (do
                       (log/warn (str "no inflection found for lexeme: "
                                      (u/get-in lexeme [:canonical])))
                       lexeme)
                     
                     :else lexeme))
                 lexeme)))
            
            ;; The lexicon is a map where each
            ;; key is a canonical string
            ;; and each value is the list of lexemes for
            ;; that string. we turn the list into a vec
            ;; so that it's completely realized rather than
            ;; a lazy sequence, so that when we periodically
            ;; reload the model from disk, (generate) or
            ;; (parse) won't have to de-lazify the list:
            ;; it will already be done before they (generate or
            ;; parse) see it.
            ;; (TODO: move this to some function within
            ;;  menard/model).
            ((fn [lexicon]
               (zipmap (keys lexicon)
                       (map (fn [vs]
                              (vec vs))
                            (vals lexicon)))))
            
            ((fn [lexicon]
               (filter-fn lexicon))))]
    (log/info (str "done."))
    retval))

    
