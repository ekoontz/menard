(ns menard.morphology.emojis
  (:require [clojure.string]
            [menard.exception :refer [exception]]
            [menard.lexiconfn :as l]
            #?(:clj [clojure.tools.logging :as log])
            #?(:cljs [menard.log :as log])
            [dag_unify.core :as u :refer [unify]]
            [dag_unify.diagnostics :as diag :refer [fail-path strip-refs]]))

(defn character-within-emoji? [character-name]
  (or (re-matches #"^LOW SURROGATES.*" character-name)
      (re-matches #"^EMOJI MODIFIER.*" character-name)))

(defn group-into-emojis [remaining-code-points & [emojis working-on-emoji]]
  (log/debug (str "group-into-emojis: remaining-code-points: "
                 remaining-code-points))
  (log/debug (str "                   emojis: " (vec emojis)))
  (log/debug (str "                   working-on-emoji: " (vec working-on-emoji)))
  (cond (empty? remaining-code-points)
        (concat emojis (if (not (empty? working-on-emoji)) [working-on-emoji]))
        :else
        (let [current-code-point (first remaining-code-points)]
          (log/debug (str "group-into-emojis: current-code-point: "
                          current-code-point))
          (cond (character-within-emoji? current-code-point)
                ;; in the middle of an emoji: continue working on it:
                (do
                  (log/debug (str " within-emoji? yes"))
                  (group-into-emojis (rest remaining-code-points) emojis
                                     (concat working-on-emoji [current-code-point])))
                :else ;; starting a new emoji:
                (do
                  (log/debug (str " starting a new emoji; existing one: " (vec working-on-emoji)))
                                 
                  (group-into-emojis (rest remaining-code-points)
                                     (concat emojis (if (not (empty? working-on-emoji))
                                                      [working-on-emoji]))
                                     [current-code-point]))))))

(defn emoji-names
  "scan the _input_ for emojis and turn them into emoji names e.g.
  'WOMAN' or 'EMOJI MODIFIER FITZPATRICK TYPE-6'"
  [input]
  (->> (range 0 (.length input))
       (map (fn [i]
              (.codePointAt input i)))
       (map (fn [codePoint]
              (Character/getName codePoint)))))

(defn encode-emojis [emojis]
  (log/debug (str "emojis: " (vec emojis)))
  (let [number (cond (> (count emojis) 1)
                     :plural
                     (= (count emojis) 1)
                     :singular)]
    (remove nil?
            (cons number
                  (mapcat (fn [emoji]
                            (let [character (first emoji)]
                              (log/debug (str "character: " character))
                              (cond (= character "BOY")
                                    [:informal :masculine]
                                    (= character "GIRL")
                                    [:informal :feminine]
                                    (= character "CHILD")
                                    [:informal :neuter]
                            (= character "OLDER WOMAN")
                            [:formal :feminine]
                            (= character "OLDER MAN")
                            [:formal :masculine]
                            (= character "OLDER ADULT")
                            [:formal :neuter]
                            :else [])))
                          emojis)))))

(defn precedence [keywords]
  (cond
    (contains? keywords :masculine)
    (cons :masculine
          (precedence
           (->> keywords
                (remove #(= % :masculine))
                (remove #(= % :neuter))
                (remove #(= % :feminine))
                set)))
    (contains? keywords :neuter)
    (cons :neuter
          (precedence
           (->> keywords
                (remove #(= % :neuter))
                (remove #(= % :feminine))
                set)))
    (contains? keywords :informal)
    (cons :informal
          (precedence
           (->> keywords
                (remove #(= % :informal))
                (remove #(= % :formal))                
                set)))

    :else keywords))

(defn into-maps [keywords]
  (cond
    (empty? keywords)
    [:top]

    (or (= (first keywords) :formal)
        (= (first keywords) :informal))
    (cons
     {:sem {:ref {:context (first keywords)}}}
     (into-maps (rest keywords)))

    (= (first keywords) :feminine)
    (cons
     {:agr {:gender :fem}}
     (into-maps (rest keywords)))

    (= (first keywords) :masculine)
    (cons
     {:agr {:gender :masc}}
     (into-maps (rest keywords)))

    (= (first keywords) :plural)
    (cons
     {:agr {:number :plur}}
     (into-maps (rest keywords)))

    (= (first keywords) :singular)
    (cons
     {:agr {:number :sing}}
     (into-maps (rest keywords)))
    
    :else
    (cons
     {:emoji? true}
     (into-maps (rest keywords)))))

(defn string-to-maps [input-string]
  (let [emoji-names (-> input-string clojure.string/trim emoji-names)]
    (if (and (seq emoji-names)
             (empty? (->> emoji-names (filter #(re-matches #"^LATIN .*" %)))))
      [(->> (-> emoji-names
                group-into-emojis encode-emojis
                set
                precedence
                into-maps)
            (map #(unify % {:emoji? true
                            :phrasal? false}))
            (reduce unify))])))










