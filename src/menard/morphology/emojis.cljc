(ns menard.morphology.emojis
  (:require [clojure.string]
            [menard.exception :refer [exception]]
            [menard.lexiconfn :as l]
            #?(:clj [clojure.tools.logging :as log])
            #?(:cljs [menard.log :as log])
            [dag_unify.core :as u :refer [unify]]
            [dag_unify.diagnostics :as diag :refer [fail-path strip-refs]]))

(def emoji-set-1
  {:informal ["🤠"]
   :formal   ["🧐"]})

(def informal-masculine ["👦" "👦🏻" "👦🏼" "👦🏼" "👦🏾" "👦🏾"])
(def informal-feminine  ["👧" "👧🏻" "👧🏼" "👧🏽" "👧🏾" "👧🏿"])
(def informal-neuter    ["🧒" "🧒🏻" "🧒🏼" "🧒🏽" "🧒🏾" "🧒🏿"])
(def formal-neuter      ["🧓🏻" "🧓🏼" "🧓🏽" "🧓🏾" "🧓🏾"])
(def formal-masculine   ["👴" "👴🏻" "👴🏼" "👴🏽" "👴🏾" "👴🏿"])
(def formal-feminine    ["👵" "👵🏻" "👵🏼" "👵🏽" "👵🏾" "👵🏿"])
(def informal   (concat informal-masculine
                        informal-feminine
                        informal-neuter))
(def formal     (concat formal-masculine
                        formal-feminine
                        formal-neuter))

(def emoji-to-informal (->> informal-masculine
                            (concat informal-feminine)
                            (concat informal-neuter)
                            (map (fn [emoji]
                                   [emoji [{:notes [:informal]}]]))
                            (into {})))

(def emoji-to-formal (->> formal-masculine
                          (concat formal-feminine)
                          (concat formal-neuter)
                          (map (fn [emoji]
                                 [emoji [{:notes [:formal]}]]))
                          (into {})))
(def music-emojis ["🎶" "🎵" "️🎺" "🎻" "🪕" "🎷" "🎸" "🥁" "🪗" "🎼" "🪉" "🎹"])
(def game-emojis ["⚽️" "🏉" "🏐"  "🏈" "🏑" "🏒" "🏸" "🏓" "🎲" "🎱" "🎮"])

(defn emoji-set-fn-1 [notes]
  (let [emoji-set emoji-set-1
        notes (set notes)]
    (log/debug (str "emoji-set-fn-1 with notes: " notes " and emoji-set: " emoji-set))
    (cond (contains? notes :formal)
          (str "(" (:formal emoji-set) ")")
          (contains? notes :plural)
          (str "(" (:informal emoji-set) ")")
          :else "")))

;; TODO: more factoring-out variables is possible beyond these two
;; ones for informal:
(def emoji-set-2
  {
   ;; vosotras
   :informal-feminine informal-feminine
   ;; vosotros
   :informal-masculine informal-masculine

   :formal-feminine formal-feminine
   :formal-masculine formal-masculine   
   
   ;; tú
   :informal (concat informal-masculine
                     informal-feminine
                     informal-neuter)
   ;; usted
   :formal   (concat formal-masculine
                     formal-feminine
                     formal-neuter)

   :all      (concat informal formal)
   
   ;; nosotros
   :masculine (concat informal-masculine
                      formal-masculine)
   
   ;; nosotras
   :feminine (concat informal-feminine
                     formal-feminine)})

(defn emoji-set-fn-2 [notes]
  (let [emoji-set emoji-set-2]
    (log/debug (str "emoji-set-fn-2 with notes: " notes " and emoji-set: " emoji-set))
    (cond
      (= notes "games")
      (-> game-emojis shuffle first)
      (= notes "music")
      (-> music-emojis shuffle first)

      (= notes [:informal :feminine :plural])
      (str (clojure.string/join ""
                                (take 2 (repeatedly #(first (shuffle (get emoji-set :informal-feminine)))))))


      ;; this case is a little more complicated because "nosotros/vosotros" is
      ;; *at least* one male is in the group, but there may also be female in that
      ;; same group. So we want to have the option to show a case of the latter
      ;; (i.e. mixed male and female):
      (= notes [:informal :masculine :plural])
      (str (clojure.string/join ""
                                [(first (shuffle (get emoji-set :informal-masculine)))
                                 (first (shuffle (get emoji-set :informal)))]))

      (= notes [:informal :singular])
      (str (clojure.string/join ""
                                (first (shuffle (get emoji-set :informal)))))
      (= notes [:formal :singular])
      (str (clojure.string/join ""
                                (first (shuffle (get emoji-set :formal)))))
      (= notes [:informal :plural])
      (str (clojure.string/join ""
                                (take 2 (shuffle (get emoji-set :informal)))))
      (= notes [:formal :plural])
      (str (clojure.string/join ""
                                (take 2 (shuffle (get emoji-set :formal)))))
      (= notes [:feminine :plural])
      (str (clojure.string/join ""
                                (take 2 (shuffle (get emoji-set :feminine)))))

      (= notes [:formal :singular :feminine])
      (str (clojure.string/join ""
                                (first (shuffle (get emoji-set :formal-feminine)))))

      (= notes [:formal :singular :masculine])
      (str (clojure.string/join ""
                                (first (shuffle (get emoji-set :formal-masculine)))))

      ;; same applies here as above with "nosotros/vosotros"
      (= notes [:masculine :plural])
      (str (clojure.string/join ""
                                [(first (shuffle (get emoji-set :masculine)))
                                 (first (shuffle (get emoji-set :all)))]))

      (= notes [:formal])
      (str (first (shuffle (get emoji-set :formal))))

      (= notes [:informal])
      (str (first (shuffle (get emoji-set :informal))))

      ;; no emoji or other cues for now.
      (= notes [:human?])
      nil
      (= notes [:human])
      nil
      (= notes [:nonhuman])
      nil

      (or (vector? notes) (seq? notes))
      (str "(" (clojure.string/join "," notes) ")")

      (string? notes)
      (str "(" notes ")")

      ;;
      :else
      (str "(unprintable note)"))))

(def emoji-set-3
  {:singular "👤"
   :plural   "👥"})

(defn emoji-set-fn-3 [notes]
  (let [notes (set notes)
        emoji-set emoji-set-3]
    (cond (contains? notes :singular)
          (str "(" (:singular emoji-set-3) ")")
          (contains? notes :plural)
          (str "(" (:plural emoji-set-3) ")")
          :else "")))

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










