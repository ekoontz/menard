(ns menard.morphology
  (:require [clojure.string]
            [menard.exception :refer [exception]]
            [menard.lexiconfn :as l]
            #?(:clj [clojure.tools.logging :as log])
            #?(:cljs [menard.log :as log])
            [dag_unify.core :as u :refer [unify]]
            [dag_unify.diagnostics :as diag :refer [fail-path strip-refs]]))

(def ^:dynamic show-notes? true)

;; TODO move all emoji stuff to menard.morphology.emojis.

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

;; TODO: more factoring-out variables is possible beyond these two
;; ones for informal:
(def emoji-set-2
  {
   ;; vosotras
   :informal-feminine informal-feminine
   ;; vosotros
   :informal-masculine informal-masculine

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

(def emoji-set emoji-set-2)

(defn decode-notes [notes]
  (log/debug (str "decode-notes with notes: " notes))
  (cond
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
    (str "(unprintable note)")))


(defn concat-with-notes [structure surface]
  (let [note (u/get-in structure [:note])]
    (cond
      (nil? note)
      (str surface)

      (= :top note)
      (str surface)

      (not (seqable? note))
      (throw (Exception. (str "the :notes value of type: " (type note) " and value: " note " is not seqable in the structure: " structure)))
      
      note
      (str surface
           (if (and show-notes?
                    (= true (u/get-in structure [:show-notes?] true))
                    (seq note))
             (if-let [decode-notes (decode-notes note)]
               (str " " decode-notes))))
      
      (and (u/get-in structure [:note-on-first-word])
           (not (= :top (u/get-in structure [:note-on-first-word]))))
      (str
       (first (clojure.string/split surface #" "))
       (if (and show-notes?
                (u/get-in structure [:note-on-first-word])
                (not (= :top (u/get-in structure [:note-on-first-word])))
                (seq (u/get-in structure [:note-on-first-word])))
         (if-let [decode-notes (decode-notes (u/get-in structure [:note-on-first-word]))]
           (str " " decode-notes " " (clojure.string/join " " (rest (clojure.string/split surface #" ")))))))

      :else surface)))

(defn morph-leaf
  "Apply morphology to a leaf node of a tree: transform the leaf's canonical string into a
   an inflected string. The morphology is a set of rules, each of which has a :u and a :g. The :u is
   what to unify the structure against, and the :g contains a _from_ and a _to_, both of which
   are regular expressions used to transform the canonical form into the inflected form."
  [structure morphology & [option-map]]
  (log/debug (str "morph-leaf: structure: " (l/pprint structure)))
  (let [canonical (u/get-in structure [:canonical])
        inflected? (u/get-in structure [:inflected?] false)
        inflected? (if (= inflected? :top)
                     false
                     inflected?)
        show-sense? (or (:show-sense? option-map false) false)
        surface (u/get-in structure [:surface])
        matching-rules
        (when (and
               (or (not surface)
                   (= :top surface))
               (or (not inflected?)
                   (= :top inflected?)))
          ;; TODO: move this regular inflection-checking to *after*
          ;; exception-checking and :surface checking is done:
          ;; if there is an exception we; won't use the result of this
          ;; regular inflecting.
          ;; TODO: should allow multiple pairs of
          ;; <from,to> in the :g
          ;; not just one pair of <from,to>.
          ;; e.g. Instead of only:
          ;; [#"a" "b"],
          ;; should also allow:
          ;; [#"a" "b"
          ;;  #"c" "d"
          ;;  ...].
          (filter (fn [rule]
                    (let [{u :u
                           [from _] :g
                           debug :debug} rule]
                      (log/debug (str "morph-leaf: from: " from "; canonical: " canonical))
                      (and (string? canonical)
                           (re-find from canonical)
                           (let [result (unify u structure)]
                             (if (not (= :fail result))
                               (log/debug (str "success: g: " from))
                               (log/debug (str "fail:    g: " from "; fail-path: " (fail-path u structure) "; structure value: " (u/get-in structure (fail-path u structure))
                                               "; rule value: " (u/get-in u (fail-path u structure))
                                               (if debug (str "; debug: " debug)))))
                             (not (= :fail result))))))
                  morphology))
        exceptions (u/get-in structure [:exceptions])
        exceptionless (when exceptions
                        (dissoc structure :exceptions))
        first-matching-exception
        (when (and exceptions (not (keyword? exceptions))) ;; if :top, ignore.
          (first (filter #(not (= :fail %))
                          (map #(unify exceptionless %)
                               exceptions))))]
    (if first-matching-exception
      (log/debug (str "morph-leaf: found exception: " (diag/strip-refs first-matching-exception)))
      (log/debug (str "morph-leaf: no exception found; tried: " exceptions)))
    (log/debug (str "morph-leaf: number of matching rules: " (count matching-rules)))
    (when (seq (rest matching-rules))
      (log/debug (str "morph-leaf: more than one rule matched: " (diag/strip-refs structure) "; rules were: "
                      (->> (range 0 (count matching-rules))
                           (map (fn [i]
                                  (str "#" (+ 1 i) ": " (:u (nth matching-rules i)))))
                           (clojure.string/join ", ")))))
    (cond
      first-matching-exception
      (do
        (log/debug (str "found an exception: using that: " first-matching-exception))
        (morph-leaf first-matching-exception morphology option-map))
      :else
      (str
       (cond
         (and (u/get-in structure [:surface])
              (not (= (u/get-in structure [:surface]) :top)))
         (do
           (log/debug (str "found surface; using that: " (u/get-in structure [:surface])))
           (concat-with-notes structure (u/get-in structure [:surface])))
         
         (seq matching-rules)
         (let [{[from to] :g} (first matching-rules)]
           (log/debug (str "morph-leaf: using matching rule:" (first matching-rules)))
           (concat-with-notes structure (clojure.string/replace canonical from to)))

         (= true (u/get-in structure [:inflected?] false))
         (do
           (log/debug (str "leaf's :inflected? is true but there was no surface form, but found canonical: '" canonical "', so using that instead."))
           (concat-with-notes structure (u/get-in structure [:canonical])))
         
         (and (false? inflected?) (empty? matching-rules)
              (not (= structure {:head? false}))
              (not (= structure {:head? true}))
              canonical)
         (do
           (log/debug (str "Cannot determine surface from structure: " (strip-refs structure)` ". No rules matched canonical: '" canonical "' . Returning canonical."))
           canonical)
         
         :else
         "_")
       (let [sense (if show-sense?
                     (str (if (not (= :top (u/get-in structure [:sense] :top)))
                            (u/get-in structure [:sense]))
                          (if (u/get-in structure [:rule-sense])
                            (str "/" (u/get-in structure [:rule-sense])))))]
         (when (seq sense) (str "(" sense ")")))))))

;; Using a macro here for use by Clojurescript, so that
;; the Clojure (Java) side compiles it, since I haven't tried to get
;; the compilation working on Clojurescript, or maybe I tried and gave
;; up temporarily:
#?(:cljs
(defmacro compile-morphology [filenames]
  `(reduce
    concat
    ~(vec (map (fn [filename]
                 (l/read-and-eval filename)
                ;; allow a rule to be either a map (a single rule)
                ;; or a sequence of rules (the :else case below):
                (mapcat (fn [rule-or-rules]
                          (cond (map? rule-or-rules)
                                [rule-or-rules]
                                :else rule-or-rules))))
               filenames)))))

#?(:clj
(defn compile-morphology-fn [filenames]
  (reduce
   concat
   (vec (map (fn [filename]
               (->>
                (l/read-and-eval filename)
                ;; allow a rule to be either a map (a single rule)
                ;; or a sequence of rules (the :else case below):
                (mapcat (fn [rule-or-rules]
                          (cond (map? rule-or-rules)
                                [rule-or-rules]
                                :else rule-or-rules)))))

             filenames)))))
