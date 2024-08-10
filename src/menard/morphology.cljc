(ns menard.morphology
  (:require [clojure.string]
            [menard.exception :refer [exception]]
            [menard.lexiconfn :as l]
            #?(:clj [clojure.tools.logging :as log])
            #?(:cljs [menard.log :as log])
            [dag_unify.core :as u :refer [unify]]
            [dag_unify.diagnostics :as diag :refer [fail-path strip-refs]]))

(def ^:dynamic show-notes? true)

(def emoji-set-1
  {:informal ["🤠"]
   :formal   ["🧐"]})

(def emoji-set-2
  {:informal ["🧒" "🧒🏻" "🧒🏼" "🧒🏽" "🧒🏾" "🧒🏿"
              "👦" "👦🏻" "👦🏼" "👦🏼" "👦🏾" "👦🏾"
              "👧" "👧🏻" "👧🏼" "👧🏽" "👧🏾" "👧🏿"]

   ;; vosotras
   :informal-feminine-plural ["👧" "👧🏻" "👧🏼" "👧🏽" "👧🏾" "👧🏿"]

   ;; nosotros
   :masculine-plural ["👴" "👴🏻" "👴🏼" "👴🏽" "👴🏾" "👴🏿"
                      "👦" "👦🏻" "👦🏼" "👦🏼" "👦🏾" "👦🏾"]
   ;; nosotras
   :feminine-plural ["👧" "👧🏻" "👧🏼" "👧🏽" "👧🏾" "👧🏿"
                     "👵" "👵🏻" "👵🏼" "👵🏽" "👵🏾" "👵🏿"]
   
   :formal   ["🧓" "🧓🏻" "🧓🏼" "🧓🏽" "🧓🏾" "🧓🏾"
              "👴" "👴🏻" "👴🏼" "👴🏽" "👴🏾" "👴🏿"
              "👵" "👵🏻" "👵🏼" "👵🏽" "👵🏾" "👵🏿"]})

(def emoji-set emoji-set-2)

(defn decode-notes [notes]
  (log/debug (str "decode-notes with notes: " notes))
  (cond
    (= notes [:informal :feminine :plural])
    (str (clojure.string/join ""
                              (take 2 (repeatedly #(first (shuffle (get emoji-set :informal-feminine-plural)))))))
    (= notes [:informal :singular])
    (str (clojure.string/join ""
                              (take 1 (repeatedly #(first (shuffle (get emoji-set :informal)))))))
    (= notes [:formal :singular])
    (str (clojure.string/join ""
                              (take 1 (repeatedly #(first (shuffle (get emoji-set :formal)))))))
    (= notes [:informal :plural])
    (str (clojure.string/join ""
                              (take 2 (repeatedly #(first (shuffle (get emoji-set :informal)))))))
    (= notes [:formal :plural])
    (str (clojure.string/join ""
                              (take 2 (repeatedly #(first (shuffle (get emoji-set :formal)))))))
    (= notes [:feminine :plural])
    (str (clojure.string/join ""
                              (take 2 (repeatedly #(first (shuffle (get emoji-set :feminine-plural)))))))
    (= notes [:masculine :plural])
    (str (clojure.string/join ""
                              (take 2 (repeatedly #(first (shuffle (get emoji-set :masculine-plural)))))))


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
                               (log/debug (str "success: u: " u " and structure: " (strip-refs structure) " and debug: " debug))
                               (log/debug (str "fail:     u: " u "; and structure: " (strip-refs structure) " : " (fail-path u structure) " and debug: " debug " and canonical: " canonical)))
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
        (morph-leaf first-matching-exception morphology))
      :else
      (str
       (cond
         (and (u/get-in structure [:surface])
              (not (= (u/get-in structure [:surface]) :top)))
         (do
           (log/debug (str "found surface; using that: " (u/get-in structure [:surface])))
           (str
            (u/get-in structure [:surface])
            (if (and show-notes?
                     (u/get-in structure [:note])
                     (not (= :top (u/get-in structure [:note])))
                     (seq (u/get-in structure [:note])))
              (if-let [decode-notes (decode-notes (u/get-in structure [:note]))]
                (str " " decode-notes)))))

         (seq matching-rules)
         (let [{[from to] :g} (first matching-rules)]
           (log/debug (str "using matching rule:" (first matching-rules)))
           (clojure.string/replace canonical from to))

         (= true (u/get-in structure [:inflected?] false))
         (do
           (log/debug (str "leaf's :inflected? is true; found canonical: '" canonical "'; using that."))
           (str canonical
                (if (and show-notes?
                         (u/get-in structure [:note])
                         (not (= :top (u/get-in structure [:note])))
                         (seq (u/get-in structure [:note])))
                  (if-let [decode-notes (decode-notes (u/get-in structure [:note]))]
                    (str " " decode-notes)))))

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
                     (str (u/get-in structure [:sense])
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
