(ns babel.enrich
  (:refer-clojure :exclude [get-in]))
(require '[dag-unify.core :refer [fail? get-in strip-refs unify]])
(require '[clojure.tools.logging :as log])

(declare against-comp)
(declare against-pred)
(declare matching-comp-lexemes)
(declare matching-head-lexemes)

(defn enrich [spec lexicon]
  (let [against-pred (against-pred spec lexicon)]
    (if true against-pred
        ;; TODO: remove this else since it's never executed (since above we have "if true")
        (let [against-comp (map (fn [spec]
                                  (against-comp spec lexicon))
                                (if (seq? against-pred)
                                  (seq (set against-pred))
                                  against-pred))]
          (if (seq? against-comp)
            (seq (set against-comp))
            against-comp)))))

(defn against-pred [spec lexicon]
  (let [pred (get-in spec [:synsem :sem :pred] :top)]
    (if (= :top pred)
      spec
      (mapcat (fn [lexeme]
                (let [result (unify spec
                                    {:synsem {:sem (strip-refs (get-in lexeme [:synsem :sem] :top))}}
                                    {:synsem {:essere (strip-refs (get-in lexeme [:synsem :essere] :top))}}
                                    )]
                  (if (not (fail? result))
                    (do
                      (log/debug (str "matched head lexeme: " (strip-refs lexeme)))
                      (list result)))))
              (matching-head-lexemes spec lexicon)))))

(defn against-comp [spec lexicon]
  (let [pred-of-comp (get-in spec [:synsem :sem :subj :pred] :top)]
    (if (= :top pred-of-comp)
      spec
      (mapcat (fn [lexeme]
                (let [result (unify spec
                                    {:comp {:synsem {:agr (strip-refs (get-in lexeme [:synsem :agr] :top))
                                                     :sem (strip-refs (get-in lexeme [:synsem :sem] :top))}}})]
                  (if (not (fail? result))
                    (list result))))
              (matching-comp-lexemes spec)))))

(defn matching-head-lexemes [spec lexicon]
  (let [pred-of-head (get-in spec [:synsem :sem :pred] :top)]
    (if (= pred-of-head :top)
      spec
      (mapcat (fn [lexemes]
                (mapcat (fn [lexeme]
                          (if (= pred-of-head
                                 (get-in lexeme [:synsem :sem :pred] :top))
                            (list lexeme)))
                        lexemes))
              (vals lexicon)))))

(defn matching-comp-lexemes [spec lexicon]
  (let [pred-of-comp (get-in spec [:synsem :sem :subj :pred] :top)]
    (if (= pred-of-comp :top)
      spec
      (mapcat (fn [lexemes]
                (mapcat (fn [lexeme]
                          (if (= pred-of-comp
                                 (get-in lexeme [:synsem :sem :pred] :top))
                            (list lexeme)))
                        lexemes))
              (vals lexicon)))))
