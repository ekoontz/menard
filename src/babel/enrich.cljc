(ns babel.enrich
  (:refer-clojure :exclude [get-in])
  (:require
   [dag_unify.core :refer [fail? get-in strip-refs unify]]
   #?(:clj [clojure.tools.logging :as log])
   #?(:cljs [babel.logjs :as log])))

(declare against-comp)
(declare against-pred)
(declare matching-comp-lexemes)
(declare matching-head-lexemes)

;; TODO: these rules are language-specific, so move all of this to italiano/writer (similar to what we did with French).
(defn enrich [spec lexicon]
  (against-pred spec lexicon))

(defn against-pred [spec lexicon]
  (let [pred (get-in spec [:synsem :sem :pred] :top)]
    (log/debug (str "against-pred with spec: " spec " and pred: " pred))
    (if (= :top pred)
      spec
      (mapcat (fn [lexeme]
                (let [result (unify spec
                                    {:synsem {:sem (strip-refs (get-in lexeme [:synsem :sem] :top))}})]
                  (if (not (fail? result))
                    (do
                      (log/debug (str "matched head lexeme: " (strip-refs lexeme)))
                      (list result)))))
              (matching-head-lexemes spec lexicon)))))

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
