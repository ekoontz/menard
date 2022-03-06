(ns menard.translate.spec
  (:require #?(:clj [clojure.tools.logging :as log])
            #?(:cljs [cljslog.core :as log])
            [dag_unify.core :as u :refer [unify]]
            [dag_unify.serialization :refer [serialize]]
            [dag_unify.diagnostics :as diag]))

(def max-depth 20)

(defn nl-to-en-spec [nl-expression]
  (log/debug (str "nl-to-en-spec: nl-expression: " (serialize nl-expression)))
  (let [
        ;; If [:sem :subj] exists, then set polarity to :plus if it's not
        ;; otherwise set.
        subj-polarity-if-subj
        (if (u/get-in nl-expression [:sem :subj])
          {:sem {:subj {:polarity (u/get-in nl-expression [:sem :subj :polarity] :plus)}}}
          :top)

        retval
        (unify subj-polarity-if-subj
               {:agr {:number
                      (or
                       (u/get-in nl-expression [:sem :subj :ref :number])
                       (u/get-in nl-expression [:agr :number] :top))
                      :person (u/get-in nl-expression [:agr :person] :top)
                      :gender (let [gender (u/get-in nl-expression [:agr :gender] :top)]
                                (cond (or (= gender :masc) (= gender :fem))
                                      gender
                                      :else :top))}
                :cat (u/get-in nl-expression [:cat])
                :comp {:interrogative? (u/get-in nl-expression [:comp :interrogative?] :top)}
                
                :max-depth (u/get-in nl-expression [:target :max-depth] max-depth)
                :phrasal? (u/get-in nl-expression [:phrasal?] true)
                :reflexive? (cond (= :top (u/get-in nl-expression [:reflexive?] :top))
                                  false
                                  :else
                                  (u/get-in nl-expression [:reflexive?] :top))
                ;; TODO: this is totally unintuitive: see TODO(1) below.
                :sem (unify (u/get-in nl-expression [:sem] :top)
                            (cond (not (= :fail
                                          (unify (u/get-in nl-expression [:sem] :top)
                                                 {:obj {:obj (u/get-in nl-expression [:sem :obj :obj])}})))
                                  (unify (u/get-in nl-expression [:sem] :top)
                                         {:obj {:obj (u/get-in nl-expression [:sem :obj :obj])}})
                                  true :top))
                :subcat []})]
    (log/debug (str "English spec to generate: " (serialize retval)))
    (let [final-check
          (unify
           retval
           (u/get-in nl-expression [:target] :top))]
      (if (= :fail final-check)
        (do (log/warn (str "something was wrong with the :target spec: "
                           (diag/fail-path retval (u/get-in nl-expression [:target] :top)) " "
                           ", so ignoring it."))
            retval)
        final-check))))

