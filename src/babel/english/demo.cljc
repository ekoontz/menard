(ns babel.english.demo
  (:require
   [babel.english :refer [generate]]
   [babel.english.grammar :refer [few-rules medium small-lexicon]]
   [babel.english.morphology :refer [fo]]
   #?(:cljs [babel.logjs :as log])
   [clojure.string :as string]
   #?(:clj [clojure.tools.logging :as log])
   [dag_unify.core :refer [unifyc]]))

(declare run-demo-with)

;; during generation, will not decend deeper than this when creating a tree:
;; should also be possible to override per-language.
(def ^:const max-total-depth 25)

;; run each (generate) as (time (generate)):
(def ^:const timings? false)

(def default-language-model (medium))

(defn demo [ & [n spec]]
  (let [demo-specs
        [{:demo-name "Dog noun phrases"
          :synsem {:cat :noun
                   :sem {:pred :dog}}}

         {:demo-name "Sentences about dogs eating"
          :synsem {:cat :verb
                   :sem {:subj {:pred :dog}
                         :pred :eat}}}

         {:demo-name "The adventures of Luisa's yellow cat"
          :synsem {:cat :verb
                   :sem {:subj {:pred :cat
                                :mod {:pred :yellow}
                                :spec {:def :genitive
                                       :of {:pred :luisa}}}}}}

         {:demo-name "Women who read books"
          :synsem {:cat :verb
                   :sem {:subj {:pred :woman}
                         :pred :read
                         :obj {:pred :book}}}}

         {:demo-name "Thinking"
          :synsem {:cat :verb
                   :sem {:pred :think}}}

         ]]
         
    (doall (map (fn [spec]
                  (let [log-message 
                        (str "running demo: " (:demo-name spec) "..")]
                    (do (if timings? (log/info log-message))
                        (println)
                        (println log-message)
                        (println)
                        (let [language-model (or (:lm spec) default-language-model)
                              expressions (run-demo-with n (dissoc spec :lm) language-model)]
                          (count (pmap (fn [expression]
                                         (let [formatted (fo expression :show-notes false)]
                                           (println
                                            (str (string/capitalize (subs formatted 0 1))
                                                 (subs formatted 1 (count formatted))
                                                 "."))))
                                       expressions))))))
                (if (and spec (not (empty? (string/trim spec))))
                  (filter #(= (:demo-name %)
                              spec)
                          demo-specs)
                  demo-specs)))
    (println (str "babel demo is finished. JVM shutting down."))))

(defn run-demo-with [n spec model]
  "print out _n_ generated sentences to stdout."
  (let [n (if n (Integer. n)
              10)
        spec (if spec (unifyc spec
                              {:modified false})
                 {:modified false})]
    (filter #(not (nil? %))
            (if timings?
              (take n (repeatedly
                       #(time
                         (generate spec
                                   :model model
                                   :max-total-depth max-total-depth))))
              (take n (repeatedly
                       #(generate spec
                                  :model model
                                  :max-total-depth max-total-depth)))))))


