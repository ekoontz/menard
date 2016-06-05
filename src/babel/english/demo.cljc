(ns babel.english.demo
  (:require
   [babel.english :refer [generate]]
   [babel.english.morphology :refer [fo]]
   #?(:cljs [babel.logjs :as log])
   [clojure.string :as string]
   #?(:clj [clojure.tools.logging :as log])))

(declare run-demo-with)

(defn demo [ & [n spec]]
  (let [demo-specs
        [{:demo-name "Dog noun phrases"
          :synsem {:cat :noun
                   :sem {:pred :cane}}}

         {:demo-name "Noun Phrases"
          :synsem {:cat :noun}}
         
         {:demo-name "Sentences about dogs eating"
          :synsem {:cat :verb
                   :sem {:subj {:pred :cane}
                         :pred :mangiare}}}

         {:demo-name "Sentences about thinking"
          :synsem {:cat :verb
                   :sem {:pred :think}}}

         {:synsem {:cat :verb}
          :demo-name "Sentences"}

         {:synsem :top
          :demo-name "Totally random expressions"}
         ]]
         
    (count (map (fn [spec]
                  (let [log-message 
                        (str "running demo: " (:demo-name spec) "; " n " attempts.")]
                    (do (log/info log-message)
                        (println)
                        (println log-message)
                        (println)
                        (let [expressions (run-demo-with n spec)]
                          (count (pmap (fn [expression]
                                         (let [formatted (fo expression :show-notes false)]
                                           (println
                                            (str (string/capitalize (subs formatted 0 1))
                                                 (subs formatted 1 (count formatted))
                                                 "."))))
                                       expressions))))))
                (if spec
                  (filter #(= (:demo-name %)
                              spec)
                          demo-specs)
                  demo-specs)))))

(defn run-demo-with [ & [n spec]]
  "print out _n_ generated sentences to stdout."
  (let [n (if n (Integer. n)
              100)
        spec (if spec spec
                 :top)]
    (filter #(not (nil? %)) 
            (take n (repeatedly
                     #(let [result
                            (time (generate spec))]
                        result))))))

