(ns babel.english.demo
  (:require
   [babel.english :refer [generate]]
   [babel.english.morphology :refer [fo]]
   #?(:cljs [babel.logjs :as log])
   #?(:clj [clojure.tools.logging :as log])))

(declare run-demo-with)

(defn demo [ & [n]]
  (let [demo-specs
        [{:synsem :top
          :demo-name "Totally random expressions"}

         {:synsem {:cat :verb}
          :demo-name "Sentences"}

         {:demo-name "Sentences about Eating"
          :synsem {:cat :verb
                   :sem {:pred :mangiare}}}

         {:demo-name "Noun Phrases"
          :synsem {:cat :noun}}
         
         {:demo-name "A variety of dogs"
          :synsem {:cat :noun
                   :sem {:pred :cane}}}

         {:demo-name "Sentences about dogs eating"
          :synsem {:cat :verb
                   :sem {:subj {:pred :cane}
                         :pred :mangiare}}}]]
         
    (count (map (fn [spec]
                  (let [log-message 
                        (str "running demo: " (:demo-name spec) "; " n " attempts.")]
                    (do (log/info log-message)
                        (println)
                        (println log-message)
                        (println)
                        (let [expressions (run-demo-with n spec)]
                          (count (pmap (fn [expression]
                                         (do
                                           (println (fo expression))))
                                       expressions))))))
                demo-specs))))

(defn run-demo-with [ & [n spec]]
  "print out _n_ generated sentences to stdout."
  (let [n (if n (Integer. n)
              100)
        spec (if spec spec
                 :top)]
    (filter #(not (nil? %)) 
            (take n (repeatedly
                     #(let [result
                            (generate spec)]
                        result))))))

