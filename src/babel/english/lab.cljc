(ns babel.english.lab
  (:require
   [babel.directory :refer [models]] ;; this is needed even though there are no references to directory in here.
   [babel.generate :as g :refer [frontier generate get-lexemes]]
   [babel.english :as english :refer [model morph morph-ps]]
   [clojure.core.async :refer [>! alts!! timeout chan go]]
   #?(:cljs [babel.logjs :as log])
   #?(:clj [clojure.tools.logging :as log])
   #?(:clj [clojure.repl :refer [doc]])
   [dag_unify.core :as u :refer [pprint strip-refs unify]]))

(defn parse [surface-string]
  (english/parse surface-string model false)) ;; false: don't truncate.

(defn nursery
  "very fast and easy sentences."
  []
  (let [spec {:synsem {:cat :verb
                       :subcat []}
              :comp {:phrasal false}
              :head {:phrasal false}}]
    (repeatedly
     #(println
       (morph (binding [babel.generate/println? false
                        babel.generate/truncate? true]
                (time (generate spec model)))
              :show-notes false)))))

(defn downtown []
  (let [specs
        [{:synsem {:cat :verb
                   :subcat []
                   :sem {:aspect :simple
                         :tense :present}}}]]
    (repeatedly
     #(println
       (let [spec (first (shuffle specs))]
         (morph (binding [babel.generate/println? false
                          babel.generate/truncate? true]
                  (generate spec model))
                :show-notes false))))))

(def ^:dynamic wait-ms-for-generation 10000)

(declare wait)

(defn relative-clauses []
  ;; TODO: improve performance by using {:mod {:first {:obj :modified}}.
  (let [tree-spec
         {:rule "sentence-nonphrasal-head"
          :phrasal true
          :comp {:phrasal true
                 :rule "noun-phrase"
                 :head {:rule "nbar-s-obj"
                        :phrasal true
                        :head {:rule "nbar"
                               :phrasal true
                               :comp {:phrasal false}
                               :head {:phrasal false}}
                        :comp {:rule "s/obj"
                               :head {:phrasal false}
                               :comp {:phrasal false}
                               :phrasal true}}}}
        meaning-spec
        {:comp {:phrasal true}
         :synsem {:cat :verb
                  :subcat []
                  :sem {:pred :sleep
                        :subj {:mod {:first {:pred :see}
                                     :rest {:first {:pred :red}}}}}}}
        spec (unify tree-spec meaning-spec)
        spec meaning-spec]
         
    (repeatedly #(println
                  (let [spec spec]
                    (let [result (morph-ps (time (binding [babel.generate/println? false]
                                                        babel.generate/truncate? false
                                                   (wait wait-ms-for-generation
                                                      (fn []
                                                        (generate spec model))))))]
                      (or (and (not (empty? result)) result)
                          "TIMEOUT.")))))))

(defn create-the-static-tree []
  (-> (-> model :grammar-map :sentence-phrasal-head)
      (u/assoc-in [:head]
                  (-> model :grammar-map :transitive-vp-nonphrasal-head))
      (u/assoc-in [:comp]
                  (-> model :grammar-map :noun-phrase))
      (u/assoc-in [:comp :head]
                  (-> model :grammar-map :nbar))
      (u/assoc-in [:head :comp]
                  (-> model :grammar-map :noun-phrase))
      (u/assoc-in [:head :comp :head]
                  (-> model :grammar-map :nbar))))

(def the-static-tree (create-the-static-tree))

(def lexemes
  (flatten (vals (:lexicon model))))

(def default-fn (:default-fn model))

(defn basecamp []
  (let [tree the-static-tree]
    (-> tree
        (basecamp-at [:head :head] (first (take 1 (shuffle ["see"]))))
        (u/dissoc-paths [[:head :head]])
        (basecamp-at [:head :comp :head :head] (first (take 1 (shuffle ["game" "word" "book"]))))
        (u/dissoc-paths [[:head :comp :head :head]])
        (basecamp-at [:head :comp :head :comp] (first (take 1 (shuffle ["new" "intelligent"]))))
        (u/dissoc-paths [[:head :comp :head]])
        (basecamp-at [:head :comp :comp] "the")
        (u/dissoc-paths [[:head]])
        (basecamp-at [:comp :head :head] (first (take 1 (shuffle ["man" "woman"]))))
        (u/dissoc-paths [[:comp :head :head]])
        (basecamp-at [:comp :head :comp] (first (take 1 (shuffle ["tall" "small" "old"]))))
        (u/dissoc-paths [[:comp :head]])
        (basecamp-at [:comp :comp] "the")
        (u/dissoc-paths [[:comp]]))))
    
(defn basecamp-at [tree path lexeme-surface-form]
;;  (println (str "@" path))
  (first (->> (shuffle (filter #(= lexeme-surface-form
                                   (u/get-in % [:english :english]))
                               lexemes))
              (map (fn [lexeme]
                     (let [tree (u/copy tree)
                           lexeme (u/copy lexeme)]
                        (u/assoc-in! tree path lexeme))))
              (filter #(not (= :fail %)))
              (mapcat (fn [tree]
                        (default-fn tree))))))


(defn nextcamp []
  (let [parse (-> "the small dogs you see" parse first)]
    (pprint (u/get-in parse [:synsem :sem]))))

(defn refresh [& [refresh-lexicon?]]
  (babel.test.test/init-db)
  (if refresh-lexicon? (println (str "wrote: "
                                     (babel.lexiconfn/write-lexicon "en" (babel.english.grammar/compile-lexicon))
                                     " items.")))
  (babel.directory/refresh-models)
  (load "../english"))

(defn get-rule [rule]
  (-> model :grammar-map
      (get (keyword rule))))

(defn rule-at [rule-as-string path]
  (map #(u/get-in % path ::none)
       [(-> model :grammar-map (get (keyword rule-as-string)))]))

(defn parse-at [expression path]
  (map #(u/get-in % path ::none)
       (parse expression)))

(defn lexeme-at [lexeme path]
  (map #(u/get-in % path ::none)
       (-> model :lexicon (get lexeme))))

(defn generate-at [spec path]
  (let [generated (generate spec model)]
    (u/get-in generated path ::none)))

;; (map pprint (assoc-head-at "ditransitive-vp-nonphrasal-head-1" "give" [:synsem :sem]))
(defn assoc-head-at
  "assoc lexeme as the head of the given rule."
  [rule-as-string lexeme path]
  (map #(u/get-in % path ::none)
       (let [rule (-> model :grammar-map (get (keyword rule-as-string)))]
         (->> (map #(u/assoc-in rule [:head] %)
                   (-> model :lexicon (get lexeme)))
              (remove #(= :fail %))))))

;; Thanks for (defn wait) to Christian Meichsner on https://stackoverflow.com/a/30903731 
(defn wait [ms f & args]
  (let [c (chan)]
    (go (>! c (apply f args)))
    (first (alts!! [c (timeout ms)]))))
