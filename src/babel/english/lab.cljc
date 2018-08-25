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

(defn basecamp []
  ;; TODO: improve performance by using {:mod {:first {:obj :modified}}.
  (let [specs
        [
         {:synsem {:cat :verb :subcat []}
          :comp {:phrasal true :rule "noun-phrase"
                 :head {:rule "nbar-s-obj"
                        :head {:rule "nbar"}
                        :comp {:rule "s/obj"}}
                 :synsem {:sem {:mod {:first {:pred :eat}
                                      :rest {:first {:pred :red
                                                     :rest []}}}}}}}
         {:synsem {:cat :verb :subcat []}
          :comp {:phrasal true :rule "noun-phrase"
                 :head {:rule "nbar-s-obj"}
                 :synsem {:sem {:mod {:first {:pred :see}
                                      :rest {:first {:pred :tall
                                                     :rest []}}}}}}}]]
    (repeatedly #(println
                  (let [spec (first (shuffle specs))]
                    (let [result (morph (time (binding [babel.generate/truncate? true
                                                        babel.generate/use-spec-cache? true]
                                                (wait wait-ms-for-generation (fn [] (generate spec model))))))]
                      (or (and (not (empty? result)) result)
                          "TIMEOUT.")))))))

(defn nextcamp []
  (let [parse (-> "the small dogs you see" parse first)]
    (pprint (u/get-in parse [:synsem :sem]))))

(defn refresh [& [refresh-lexicon? rebuild-lexicon?]]
  (let [refresh-lexicon? (or refresh-lexicon? rebuild-lexicon? false)]
    (babel.test.test/init-db)
    (if rebuild-lexicon? (println (str "wrote: "
                                       (babel.lexiconfn/write-lexicon "en" (babel.english.grammar/compile-lexicon))
                                       " items.")))
    (if refresh-lexicon? (babel.lexiconfn/write-lexicon "en" (babel.english.grammar/compile-lexicon)))
    (babel.directory/refresh-models)
    (load "../english")))

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
