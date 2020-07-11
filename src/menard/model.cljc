(ns menard.model
  (:refer-clojure :exclude [load])
  (:require #?(:clj [clojure.tools.logging :as log])
            #?(:cljs [cljslog.core :as log])))

#?(:clj
   (defn load [language-name rules-fn lexicon-fn fill-lexicon-indexes-fn load-morphology-fn load-grammar-fn]
     (log/info (str "loading resources for language: " language-name))
     (let [lexical-rules (lexical-rules-fn)]

       ;; TODO: show count of rules in each set:
       (log/info (str "loaded: " (count lexical-rules) " lexical rule sets. Sizes: "
                      (reduce (fn [a b]
                                (clojure.string/join ", " [a b]))
                              (map count lexical-rules))))

       (let [lexicon (lexicon-fn lexical-rules)]
         (log/info (str "loaded: " (count (keys lexicon)) " lexeme keys."))
         (let [indices (fill-lexicon-indexes-fn lexicon)]
           (log/info (str "loaded: " (count (keys indices)) " lexicon indices."))
           (let [morphology (load-morphology-fn)]
             (log/info (str "loaded: " (count morphology) " morphological rules."))
             (let [grammar (load-grammar-fn)]
               (log/info (str "loaded: " (count grammar) " grammar rules."))
               {:grammar grammar
                :loaded-when (str (java.util.Date.))
                :language language-name
                :morphology morphology
                :rules rules
                :lexicon lexicon
                :indices indices})))))))
   
   #?(:cljs
      (defn load [language-name rules-fn lexicon-fn fill-lexicon-indexes-fn load-morphology-fn load-grammar-fn]
        (log/error (str "should never get here! use compiled linguistic resources. Clojurescript does not have access to the filesystem (at least running within web agents)"))))

 

