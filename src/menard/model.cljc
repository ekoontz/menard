(ns menard.model
  (:refer-clojure :exclude [load])
  (:require #?(:clj [clojure.tools.logging :as log])
            #?(:cljs [cljslog.core :as log])))

#?(:clj
   (defn use-path [path]
     (if (System/getenv "MODEL_URL")
       (str (System/getenv "MODEL_URL") path)
       path)))

#?(:clj
   (defn load [existing-model language-name lexical-rules-fn lexicon-fn fill-lexicon-indexes-fn load-morphology-fn load-grammar-fn]
     (ref-set
      existing-model
      (do
        (log/info (str "loading resources for language: "
                       language-name))
        (let [lexical-rules (lexical-rules-fn)]
          
          ;; TODO: show count of rules in each set:
          (log/info (str "loaded: " (count lexical-rules) " lexical rule sets; sizes: "
                         (reduce (fn [a b]
                                   (clojure.string/join ", " [a b]))
                                 (map count lexical-rules)) "."))
          
          (let [lexicon (lexicon-fn lexical-rules)]
            (log/info (str "loaded: " (count (keys lexicon)) " lexeme keys."))
            (let [indices (fill-lexicon-indexes-fn lexicon)]
              (log/info (str "loaded: " (count (keys indices)) " lexicon indices."))
              (let [morphology (load-morphology-fn)]
                (log/info (str "loaded: " (count morphology) " morphological rules."))
                (let [grammar (load-grammar-fn)]
                  (log/info (str "loaded: " (count grammar) " grammar rules."))
                  (let [retval
                        {:grammar grammar
                         :loaded-when (.getTime (java.util.Date.))
                         :language language-name
                         :morphology morphology
                         :lexicon lexicon
                         ;; note that we don't save the lexical rules since they
                         ;; are only used to compile the lexicon, and so after that's done,
                         ;; the lexicon is what is saved, but the rules used for it
                         ;; aren't needed to be saved in the ref.
                         :indices indices}]
                    (log/info (str "loaded resources for language: " language-name))
                    retval))))))))))
   
#?(:cljs
      (defn load [language-name rules-fn lexicon-fn fill-lexicon-indexes-fn load-morphology-fn load-grammar-fn]
        (log/error (str "should never get here! use compiled linguistic resources. Clojurescript does not have access to the filesystem (at least if running within a web browser)."))))

 

