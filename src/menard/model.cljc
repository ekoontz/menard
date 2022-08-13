(ns menard.model
  (:refer-clojure :exclude [load])
  (:require [babashka.fs :as fs]
            #?(:clj [clojure.java.io :as io :refer [resource]])
            #?(:clj [clojure.tools.logging :as log])
            [clojure.string :as string]
            #?(:cljs [cljslog.core :as log])
            [menard.lexiconfn :as l]))

#?(:clj
   (defn use-path [path]
     (if (System/getenv "MODEL_URL")
       (str (System/getenv "MODEL_URL") path)
       (str "" path))))

#?(:clj
   (defn load [language-name lexical-rules-fn lexicon-fn fill-lexicon-indexes-fn
               load-morphology-fn load-grammar-fn model-spec]
     (log/info (str "loading resources for language: "
                    language-name "; model-spec name: " (:name model-spec)))
     (let [logging-label (str language-name "/" (:name model-spec))
           lexical-rules (lexical-rules-fn)
           lexicon (lexicon-fn lexical-rules)
           indices (fill-lexicon-indexes-fn lexicon)
           morphology (load-morphology-fn)
           grammar (load-grammar-fn)]
       (log/info (str logging-label " loaded: " (count lexical-rules) " lexical rules."))
       (log/info (str logging-label " loaded: " (count (keys lexicon)) " lexeme keys."))
       (log/info (str logging-label " loaded: " (count (keys indices)) " lexicon indices."))
       (log/info (str logging-label " loaded: " (count morphology) " morphological rules."))
       (log/info (str logging-label " loaded: " (count grammar) " grammar rules."))
       (log/info (str logging-label " loaded resources for language: " language-name "."))
       {:grammar grammar
        :loaded-when (.getTime (java.util.Date.))
        :language language-name
        :morphology morphology
        :lexicon lexicon
        ;; note that we don't save the lexical rules since they
        ;; are only used to compile the lexicon, and so after that's done,
        ;; the lexicon is what is saved, but the rules used for it
        ;; aren't needed to be saved.
        :indices indices})))

#?(:cljs
   (defn load [language-name rules-fn lexicon-fn fill-lexicon-indexes-fn
               load-morphology-fn load-grammar-fn]
        (log/error (str "should never get here! use compiled linguistic resources."
                        "Clojurescript does not have access to the filesystem "
                        "(at least if running within a web browser)."))))
#?(:clj
   (defn current-ms [] (.getTime (new java.util.Date))))
#?(:clj
   ;; time in milliseconds that the most-recently-modified file was modified:
   (defn latest-file-timestamp [top-of-resources]
     (. (->> (fs/glob top-of-resources "**{.edn}")
             (map #(fs/get-attribute % "lastModifiedTime"))
             sort reverse first)
        toMillis)))

#?(:clj

   (defn file-info [top-of-resources pattern]
     (->> (fs/glob top-of-resources pattern)
          (map
           (fn [file]
             {:last-modified-time-ms
              (. (fs/get-attribute file "lastModifiedTime")
                 toMillis)
              :last-modified-time
              (fs/get-attribute file "lastModifiedTime")
              :parent
              (-> file .getParent str)
              :filename
              (-> file .getFileName str)}))))
   
   )

#?(:clj
   (defn get-info-of-files [top-of-resources pattern]
     (->> (file-info top-of-resources pattern)))
   )

#?(:clj
   ;; time in milliseconds that the most-recently-modified file
   ;; in the top-level resources/ directory was modified:
   (defn latest-file-timestamp-depth-one [top-of-resources]
     (. (->> (fs/glob top-of-resources "*{.edn}")
             (map #(fs/get-attribute % "lastModifiedTime"))
             sort reverse first)
        toMillis)))

#?(:clj
   (defn read-model-spec [model-spec-filename]
     (-> model-spec-filename
         ((fn [filename]
            (if (re-find #"^file:///" filename)
              (do
                (log/info (str "got a file:// filename: " filename ": using filesystem."))
                filename)

              ;; else, assume it's a relative path, in which case we
              ;; we have to "cast" the filename to an io/resource,
              ;; which uses the JVM classpath, not the local filesystem, for relative paths
              (do
                (log/info (str "got a non-file:// filename: " filename ": using resource."))
                (io/resource filename)))))
         slurp
         read-string)))

#?(:clj
   (defn create [path-to-model
                 load-morphology-fn
                 load-lexicon-with-morphology-fn
                 load-lexicon-fn
                 load-grammar-fn
                 lexicon-index-fn
                 fill-lexicon-indexes-fn
                 ]
     (let [model-spec-filename 
           (str path-to-model ".edn")]
       (log/info (str "creating model with "
                      "filename: " model-spec-filename " .."))
       (let [model-spec (read-model-spec model-spec-filename)
             lexical-rules-path (-> model-spec :lexicon :rules)
             lexical-rules (l/read-and-eval (use-path lexical-rules-path))
             morphology (load-morphology-fn)
             filter-lexicon-fn (or (-> model-spec :lexicon :filter-fn eval)
                                   (fn [lexicon] lexicon))
             ;; apply those lexical rules
             ;; to a source lexicon to create
             ;; compile lexicon:
             lexicon (load-lexicon-with-morphology-fn
                      (load-lexicon-fn lexical-rules model-spec)
                      morphology
                      filter-lexicon-fn)

             grammar (load-grammar-fn)]
         (log/info (str "create: grammar for "
                        "'" model-spec-filename "'"
                        " has this many rules: " (count grammar)))
         (->
          (load "nl"
                ;; loads the lexical rules:
                ;; (we already did this above,
                ;;  so we'll just return those rules.
              (fn [] lexical-rules)
              
              ;; function to load the lexicon:
              (fn [_] lexicon)
              
              ;; create indices on the compiled lexicon:
              fill-lexicon-indexes-fn
              
              ;; function to load the morphology:
              (fn [] morphology)
              
              (fn [] grammar)
              model-spec)
        ((fn [model]
           (merge model
                  {:name model-spec-filename
                   :spec model-spec
                   :lexicon-index-fn (lexicon-index-fn model)}))))))))




     
