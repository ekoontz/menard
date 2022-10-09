(ns menard.model
  (:refer-clojure :exclude [load])
  (:require [babashka.fs :as fs]
            [config.core :refer [env]]
            #?(:clj [clojure.java.io :as io :refer [resource]])
            #?(:clj [clojure.tools.logging :as log])
            [clojure.string :as string]
            #?(:cljs [cljslog.core :as log])
            [dag_unify.core :as u :refer [unify]]
            [dag_unify.diagnostics :as diag]
            [menard.exception :refer [exception]]
            [menard.grammar :as grammar]
            [menard.lexiconfn :as l]
            [menard.morphology :as m]
            [menard.nederlands.tenses]
            [menard.nesting]
            [menard.serialization :as s]
            [menard.subcat]
            [menard.ug]))

;; TODO: use path/use-path
#?(:clj
   (defn use-path [path]
     (if (System/getenv "MODEL_URL")
       (str (System/getenv "MODEL_URL") path)
       (str "" path))))

#?(:clj
   (declare fill-lexicon-indexes))

#?(:clj
   (defn load [language-name lexical-rules-fn lexicon-fn
               load-morphology-fn load-grammar-fn model-spec]
     (log/info (str "loading resources for language: "
                    language-name "; model-spec name: " (:name model-spec)))
     (let [logging-label (str language-name "/" (:name model-spec))
           lexical-rules (lexical-rules-fn)
           lexicon (lexicon-fn lexical-rules)
           indices (fill-lexicon-indexes lexicon)
           morphology (load-morphology-fn)
           grammar (load-grammar-fn)]
       (log/info (str logging-label " loaded: " (count lexical-rules) " lexical rules."))
       (log/info (str logging-label " loaded: " (count (keys lexicon)) " lexeme keys."))
       (if (empty? (keys lexicon))
         (exception (str "lexicon was empty for model with spec: " model-spec)))
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
   (defn load [language-name rules-fn lexicon-fn
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
   (defn load-morphology [path-prefix source-files]
     (log/info (str "load-morphology: path-prefix: " path-prefix
                    "; source-files: " (vec source-files)))
     (m/compile-morphology-fn
      (map (fn [source-file]
             (use-path (str path-prefix "/" source-file)))
           source-files))))

#?(:clj
   (defn load-grammar-from-file [path]
     (-> (use-path path)
         grammar/read-grammar-fn
         grammar/process)))

#?(:clj
   (defn load-grammar [spec]
     (load-grammar-from-file (-> spec :grammar))))

#?(:clj
  (defn fill-lexicon-indexes [lexicon]
    (let [flattened-lexicon (flatten (vals lexicon))]
      {:adjective-lexicon
       (->> flattened-lexicon
            (filter #(and (not (u/get-in % [:exception]))
                          (= (u/get-in % [:cat]) :adjective))))
       :det-lexicon
       (->> flattened-lexicon
            (filter #(and (not (u/get-in % [:exception]))
                          (= (u/get-in % [:cat]) :det))))
       :noun-lexicon
       (->> flattened-lexicon
            (filter #(and (not (u/get-in % [:exception]))
                          (= (u/get-in % [:cat]) :noun))))
       :misc-lexicon
       (->> flattened-lexicon
            (filter #(and (not (= (u/get-in % [:cat]) :verb))
                          (not (= (u/get-in % [:cat]) :adjective))
                          (not (= (u/get-in % [:cat]) :det))
                          (not (= (u/get-in % [:cat]) :noun))
                          (not (u/get-in % [:exception])))))
       :verb-lexicon
       (->> flattened-lexicon
            (filter #(and (not (u/get-in % [:exception]))
                          (= (u/get-in % [:cat]) :verb))))})))

(def include-derivation? false)

#?(:clj
   (defn compile-lexicon-source [source-filename lexical-rules & [unify-with apply-fn]]
     (log/debug (str "compile-lexicon-source start: '" source-filename "'"))
     (binding [menard.lexiconfn/include-derivation? include-derivation?]
       (-> source-filename
           l/read-and-eval
           ((fn [lexicon]
              (if (nil? unify-with)
                lexicon
                (do
                  (log/debug (str "  apply-to-every-lexeme..(unify-with)"))
                  (l/apply-to-every-lexeme lexicon
                                           (fn [lexeme]
                                             (let [result (unify lexeme unify-with)]
                                               (if (= :fail result)
                                                 (exception (str "hit a fail while processing source filename: " source-filename "; lexeme: " lexeme "; unify-with: " unify-with)))
                                               result)))))))
           ((fn [lexicon]
              (if (nil? apply-fn)
                lexicon
                (do
                  (log/debug (str "  apply-to-every-lexeme (apply-fn)"))
                  (l/apply-to-every-lexeme lexicon
                                           (fn [lexeme]
                                             (apply-fn lexeme)))))))
           ((fn [lexicon]
              (log/debug (str "  add-exceptions-to-lexicon.."))
              (l/add-exceptions-to-lexicon lexicon)))
           ((fn [lexicon]
              (log/debug (str "  apply-rules-in-order.."))
              (l/apply-rules-in-order lexicon lexical-rules)))
           ((fn [lexicon]
              (log/info (str "compile-lexicon-source compiled: '" source-filename "'."))
              lexicon))))))

#?(:clj
   (defn load-lexicon [lexical-rules model-spec & [path-suffix]]
     (log/info (str "load-lexicon: lexical-rules count): " (count lexical-rules)))
     (if (nil? model-spec)
       (exception "model-spec is unexpectedly nil."))
     (log/debug (str "model-spec: " model-spec))
     (let [path-suffix (or path-suffix (-> model-spec :lexicon :path))]
       (log/info (str "lexicon filenames: " (-> model-spec :lexicon :sources keys sort)))

       (reduce (fn [a b]
                 (merge-with concat a b))
               (->> (-> model-spec :lexicon :sources keys sort)
                    (map (fn [filename]
                           (let [unify-with
                                 (get
                                  (get (-> model-spec :lexicon :sources)
                                       filename)
                                 :u :top)
                                 postprocess-fn
                                 ;; have to (eval) twice for some reason..
                                 (eval (eval (get (get (-> model-spec :lexicon :sources)
                                                       filename)
                                                  :f '(fn [x] x))))]
                             (log/debug (str "source filename: " filename))
                             (log/debug (str "unify-with: " unify-with))
                             (compile-lexicon-source
                              (use-path (str path-suffix "/" filename))
                              lexical-rules
                                    
                              unify-with
                              
                              postprocess-fn)))))))))

(def filter-for-fails? false)
(def filter-out-nils? false)

#?(:clj
   (defn lexicon-index-fn [model]
     (fn [spec]
       (log/debug (str "spec: " (diag/strip-refs spec)))
       (let [pre-result
             (cond (= (u/get-in spec [:cat]) :verb)
                   (-> model :indices :verb-lexicon)
                   
                   (= (u/get-in spec [:cat]) :adjective)
                   (-> model :indices :adjective-lexicon)
                   
                   (= (u/get-in spec [:cat]) :noun)
                   (-> model :indices :noun-lexicon)
                   
                   (= (u/get-in spec [:cat]) :det)
                   (-> model :indices :det-lexicon)
                   
                   :else (-> model :indices :misc-lexicon))
             result (if (false? filter-for-fails?)
                      (->>
                       pre-result
                       (filter #(or (false? filter-out-nils?)
                                    (not (true? (u/get-in % [:null?]))))))
                      (->> pre-result
                           (filter #(or (false? filter-out-nils?)
                                        (not (true? (u/get-in % [:null?])))))
                           (map #(unify % spec))
                           (filter #(not (= :fail %)))))]
         (if true
           (shuffle result)
           result)))))

#?(:clj
   (defn add-functions [model]
     (-> model
         (merge
          {:lexicon-index-fn (lexicon-index-fn model)
           :syntax-tree-fn (fn [tree]
                             (s/syntax-tree tree (:morphology model)))
           :morph-fn (fn [tree]
                       (s/morph tree (:morphology model)))}))))

#?(:clj
   (defn create [path-to-model
                 name
                 compile-lexicon-fn]
     (let [model-spec-filename 
           (str path-to-model ".edn")]
       (log/info (str "creating model with "
                      "filename: " model-spec-filename " .."))
       (let [model-spec (read-model-spec model-spec-filename)
             lexical-rules-path (str
                                 (-> model-spec :lexicon :path) "/"
                                 (-> model-spec :lexicon :rules))
             lexical-rules (l/read-and-eval (use-path lexical-rules-path))
             morphology (load-morphology (-> model-spec :morphology :path)
                                         (-> model-spec :morphology :sources))
             filter-lexicon-fn (or (-> model-spec :lexicon :filter-fn eval eval)
                                   (fn [lexicon] lexicon))
             ;; apply those lexical rules
             ;; to a source lexicon to create
             ;; compile lexicon:
             lexicon (compile-lexicon-fn
                      (load-lexicon lexical-rules model-spec)
                      morphology
                      filter-lexicon-fn)

             grammar (load-grammar model-spec)]
         (log/info (str "create: grammar for "
                        "'" model-spec-filename "'"
                        " has this many rules: " (count grammar)))
         (log/info (str "create: lexicon for "
                        "'" model-spec-filename "'"
                        " has this many lexemes: " (count (keys lexicon))))
         (if (empty? grammar)
           (exception (str "create: grammar for model "
                           "'" model-spec-filename "'"
                           " is empty.")))
         (if (empty? (keys lexicon))
           (exception (str "create: lexicon for model "
                           "'" model-spec-filename "'"
                           " is empty.")))
         (let [retval
               (->
                (load (-> model-spec :language)
                      ;; loads the lexical rules:
                      ;; (we already did this above,
                      ;;  so we'll just return those rules.
                      (fn [] lexical-rules)

                      ;; function to load the lexicon:
                      (fn [_] lexicon)

                      ;; function to load the morphology:
                      (fn [] morphology)

                      (fn [] grammar)
                      model-spec)
                (merge {:name name :spec model-spec})
                (add-functions))]
           (log/info (str "returning model with keys: " (keys retval)))
           retval)))))

#?(:clj
   (defn create-model-from-filesystem [spec compile-lexicon-fn & [use-env]]
    (if (nil? spec)
      (exception (str "create-model-from-filesystem: spec was nil.")))
    (let [env (or use-env env)]
      (log/info (str "create-model-from-filesystem: menard-dir env: " (:menard-dir env)))
      (log/info (str "create-model-from-filesystem: spec: " spec))
      (if (empty? (:menard-dir env))
        (exception (str "you must set MENARD_DIR in your environment.")))
      (let [menard-dir (str (:menard-dir env) "/")]
        (log/debug (str "loading ug.."))
        (menard.ug/load-from-file)
        (log/debug (str "loading nesting.."))
        (menard.nesting/load-from-file)
        (log/debug (str "loading subcat.."))
        (menard.subcat/load-from-file)
        (log/debug (str "loading grammar.."))
        (let [grammar (load-grammar-from-file (str "file://" menard-dir "resources/"
                                                   (-> spec :grammar)))]
          (log/debug (str "loaded " (count grammar) " grammar rules."))
          (log/debug (str "loading morphology.."))
          (let [morphology (load-morphology (str "file://" menard-dir "resources/"
                                                 (-> spec :morphology :path) "/")
                                            (-> spec :morphology :sources))]
            (log/debug (str "loaded " (count morphology) " morphological rules."))
            (log/debug (str "loading lexical rules.."))
            (let [lexical-rules (-> (str "file://" menard-dir "resources/"
                                         (-> spec :lexicon :path) "/"
                                         (-> spec :lexicon :rules))
                                    l/read-and-eval)]
              (log/debug (str "loaded " (count lexical-rules) " lexical rules."))
              (log/info (str "create-model-from-filesystem: loading lexicon with spec: " spec))
              (let [lexicon (compile-lexicon-fn
                             (load-lexicon lexical-rules
                                           spec
                                           (str "file://" menard-dir "resources/"
                                                (-> spec :lexicon :path)))
                             morphology
                             (fn [x] x))]
                (log/debug (str "loaded " (count (keys lexicon)) " lexical keys."))
                (log/debug (str "done loading model."))

                (->
                 (load "nl"
                       (fn [] lexical-rules)
                       (fn [_] lexicon)
                       (fn [] morphology)
                       (fn [] grammar)
                       spec)
                 (merge {:name (-> spec :name) :spec spec})
                 (add-functions))))))))))

#?(:clj
   (defn load-model [model & [reload?]]
      (when (or (nil? @model) (true? reload?))
        (try
          (log/info (str (when @model "re") "loading model: " (:name @model)))
          (let [loaded (create-model-from-filesystem (:spec @model))]
            (dosync
             (ref-set model loaded))
            (log/info (str "loaded model: " (:name @model))))
          (catch Exception e (do
                               (log/info (str "Failed to load model; the error was: '" (str e) "'. Will keep current model as-is and wait 10 seconds and see if it's fixed then."))))))
     (if (nil? @model)
       (log/error (str "load-model: model couldn't be loaded. Tried both built-in jar and filesystem.")))
     @model))


(defn resolve-model [model]
  (cond (= (type model) clojure.lang.Ref) @model
        (map? model)                      model
        :else                             (exception (str "invalid model: " model))))


(defmacro install-the-usual-suspects []
  '(do
     (defn analyze [expression]
       (menard.parse/analyze expression model))
     
     (defn generate [expression]
       (menard.generate/generate expression model))
     
    (defn morph [expression]
      (menard.serialization/morph expression
                           (-> model deref :morphology)))
    
    (defn parse [expression]
      (menard.parse/parse expression model))
    
    (defn syntax-tree [expression]
      (menard.serialization/syntax-tree expression
                                        (-> model deref :morphology)))))
