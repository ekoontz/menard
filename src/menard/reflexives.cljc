(ns menard.reflexives
  (:require #?(:clj [clojure.tools.logging :as log])
            [dag_unify.core :as u :refer [unify]]
            #?(:clj [clojure.java.io :as io :refer [resource]])
            #?(:cljs [cljslog.core :as log])
            [menard.lexiconfn :refer [read-and-eval]]
            [menard.model :refer [use-path]]))

(declare define)

#?(:clj
   (defn load-from-file []
     (with-open [r (io/reader "/Users/ekoontz/menard/resources/reflexives.edn")]
       (eval (read (java.io.PushbackReader. r))))))


(defn load-reflexive-options [& from-file?]
  (intern 'menard.reflexives 'reflexive-options
          (let [retval
                (->> (if from-file?
                       (load-from-file)
                       (read-and-eval (use-path "reflexives.edn")))
                     (remove nil?))]
            (log/info (str "loaded: " (count retval) " reflexive options."))
            retval)))

(load-reflexive-options)






