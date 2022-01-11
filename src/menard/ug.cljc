(ns menard.ug
  (:require
   [dag_unify.core :as u :refer [unify]]
   #?(:clj [clojure.java.io :as io :refer [resource]])
   [menard.lexiconfn :refer [read-and-eval]]
   [menard.model :refer [use-path]]
   #?(:clj [clojure.tools.logging :as log])
   #?(:cljs [cljslog.core :as log])))

#?(:clj
   (defn load-from-file []
     (with-open [r (io/reader "/Users/ekoontz/menard/resources/ug.edn")]
       (eval (read (java.io.PushbackReader. r))))))

#?(:clj
   (defn load-from-jar []
     (-> "ug.edn" use-path read-and-eval)))

(defn define [defs]
  (let [result
        (->> defs
             (map (fn [def]
                    (let [k (:def def)
                          v (-> def (dissoc :unify) (dissoc :def))]
                      (let [unify-with (:unify def)]
                        (let [value (if unify-with
                                      (reduce unify (cons v
                                                          (map eval unify-with)))
                                      v)]
                          (eval `(def ~(symbol k) ~value)))))))
             doall)]
    (log/info (str "loaded: " (count result) " symbols."))))

(define (load-from-jar))


