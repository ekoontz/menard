(ns babel.reader.lab
  (:require
   [babel.directory :refer [models]] ;; this is needed even though there are no references to directory in here.
   [babel.generate :as g :refer [frontier generate get-lexemes]]
   [babel.reader :refer [generate-question-and-correct-set]]
   [clojure.core.async :refer [>! alts!! timeout chan go]]
   #?(:cljs [babel.logjs :as log])
   #?(:clj [clojure.tools.logging :as log])
   #?(:clj [clojure.repl :refer [doc]])
   [dag_unify.core :as u :refer [pprint strip-refs unify]]))


(defn basecamp []
  (repeatedly
   #(time
     (println (generate-question-and-correct-set 
               {:root {:italiano {:italiano "chiamarsi"}
                       :synsem {:subcat {:1 {:top :top}
                                         :2 {:top :top} :3 {:top :top}}
                                :cat :verb :sem {:tense :present :aspect :simple}}}
                :synsem {:subcat []}
                :comp {:phrasal false :synsem {:agr {:number :sing
                                                     :person :1st}}}
                "en"
                "US"
                "it"
                "IT"})))))

(def wtf (generate-question-and-correct-set 
          {:root {:italiano {:italiano "chiamarsi"}
                  :synsem {:subcat {:1 {:top :top}
                                    :2 {:top :top} :3 {:top :top}}
                           :cat :verb :sem {:tense :present :aspect :simple}}}
           :synsem {:subcat []}
           :comp {:phrasal false :synsem {:agr {:number :sing
                                                :person :1st}}}}
          "en"
          "US"
          "it"
          "IT"))

(defn wtf2 []
  (generate-question-and-correct-set 
   {:root {:italiano {:italiano "chiamarsi"}
           :synsem {:subcat {:1 {:top :top}
                             :2 {:top :top} :3 {:top :top}}
                    :cat :verb :sem {:tense :present :aspect :simple}}}
    :synsem {:subcat []}
    :comp {:phrasal false :synsem {:agr {:number :sing
                                         :person :1st}}}}
   "en"
   "US"
   "it"
   "IT"))



