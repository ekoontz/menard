(ns babylon.english.lab
  (:require
   [babylon.english :as en :refer [analyze generate grammar morph parse syntax-tree]]
   [babylon.generate :as g]
   [dag_unify.core :as u :refer [unify fail? ref? simplify-ref]]
   [dag_unify.serialization :as s :refer [all-refs]]
   [clojure.tools.logging :as log]))

(def specs
  [{:phrasal true
    :rule "np"
    :head {:rule "nbar4"
           :phrasal true
           :comp {:phrasal true
                  :comp {:phrasal true
                         :comp {:phrasal true}}}}}
   {:phrasal true
    :rule "s"
    :comp {:phrasal true
           :agr {:number :sing}
           :sem {:pred :dog}}
    :canonical "be"}])

(defn gen
  "how to generate a phrase with particular constraints."
  [i]
  (let [expression (generate (nth specs i))]
      {:st (syntax-tree expression)
       :morph (morph expression)
       :agr (u/strip-refs (u/get-in expression [:head :agr]))
       ;;       :parses (map syntax-tree (parse (morph expression)))
       :phrase? (u/get-in expression [:head :comp :phrasal])}))

(def specific-sentence-spec
  [{:cat :verb
    :subcat []
    :pred :top
    :comp {:phrasal true
           :rule "np"
           :agr {:number :sing
                 :person :3rd}
           :head {:phrasal true
                  :rule "nbar2"
                  :head {:canonical "dog"}}}
    :head {:phrasal false
           :canonical "be"}}])

(def poetry-specs
  [
   {:cat :verb
    :phrasal true
    :subcat []
    :comp {:phrasal false}
    :head {:phrasal true
           :comp {:phrasal true}}}])

(defn poetry-line []
  (try
    (->
     poetry-specs
     shuffle
     first
     generate)
    (catch Exception e
      (log/warn (str "fail:(" (-> e ex-data :why) ":)"
                     (syntax-tree (:tree (ex-data e))) " with spec:"
                     (u/strip-refs (:child-spec (ex-data e))) "; at path:"
                     (:frontier-path (ex-data e)) "; immediate-parent: "
                     (-> e ex-data :immediate-parent))))))

(defn benchmark []
  (repeatedly
   #(time (->
           (or (poetry-line) "(failed)")
           (morph :sentence-punctuation? true)
           println))))

(defn poetry []
  (loop []
    (println (morph (or (poetry-line) "(failed)") :sentence-punctuation? true))
    (recur)))

(defn long-s []
  (count
   (take 1
    (repeatedly
      #(println
         (morph
          (generate
           {:cat :verb
            :subcat []
            :pred :top
            :comp {:phrasal true
                   :head {:phrasal true}}
            :head {:phrasal true
                   :comp {:phrasal true
                          :head {:phrasal true}}}})))))))

(defn set-started [tree path]
  (if (not (empty? path))
    (set-started
     (u/assoc-in tree (concat path [:babylon.generate/started?]) true)
     (rest path))
    (u/assoc-in tree [:babylon.generate/started?] true)))

(defn set-done [tree path]
  (if (and (not (empty? path))
           (= :comp (last path)))
      (set-done
       (u/assoc-in tree (concat path [:babylon.generate/done?]) true)
       (butlast path))
    (u/assoc-in tree (concat path [:babylon.generate/done?]) true)))

(defn add-rule-at [tree rule-name at]
  (->> grammar
       (filter #(= (:rule %) rule-name))
       (map #(u/assoc-in tree at %))
       (map #(set-started % at))))

(defn add-lexeme-at [tree surface at]
  (->> (analyze surface)
       (map (fn [lexeme]
              (log/debug (str "lexeme: " (u/fail-path (u/get-in tree at)
                                                      lexeme)))
              (u/assoc-in tree at lexeme)))
       (map #(set-done % at))))

(def skels
  (->> grammar
       (filter #(= (:rule %) "s"))
       (mapcat #(add-rule-at % "vp-aux" [:head]))
       (mapcat #(add-lexeme-at % "would" [:head :head]))
       (mapcat #(add-rule-at % "vp" [:head :comp]))
       (mapcat #(add-lexeme-at % "see" [:head :comp :head]))
       (filter #(not (= % :fail)))))

(def skel2
  (->> grammar
       (filter #(= (:rule %) "s"))
       (mapcat #(add-rule-at % "vp-aux" [:head]))
       (mapcat #(add-lexeme-at % "would" [:head :head]))
       (mapcat #(add-rule-at % "vp" [:head :comp]))
       (mapcat #(add-lexeme-at % "see" [:head :comp :head]))
       (mapcat #(add-lexeme-at % "her" [:head :comp :comp]))
       (filter #(not (= % :fail)))
       first))

(def merge-with-keys)

(defn unify!
  "destructively merge arguments, where arguments are maps possibly containing references, so that 
   sharing relationship in the arguments is preserved in the result"
  ([val1]
   val1)
  
  ([val1 val2 & rest-args]
   (cond
     ;; This is the canonical unification case: unifying two DAGs
     ;; (maps with possible references within them).
     ;;
     (and (map? val1)
          (map? val2))
     (let [result (merge-with-keys
                   (reduce dissoc val1 dag_unify.serialization/*exclude-keys*)
                   (reduce dissoc val2 dag_unify.serialization/*exclude-keys*)
                   (filter #(not (contains? dag_unify.serialization/*exclude-keys*
                                            %)) ;; TODO: rather than filter, simply get keys from dissoc'ed val1 (above)
                           (keys val1)))]
       (if (empty? rest-args)
         result
         (unify! result
                 (apply unify! rest-args))))
     
     (or (= val1 :fail)
         (= val2 :fail))
     :fail
     
     (and (= val1 :top)
          (empty? rest-args))
     val2
     
     (= val1 :top)
     (apply unify! (cons val2 rest-args))
     
     (and (= val2 :top)
          (not (empty? rest-args)))
     (apply unify! (cons val1 rest-args))
     
     (= val2 :top) val1
     
     ;; expensive if val1 and val2 are not atomic values: the above
     ;; checks should ensure that by now val1 and val2 are atomic.
     (= val1 val2) val1
     
     ;; val1 is a ref, val2 is not a ref.
     (and
      (ref? val1)
      (not (ref? val2)))
     (do
       (cond
         (contains? (set (all-refs val2)) val1)
         :fail ;; cannot unify these because it would create a cycle.
         
         true
         (do (swap! val1
                    (fn [x] (unify! @val1 val2)))
             val1)))
     
     ;; val2 is a ref, val1 is not a ref.
     (and
      (ref? val2)
      (not (ref? val1)))
     (do
       (cond
         (contains? (set (all-refs val1)) val2)
         :fail
         true
         (do
           (swap! val2
                  (fn [x] (unify! val1 @val2)))
           val2)))
     
     (= val1 '())
     :fail
     
     (= val1 nil)
     :fail
     
     (and
      (ref? val1)
      (ref? val2))
     (cond
       (= (simplify-ref val1)
          (simplify-ref val2))
       val1
       
       (or (contains? (set (all-refs @val1)) val2)
           (contains? (set (all-refs @val2)) val1))
       :fail
       
       (= @val1 val2) ;; val1 -> val2
       val2
       
       :else
       (do
         (swap! val1
                (fn [x] (unify! @val1 @val2)))
         (swap! val2
                (fn [x] val1)) ;; note that now val2 is a ref to a ref.
         val1))
     
     ;; convoluted way of expressing: "if val1 has the form: {:not X}, then .."
     (not (= :notfound (:not val1 :notfound)))
     (if (= val2 :top)
       val1
       ;; else
       (let [result (unify! (:not val1) val2)]
         (if (= result :fail)
           val2
           :fail)))
     
     ;; convoluted way of expressing: "if val2 has the form: {:not X}, then .."
     (not (= :notfound (:not val2 :notfound)))
     (if (= val1 :top)
       val2
       (let [result (unify! val1 (:not val2))]
         (if (= result :fail)
           val1
           :fail)))
     
     :else
     :fail)))

(defn merge-with-keys [arg1 arg2 keys-of-arg1]
  (log/info (str "MWK:" (vec keys-of-arg1)))
  (loop [arg1 arg1 arg2 arg2 keys-of-arg1 keys-of-arg1]
    (let [key1 (first keys-of-arg1)
          val1 (key1 arg1 :top)
          val2 (key1 arg2 :top)
          result (if (not (empty? keys-of-arg1))
                   (unify! val1
                           val2))]
      (log/info (str "KEY:" key1))
      (log/info (str "TYPE OF RESULT WITH KEY: " (type result)))
      (if (= clojure.lang.Atom (type val1))
        (log/info (str "val1 atom: " @val1))
        (log/info (str "val1: " val1)))
      (if (= clojure.lang.Atom (type val2))
        (log/info (str "val2 atom: " @val2))
        (log/info (str "val2: " val2)))
      (cond

        ;; if keys-of-arg1 is empty, then arg2 contains only keys that
        ;; were *not* in arg1.
        (empty? keys-of-arg1) arg2

        ;; TODO: consider using: (= :fail result) rather than (expensive) (fail?).
        (fail? result)
        (do
          (if true (log/info (str "FAILED! on key=" key1)))
          :fail)
        true (recur arg1
                    (clojure.core/merge
                     {key1 result}
                     (dissoc arg2 key1))
                    (rest keys-of-arg1))))))

(defn fold-up [tree path]
  (let [tree
        (->
         tree
         (dissoc :dag_unify.serialization/serialized)
         (u/get-in path))] ;; <- descend tree to the subtree to be operated on.
    (swap! (get tree :head)
           (fn [x]
             {:surface (str
                        (clojure.string/join " " [(morph (u/get-in tree [:head]))
                                                  (morph (u/get-in tree [:comp :head]))])
                        " ..")
              :babylon.generate/done? true
              :syntax-tree (syntax-tree (u/get-in tree [:head]))
              :sem (u/get-in tree [:head :sem])
              :subcat {:1 (u/get-in tree [:head :subcat :1])
                       :2 (u/get-in tree [:comp :head :subcat :2])
                       :3 []}}))
    (swap! (get tree :comp)
           (fn [x] (u/get-in tree [:comp :comp])))))

(defn do-it [skels]
  (loop [skels skels]
    (when (not (empty? skels))
      (fold-up (first skels) [:head])
      (recur (rest skels)))))

(defn do-it-more []
  (do-it skels)
  (->> skels
       (map #(u/assoc-in % [:head :comp] (first (analyze "her"))))
       (filter #(not (= :fail %)))))

;; fails if (dag_unify.serialization/serialized) uses :serialized.
(def ok (do-it-more))
