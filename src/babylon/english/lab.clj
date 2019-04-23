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
       (map #(u/assoc-in tree at %))
       (map #(set-done % at))))


;; 1. build unfolded trees:
;;
;;    s
;;   / \ 
;;  /   \ H
;;  _    vp-aux
;;      /   \
;;     / H   vp
;;   would  / \
;;         /   \
;;      H /     \
;;      see      _
;;
(defn create-bolts []
  (->> grammar
       (filter #(= (:rule %) "s"))
       (mapcat #(add-rule-at % "vp-aux" [:head]))
       (mapcat #(add-lexeme-at % "would" [:head :head]))
       (mapcat #(add-rule-at % "vp" [:head :comp]))
       (mapcat #(add-lexeme-at % "see" [:head :comp :head]))
       (filter #(not (= % :fail)))))

;; 2. fold up tree from the above representation to:
;;    s
;;   / \
;;  /   \ H
;;  _    vp-aux
;;      /      \
;;     / H      \
;;    would see  _
;;
(defn fold-up [tree at]
  (let [subtree (u/get-in tree at)]
    (log/info (str "swap head.."))
    (swap! (get subtree :head)
           (fn [head]
             {:surface (str
                        (clojure.string/join " " [(morph head)
                                                  (morph (u/get-in subtree [:comp :head]))])
                        " ..")
              :babylon.generate/done? true
              ;; TODO: needs to be sensitive to the orderedness of the children:
              ;; currently assumes that head is first (:head == :1,:comp == :2)
              ;; but need to also check and handle (:comp == :1,:head == :2)
              :syntax-tree (str (syntax-tree head)
                                " .["
                                (u/get-in subtree [:comp :rule])
                                " "
                                "*^"
                                (syntax-tree (u/get-in subtree [:comp :head]))
                                " ")

              :sem (u/get-in head [:sem])
              :subcat {:1 (u/get-in head [:subcat :1])
                       :2 (u/get-in subtree [:comp :head :subcat :2])
                       :3 []}}))
    (log/info (str "swap comp.."))
    (swap! (get subtree :comp)
           (fn [comp] (u/get-in comp [:comp])))
    ;; we have to reach into the internals of dag_unify to
    ;; to remove the now-invalid cached serialization of
    ;; each tree:
    (dissoc tree :dag_unify.serialization/serialized)))

;; 3. add complement at path [:head :comp]:
;;    s
;;   / \
;;  /   \ H
;;  _     vp-aux
;;       /      \
;;      / H      \
;;    would see   <new complement>
;;
(defn add-lower-comp [tree]
  (log/info (str "adding lower comp at: " (g/frontier tree)))
  (-> tree
      (u/assoc-in (g/frontier tree) (first (analyze "her")))))

(defn working-example []
  (->>
   (create-bolts)
   (remove #(= % :fail))
   (map #(fold-up % [:head]))
   (remove #(= % :fail))
   (map add-lower-comp)
   (remove #(= % :fail))
   (map syntax-tree)))

;;(repeatedly #(println (syntax-tree (first (working-example)))))
