(ns babel.ug
  (:refer-clojure :exclude [get-in resolve])
  (:require [babel.lexiconfn :refer [apply-default]]
            [clojure.math.combinatorics :as combo]
            [clojure.string :as string]
            #?(:clj [clojure.tools.logging :as log])
            [dag_unify.core :refer [fail? fail-path get-in strip-refs unify]]))

(def phrasal {:phrasal true})

(defn exception [error-string]
  #?(:clj
     (throw (Exception. (str ": " error-string))))
  #?(:cljs
     (throw (js/Error. error-string))))

(defn unify-check [ & vals]
  (let [result (apply unify vals)]
    (if (fail? result)
      (exception (str "failed to unify components of grammar rule: "
                      (first  ;; there may be other failed components, but showing
                       ;; only one should be enough to help the grammarian without
                       ;; overwhelming them.
                       (filter #(not (nil? %))
                               (map (fn [[a b]]
                                      (let [fp (fail-path a b)]
                                        (if (not (nil? fp))
                                          {:path (:fail-path fp)
                                           :a (strip-refs a)
                                           :b (strip-refs b)})))
                                    (filter #(= (count %) 2)
                                            (clojure.math.combinatorics/subsets vals)))))))
      result)))

(defn verb-default? [tree]
  (let [result
        (and (= :verb (get-in tree [:synsem :cat]))
             (or (= :top (get-in tree [:synsem :sem :tense] :top))
                 (= :top (get-in tree [:synsem :infl] :top))))]
    (log/debug (str "verb-default on this tree: => " result))
    result))

(defn apply-default-if [tree test to-apply]
  (if (test tree)
    (apply-default tree to-apply)
    tree))

;;    [1]
;;   /   \
;;  /     \
;; H[1]    C
(def head-principle-no-infl
  (let [head-cat (atom :top)
        head-essere (atom :top)
        head-is-pronoun (atom :top)
        head-sem (atom :top)]
    (unify phrasal
            {:synsem {:cat head-cat
                      :essere head-essere
                      :pronoun head-is-pronoun
                      :sem head-sem}
             :head {:synsem {:cat head-cat
                             :essere head-essere
                             :pronoun head-is-pronoun
                             :sem head-sem}}})))

;;    [1]
;;   /   \
;;  /     \
;; H[1]    C
(def head-principle
  (unify head-principle-no-infl
          phrasal
          (let [head-infl (atom :top)
                agr (atom :top)]
            {:synsem {:infl head-infl
                      :agr agr}
             :head {:synsem {:infl head-infl
                             :agr agr}}})))

;;     subcat<>
;;     /      \
;;    /        \
;; H subcat<1>  C[1]
(def subcat-1-principle
  (let [comp-synsem (atom :top)]
    {:synsem {:subcat '()}
     :head {:synsem {:subcat {:1 comp-synsem
                              :2 '()}}}
     :comp {:synsem comp-synsem}}))

;;     subcat<>
;;     /      \
;;    /        \
;; H subcat<1>  C[1]
(def subcat-1-principle-no-complement-subcat-restrictions
  (let [comp-synsem (atom {:subcat :top})]
    {:synsem {:subcat '()}
     :head {:synsem {:subcat {:1 comp-synsem
                              :2 '()}}}
     :comp {:synsem comp-synsem}}))

;;     subcat<1>
;;     /      \
;;    /        \
;; H subcat<1>  C<>
(def subcat-1-1-principle
  (let [subcat (atom :top)]
    {:synsem {:subcat {:1 subcat
                       :2 '()}}
     :comp {:synsem {:subcat '()}}
     :head {:synsem {:subcat {:1 subcat
                              :2 '()}}}}))


;;     subcat<1,2>
;;     /         \
;;    /           \
;; H subcat<1,3>  3:C<1,2>
(def subcat-2-2-principle
  (let [subcat1 (atom :top)
        subcat2 (atom :top)
        subcat3 (atom {:subcat {:1 subcat1
                               :2 subcat2
                               :3 '()}})]
    {:synsem {:subcat {:1 subcat1
                       :2 subcat2
                       :3 '()}}
     :comp {:synsem subcat3}
     :head {:synsem {:subcat {:1 subcat1
                              :2 subcat3
                              :3 '()}}}}))

;;     subcat<1>
;;     /      \
;;    /        \
;; H subcat<1>  C<>
(def subcat-1-1-principle-comp-subcat-1
  (let [subcat (atom :top)]
    {:synsem {:subcat {:1 subcat
                       :2 '()}}
     :comp {:synsem {:subcat {:1 :top
                              :2 '()}}}
     :head {:synsem {:subcat {:1 subcat
                              :2 '()}}}}))


;;     subcat<1>
;;     /      \
;;    /        \
;; H subcat<1,2>  C[2]
(def subcat-2-principle
  (let [comp-synsem (atom {:cat :top})
        parent-subcat (atom {:cat :top})]
    {:synsem {:subcat {:1 parent-subcat
                       :2 '()}}
     :head {:synsem {:subcat {:1 parent-subcat
                              :2 comp-synsem
                              :3 '()}}}
     :comp {:synsem comp-synsem}}))

;;     subcat<1,3>
;;     /      \
;;    /        \
;; H subcat<1,2>  C[2]<1,3>
(def subcat-3-principle
  (let [subcat-1 (atom :top)
        subcat-3 (atom :top)
        subcat-2 (atom {:subcat {:1 subcat-1
                                :2 subcat-3}})]
    {:synsem {:subcat {:1 subcat-1
                       :2 subcat-3
                       :3 '()}}
     :head {:synsem {:subcat {:1 subcat-1
                              :2 subcat-2
                              :3 '()}}}
     :comp {:synsem subcat-2}}))

;;     subcat<1>
;;     /      \
;;    /        \
;; H subcat<2>  C[2]<1>
(def subcat-4-principle
  (let [subcat-1 (atom :top)
        subcat-2 (atom {:subcat {:1 subcat-1}})]
    {:synsem {:subcat {:1 subcat-1
                       :2 '()}}
     :head {:synsem {:subcat {:1 subcat-2}}}
     :comp {:synsem subcat-2}}))

;;       subcat<1,2>
;;      /          \
;;     /            \
;; H subcat<1,2,3>  C[3]
(def subcat-5-principle
  ;; we specify {:cat :top} rather than simply :top
  ;; because we want to prevent matching with '()
  ;; that is, a verb which only subcats for :1 and 2: (transitive)
  ;; would match :3 because (unify '() :top) => :top,
  ;; and would fit in here erroneously.
  ;; This is prevented by {:cat :top},
  ;; because (unify '() {:cat :top}) => :fail.
  (let [subcat-1 (atom {:cat :top})
        subcat-2 (atom {:cat :top})
        subcat-3 (atom {:cat :top})]
    {:head {:synsem {:subcat {:1 subcat-1
                              :2 subcat-2
                              :3 subcat-3}}}
     :comp {:synsem subcat-3}
     :synsem {:subcat {:1 subcat-1
                       :2 subcat-2
                       :3 '()}}}))

(def comp-modifies-head
  (let [human (atom :top)
        animate (atom :top)
        comp-semantics (atom {:animate animate :human human})
        head-semantics (atom {:animate animate :human human :mod comp-semantics})]
    {:head {:synsem {:sem head-semantics}}
     :comp {:synsem {:sem comp-semantics}}}))

(def comp-specs-head
  (let [comp-semantics (atom :top)
        head-semantics (atom {:spec comp-semantics})]
    {:head {:synsem {:sem head-semantics}}
     :comp {:synsem {:sem comp-semantics}}}))

(def schema-10
  (unify-check
   subcat-1-principle
   head-principle
   {:first :comp
    :comp {:synsem {:subcat '()}}}))

(def c10
  (unify-check
   schema-10
   {:comment "c10"
    ;; TODO: using :schema-symbol below - cannot use :schema for some reason; need to figure out why.
    ;; if you try to use :schema, I get:
    ;; java.util.concurrent.ExecutionException: java.lang.RuntimeException:
    ;; Can't embed object in code, maybe print-dup not defined: clojure.lang.Ref@11819f3c
    :schema-symbol 'c10 ;; used by over-each-parent to know where to put children.
    :first :comp
    :comp {:synsem {:subcat '()}}}))

(def c21
  (unify
   subcat-2-principle
   head-principle
   {:comp {:synsem {:subcat '()}}
    :schema-symbol 'c21 ;; used by over-each-parent to know where to put children.
    :first :comp
    :comment "c21"}))

(def h11
  (unify
   subcat-1-1-principle
   head-principle
   comp-modifies-head
   {:schema-symbol 'h11 ;; used by over-each-parent to know where to put children.
    :first :head
    :comment "h11"}))

;; -- END SCHEMA DEFINITIONS

(defn sentence-impl [input]
  "do things necessary before something can be a sentence. e.g. if infl is still :top, set to
:present (later, set to a randomly selected member of {:finite, :futuro, ..}."
  (do
    (cond
     (seq? input)
     (map (fn [each]
            (sentence-impl each))
          input)
     (= input :top) input
     true
     (let [finitize
           (cond (or (= (get-in input '(:synsem :infl))
                        :top)
                     (= (get-in input '(:synsem :infl))
                        :infinitive))

                 (first (take 1 (shuffle
                                 (list
                                  {:synsem {:infl :futuro}}
                                  {:synsem {:infl :imperfetto}}
                                  {:synsem {:infl :present}}
                                  ))))
                 ;; special additional case for 'potere' exclude :imperfetto.
                 (= (get-in input '(:synsem :infl :not))
                    :imperfetto)
                 (first (take 1 (shuffle
                                 (list
                                  {:synsem {:infl :futuro}}
                                  {:synsem {:infl :present}})))))]
       (let [merged
             (if (= input :fail) :fail
                 (unify input finitize))]
         (do
           merged)))))) ;; for now, no recursive call.

(defn sent-impl [input]
  "shortcut"
  (sentence-impl input))

;; Phrase [:root [1]]
;;       /        \
;;  [1] H          C
(def root-is-head
  "'root' is used to generate and search for expressions that have a given lexeme as their root. e.g. the 'root' of 'io ho parlato' is 'parlare'"
  (let [root (atom {:phrasal false})]
    {:root root
     :head root}))

;; Phrase [:root [1]]
;;        /       \
;;  H [:root [1]]  C
(def root-is-head-root
  (let [root (atom :top)]
    {:root root
     :head {:root root}}))

;; Phrase [:root [1]]
;;        /       \
;;       H     [1] C
(def root-is-comp
  (let [root (atom :top)]
    {:root root
     :comp root}))

(def root-is-comp-root
  (let [root (atom :top)]
    {:root root
     :comp {:root root}}))

(def c10
  (unify schema-10
         {:comment "c10"
          :first :comp
          :schema-symbol 'c10}))
