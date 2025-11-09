(ns menard.test.crossproduct
  (:require [dag_unify.core :as u :refer [unify]]
            [dag_unify.serialization :refer [serialize]]
            [clojure.test :refer [deftest is]]
            [menard.crossproduct :refer [cross-product]]
            #?(:clj [clojure.tools.logging :as log])
            #?(:cljs [cljslog.core :as log])))

(deftest cp-1
  ;; can unify the whole thing into one map:
  (is (= (cross-product [{:a 4}{:b 5}{:c 6}{:d 7}{:e 8}])
         #{{:a 4, :d 7, :b 5, :c 6, :e 8}}))

  ;; can't unify anything, so keep as the same maps:
  (is (= (cross-product [{:a 4}{:a 5}{:a 6}{:a 7}{:a 8}])
         #{{:a 4} {:a 5} {:a 6} {:a 7} {:a 8}}))

  ;; some maps can unify, some can't; the result is 2 sets:
  (is (= (-> [{:a 42}{:b 43}{:c 44}{:a 45}]
              shuffle
              cross-product)
         #{{:a 42 :b 43 :c 44}
           {:a 45 :b 43 :c 44}}))

  ;; some maps (that is, {:a 1}) need to be cleaned up:
  (is (= (-> [{:a 1}{:a 2}{:a 3}{:a 4}{:a 1 :b 43}] shuffle cross-product)
         #{{:a 1
            :b 43}
           {:a 2}
           {:a 3}
           {:a 4}})))

(def grammar [{:sem {:tense :present, :aspect :simple}}
              {:sem {:tense :past, :aspect :progressive}}
              {:sem {:tense :past, :aspect :simple}}
              {:comp {:agr {:number :sing, :person :1st}}}
              {:comp {:agr {:number :sing, :person :2nd}}}
              {:comp {:agr {:number :sing, :person :3rd}}}
              {:comp {:agr {:number :plur, :person :1st}}}
              {:comp {:agr {:number :plur, :person :2nd, :formal? true}}}
              {:comp {:agr {:number :plur, :person :3rd}}}])

(def lexicon  [{:canonical "abrazar"}
               {:canonical "abrir"}
               {:canonical "aceptar"}
               {:canonical "almorzar"}
               {:canonical "amar"}
               {:canonical "andar"}
               {:canonical "aprender"}
               {:canonical "ayudar"}
               {:canonical "bailar"}
               {:canonical "beber"}
               {:canonical "buscar"}
               {:canonical "caber"}
               {:canonical "caer"}
               {:canonical "caerse"}
               {:canonical "cambiar"}
               {:canonical "cambiarse"}
               {:canonical "caminar"}
               {:canonical "cancelar"}
               {:canonical "cantar"}
               {:canonical "casarse"}
               {:canonical "cenar"}
               {:canonical "cerrar"}
               {:canonical "comenzar"}
               {:canonical "comer"}
               {:canonical "compartir"}
               {:canonical "comprar"}
               {:canonical "comprender"}
               {:canonical "conducir"}
               {:canonical "conocer"}
               {:canonical "contar"}
               {:canonical "correr"}
               {:canonical "creer"}
               {:canonical "dar"}
               {:canonical "decidir"}
               {:canonical "dejar"}
               {:canonical "desear"}
               {:canonical "despertarse"}
               {:canonical "dibujar"}
               {:canonical "divertirse"}
               {:canonical "dormir"}
               {:canonical "dormirse"}
               {:canonical "empezar"}
               {:canonical "encontrar"}
               {:canonical "enojarse"}
               {:canonical "enseñar"}
               {:canonical "entender"}
               {:canonical "escribir"}
               {:canonical "escuchar"}
               {:canonical "esperar"}
               {:canonical "estudiar"}
               {:canonical "ganar"}
               {:canonical "haber"}
               {:canonical "hablar"}
               {:canonical "hacer"}
               {:canonical "ir"}
               {:canonical "lavar"}
               {:canonical "lavarse"}
               {:canonical "leer"}
               {:canonical "levantarse"}
               {:canonical "limpiar"}
               {:canonical "llamarse"}
               {:canonical "llegar"}
               {:canonical "llenar"}
               {:canonical "llevar"}
               {:canonical "llorar"}
               {:canonical "mirar"}
               {:canonical "mostrar"}
               {:canonical "nacer"}
               {:canonical "nadar"}
               {:canonical "necesitar"}
               {:canonical "oir"}
               {:canonical "olvidar"}
               {:canonical "pagar"}
               {:canonical "pedir"}
               {:canonical "peinarse"}
               {:canonical "pensar"}
               {:canonical "perder"}
               {:canonical "poder"}
               {:canonical "poner"}
               {:canonical "preguntar"}
               {:canonical "querer"}
               {:canonical "romper"}
               {:canonical "saber"}
               {:canonical "sacar"}
               {:canonical "salir"}
               {:canonical "seguir"}
               {:canonical "tener"}
               {:canonical "venir"}
               {:canonical "ver"}
               {:canonical "volver"}])

(deftest cross-product-of-grammar-and-lexicon
  ;; slow: doesn't take into account that all lexical entries are disjoint (i.e. for any pair in the lexicon, the unification of the two is :fail):
  (is (= 1620 (count (->> grammar
                          (lazy-cat lexicon)
                          cross-product))))
  ;; fast: takes into account that all lexical entries are disjoint (i.e. for any pair in the lexicon, the unification of the two is :fail):
  (is (= 1620 (count (->> grammar
                          cross-product
                          (mapcat (fn [g] (->> lexicon (map (fn [l]
                                                              (unify g l))))))
                          (remove #(= :fail %)))))))
