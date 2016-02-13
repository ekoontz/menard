(ns babel.italiano.morphology.verbs)

(def replace-patterns
  [
   {:p [#"^([^' ]+)i$" "$1are"]
    :u {:synsem {:subcat {:1 {:agr {:number :sing
                                    :person :2nd}}}
                 :infl :present}}}
   ])


