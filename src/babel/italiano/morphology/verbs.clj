(ns babel.italiano.morphology.verbs)

(def replace-patterns
  [
   {:p [#"^([^' ]+)o$" "$1are"]
    :u {:synsem {:subcat {:1 {:agr {:number :sing
                                    :person :1st}}}
                 :infl :present}}}
   {:p [#"^([^' ]+)o$" "$1ere"]
    :u {:synsem {:subcat {:1 {:agr {:number :sing
                                    :person :1st}}}
                 :infl :present}}}
   {:p [#"^([^' ]+)o$" "$1ire"]
    :u {:synsem {:subcat {:1 {:agr {:number :sing
                                    :person :1st}}}
                 :infl :present}}}
   
   {:p [#"^([^' ]+)i$" "$1are"]
    :u {:synsem {:subcat {:1 {:agr {:number :sing
                                    :person :2nd}}}
                 :infl :present}}}
   {:p [#"^([^' ]+)i$" "$1iare"] ;; "studi" => "studiare"
    :u {:synsem {:subcat {:1 {:agr {:number :sing
                                    :person :2nd}}}}}}

   {:p [#"^([^' ]+)ci$" "$1ciare"] ;; e.g. "abbracci" => "abbracciare"
    :u {:synsem {:subcat {:1 {:agr {:number :sing
                                    :person :2nd}}}
                 :infl :present}}}
   {:p [#"^([^' ]+)i$" "$1ere"]
    :u {:synsem {:subcat {:1 {:agr {:number :sing
                                    :person :2nd}}}
                 :infl :present}}}
   {:p [#"^([^' ]+)i$" "$1ire"]
    :u {:synsem {:subcat {:1 {:agr {:number :sing
                                    :person :2nd}}}
                 :infl :present}}}

   {:p [#"^([^' ]+)a$" "$1are"]
    :u {:synsem {:subcat {:1 {:agr {:number :sing
                                    :person :3rd}}}
                 :infl :present}}}
   {:p [#"^([^' ]+)e$" "$1ere"]
    :u {:synsem {:subcat {:1 {:agr {:number :sing
                                    :person :3rd}}}
                 :infl :present}}}
   {:p [#"^([^' ]+)e$" "$1ire"]
    :u {:synsem {:subcat {:1 {:agr {:number :sing
                                    :person :3rd}}}
                 :infl :present}}}

   {:p [#"^([^' ]+)iamo$" "$1are"]
    :u {:synsem {:subcat {:1 {:agr {:number :plur
                                    :person :1st}}}
                 :infl :present}}}
   {:p [#"^([^' ]+)iamo$" "$1ere"]
    :u {:synsem {:subcat {:1 {:agr {:number :plur
                                    :person :1st}}}
                 :infl :present}}}
   {:p [#"^([^' ]+)iamo$" "$1ire"]
    :u {:synsem {:subcat {:1 {:agr {:number :plur
                                    :person :1st}}}
                 :infl :present}}}

   {:p [#"^([^' ]+)([aei])te$" "$1$2re"] ;; parlate => parlare; leggere => leggere
    :u {:synsem {:subcat {:1 {:agr {:number :plur
                                    :person :2nd}}}
                 :infl :present}}}

   {:p [#"^([^' ]+)ano$" "$1are"]
    :u {:synsem {:subcat {:1 {:agr {:number :plur
                                    :person :3rd}}}
                 :infl :present}}}
   {:p [#"^([^' ]+)ono$" "$1ere"]
    :u {:synsem {:subcat {:1 {:agr {:number :plur
                                    :person :3rd}}}
                 :infl :present}}}
   {:p [#"^([^' ]+)ono$" "$1ire"]
    :u {:synsem {:subcat {:1 {:agr {:number :plur
                                    :person :3rd}}}
                 :infl :present}}}
   ])



