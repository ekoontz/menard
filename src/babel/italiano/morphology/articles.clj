(ns babel.italiano.morphology.articles)

(def l-apostrophe
  {#"^l$"
   {:replace-with "la"
    :unify-with :top}

  #"^gli$"
   {:replace-with "i"
    :unify-with :top}
   })



