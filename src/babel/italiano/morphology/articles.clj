(ns babel.italiano.morphology.articles)

;; TODO: unify with (babel.italiano.morphology/conjugate-italian-prep)
(def l-apostrophe
  {

   #"^l$"
   {:replace-with "la"
    :unify-with :top}
   
   #"^gli$"
   {:replace-with "i"
    :unify-with :top}

   }
  )



