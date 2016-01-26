(ns babel.italiano.morphology.articles)

;; TODO: unify with (babel.italiano.morphology/conjugate-italian-prep)
(def l-apostrophe
  {

   #"^dei$"
   {:replace-with "de i"
    :unify-with :top}

   #"^del$"
   {:replace-with "di il"
    :unify-with :top}

   #"^della$"
   {:replace-with "di la"
    :unify-with :top}
   
   #"^l$"
   {:replace-with "la"
    :unify-with :top}
   
   #"^gli$"
   {:replace-with "i"
    :unify-with :top}

   }
  )



