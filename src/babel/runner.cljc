(ns babel.runner
  (:require #?(:cljs [doo.runner :refer-macros [doo-tests]])
            [babel.core]))

#?(:cljs (doo-tests 'babel.test.pos))
;#?(:cljs (doo-tests 'babel.test.de))
;#?(:cljs (doo-tests 'babel.test.en))
;#?(:cljs (doo-tests 'babel.test.es))
;#?(:cljs (doo-tests 'babel.test.fr))
;#?(:cljs (doo-tests 'babel.test.it))
;;#?(:cljs (doo-all-tests))
