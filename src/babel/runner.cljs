(ns babel.runner
  (:require [doo.runner :refer-macros [doo-tests doo-all-tests]]
            [babel.test.en]
            [babel.test.pos]
            [babel.test.es]
            [babel.test.fr]
            [babel.test.it]))

(doo-tests 'babel.test.pos)
(doo-tests 'babel.test.en)
(doo-tests 'babel.test.es)
(doo-tests 'babel.test.fr)
(doo-tests 'babel.test.it)
;(doo-all-tests)

