echo "10 random expressions:"
lein run -m babel.english/demo 10

echo "10 random sentences:"
lein run -m babel.english/demo 10 "{:synsem {:cat :verb}}"

echo "10 random sentences about eating:"
lein run -m babel.english/demo 10 "{:synsem {:cat :verb
                                             :sem {:pred :mangiare}}}"

echo "10 random noun phrases:"
lein run -m babel.english/demo 10 "{:synsem {:cat :noun}}"

echo "10 random noun phrases with 'dog':"
lein run -m babel.english/demo 10 "{:synsem {:cat :noun
                                             :sem {:pred :cane}}}"

