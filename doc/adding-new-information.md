# How to add new grammatical functionality

## Think of a sentence

Think of a sentence that uses the functionality you want to add. 

## Iterate until parse

Iterate on:

- lexicon (e.g. nederlands/lexicon/verbs.edn)
- grammar (e.g. nederlands/grammar.edn)
- lexical rules (e.g. nederlands/lexicon/rules/rules-1.edn)
- universal grammar (ug.edn,subcat.edn)

(hopefully most development will take place in the higher of these and less in lower (lexical rules and universal grammar).

until you can parse the example sentence.

## Find a spec

Find a spec that generates the example sentence, specifically and consistently.

## Generalize the spec

Experiment with making the spec less specific and evaluate whether the resulting generated sentences are grammatical and otherwise sensible.

## Iterate

Iterate on:

- lexicon (e.g. nederlands/lexicon/verbs.edn)
- grammar (e.g. nederlands/grammar.edn)
- lexical rules (e.g. nederlands/lexicon/rules/rules-1.edn)
- universal grammar (ug.edn,subcat.edn)

(hopefully most development will take place in the higher of these and less in lower (lexical rules and universal grammar).

until undesired generated expressions are no longer generated.

