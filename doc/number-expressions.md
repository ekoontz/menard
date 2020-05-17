# Number expressions

Number expressions are phrases like "vier en twentig" or "twenty four".

# Think of a sentence

For example, "vier en twintig".

# do(until(can-tokenize-all-words-with-correct-semantics))

Edit lexemes and lexical rules until you can analyze all the words in the expression
and that a subset of these lexemes are of the expected semantics.
It's ok if there are other incorrect analyses if said analyses are useful
in other expressions. These analyses will be filtered out during parsing in the
next step.

# do(until(a-parse-with-correct-semantics))

Edit rules and lexemes until you can parse the expression, and the semantics
of this expression are as expected.

# do(until(generate-only-good-sentences))

Starting with the semantics `semantics-of-parse` of the parse from the last section, 
create: 
```
{:sem semantics-of-parse}
``` 

and iterate generating on this, adding information to this starting spec until:
- It generates only the desired surface forms as expected.
- Is reasonably "clean", i.e. anything unneeded for the correct generation is
  is removed.


