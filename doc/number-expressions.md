# Number expressions

Number expressions are phrases like "vier en twintig" or "twenty four".

# Think of a sentence

"vier en twintig"

# do(until(a-parse-with-correct-semantics))

Edit rules and lexemes until you can parse the expression.

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


