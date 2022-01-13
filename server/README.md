# API

## Generation

- /generate
- /generate-with-alts
- /generate/en

## Parsing 

- /parse
- /parse-start

## Morphology

- /analyze

## Resources

- /grammar/:lang
- /morphology/:lang
- /rule

# Run with:

```
HOSTNAME=$(hostname); \
  export HOSTNAME=$(echo $(perl -e "print lc('$HOSTNAME');")); \
  pushd .; cd .. && lein install && popd \
  && ORIGIN=http://${HOSTNAME}:3449 lein ring server-headless
```

