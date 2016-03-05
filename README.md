[![Clojars Project](http://clojars.org/babel/latest-version.svg)](http://clojars.org/babel)
[![Build Status](https://secure.travis-ci.org/ekoontz/babel.png?branch=master)](http://travis-ci.org/ekoontz/babel)

# babel

A Clojure library for generation of expressions from grammars and
lexicons.

## Prerequisites

- Postgres
- A recent `pgdbc` jar with: https://github.com/pgjdbc/pgjdbc/commit/d313138ee1291220feccccb686d00643ea2e794f
(see https://jdbc.postgresql.org/download.html for downloads).

## Create babel database

```
createuser babel
createdb -U babel babel
psql -U babel babel < src/sql/babel.sql
```

## Set environment so that babel can find your database

```
export DATABASE_URL="postgres://verbcoach@localhost/verbcoach"
```

## Generate sentences

One thing you can do is generate pairs of sentences. Each pair is an
English sentence and a sentence in either French, Italian, or Spanish.

`lein run -m babel.italiano.writer/tutti`

will generate 10 sentences pairs per Italian tense (past, present,
etc). Each pair will be the Italian sentence and its English
translation. Each will be added to the `expression` table. You can then
see them from sql with:

```
psql -U babel babel
SELECT surface,language FROM expression LIMIT 10;
verbcoach=> SELECT id,language,surface FROM expression LIMIT 5;
  id   | language |        surface
-------+----------+-----------------------
 70266 | en       | Antonio erased
 70265 | it       | Antonio ha cancellato
 70264 | en       | he erased
 70263 | it       | lui ha cancellato
 70262 | en       | you erased
```

## License

Copyright © 2015 Eugene Koontz
