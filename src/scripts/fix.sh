if [ ${HEROKU_DB} ]; then
    PSQL="heroku pg:psql --app verbcoach ${HEROKU_DB}"
else
    PSQL="psql -U verbcoach verbcoach"
fi

cat ~/babel/src/sql/delete_wrong.sql | ${PSQL}

if [[ ! ${DATABASE_URL} ]]; then
    echo "please define \$DATABASE_URL in your environment."
    exit 1
fi

if [[ ! ${PSQL} ]]; then
    echo "please define \$HEROKU_DB or \$PSQL in your environment."
    exit 1
fi

cd ~/babel
lein run -m babel.francais.writer/rewrite-lexicon
lein run -m babel.francais.writer/tout
lein run -m babel.english.writer/translate "fr"

lein run -m babel.italiano.writer/rewrite-lexicon
lein run -m babel.italiano.writer/tutti
lein run -m babel.english.writer/translate "it"

