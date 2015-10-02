if [ ! ${DATABASE_URL} ]; then
    echo "you must define DATABASE_URL in your environment."
    exit 1
fi

echo "\d" |  psql -U ${USER} ${DB} | grep expression_import_ | awk '{print $3}' | xargs -I{} echo "drop table {};" | psql -U ${USER} ${DB}


