CREATE OR REPLACE FUNCTION by_root (in root text)
    RETURNS TABLE(id integer,created timestamp without time zone,language text,
             	  model text,surface text, structure jsonb,serialized text) AS $$
	  SELECT  id,created,language,
   	          model,surface,structure,serialized
FROM expression
WHERE (expression.language='it' AND expression.structure->'root'->'italiano'->>'italiano' = root)
   OR (expression.language='fr' AND expression.structure->'root'->'francais'->>'francais' = root)
   OR (expression.language='es' AND expression.structure->'root'->'espanol'->>'espanol' = root)
   OR (expression.language='en' AND expression.structure->'root'->'english'->>'english' = root)
    $$
    LANGUAGE SQL;

CREATE VIEW exp_with_root
    AS SELECT id,created,language,model,surface,structure,
    serialized,COALESCE(
	structure->'root'->'italiano'->>'italiano',
        structure->'root'->'francais'->>'francais',
   	structure->'root'->'english'->>'english',
	structure->'root'->'espanol'->>'espanol') AS root
FROM expression;
