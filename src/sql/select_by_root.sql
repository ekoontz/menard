CREATE OR REPLACE VIEW expression_with_root
    AS SELECT id,created,language,model,surface,structure,
    serialized,
    structure->'synsem'->'sem'->'subj'->>'pred' AS subj,
    COALESCE(structure->'synsem'->'sem'->'obj'->>'pred',
            structure->'synsem'->'sem'->>'obj') AS obj,
    structure->'synsem'->'sem'->>'pred' AS pred,
       COALESCE(
	structure->'root'->'italiano'->>'italiano',
        structure->'root'->'français'->>'français',
   	structure->'root'->'english'->>'english',
	structure->'root'->'espanol'->>'espanol') AS root
FROM expression;
