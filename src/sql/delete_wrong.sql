DELETE FROM expression WHERE language='fr' AND surface ILIKE '% paindr%';
DELETE FROM expression WHERE language='fr' AND surface ILIKE '% êtra%';
DELETE FROM expression WHERE language='fr' AND surface ILIKE '% étudii%';
DELETE FROM expression WHERE language='fr' AND surface ILIKE '% oublii%';
DELETE FROM expression WHERE language='fr' AND surface ILIKE '% boi';
DELETE FROM expression WHERE language='fr' AND surface ILIKE '% boi %';
DELETE FROM expression WHERE language='fr' AND surface ILIKE '% devoi';
DELETE FROM expression WHERE language='fr' AND surface ILIKE '% devoi %';
DELETE FROM expression WHERE language='fr' AND surface ILIKE '% étuder%';
DELETE FROM expression WHERE language='fr' AND surface ILIKE '% allera%';
DELETE FROM expression WHERE language='fr' AND surface ILIKE $$%'allera%$$;
DELETE FROM expression WHERE language='fr' AND surface ILIKE '% anonc%';
DELETE FROM expression WHERE language='it' AND surface ILIKE '% scaricer%';
DELETE FROM expression WHERE language='it' AND surface ILIKE '% multiplice%';
DELETE FROM expression WHERE language='fr' AND surface ILIKE '% êtri%';
DELETE FROM expression WHERE language='fr' AND surface ILIKE '% devoiri%';
DELETE FROM expression WHERE language='fr' AND surface ILIKE '% devoira%';
DELETE FROM expression WHERE language='fr' AND surface ILIKE '% devois%';
DELETE FROM expression WHERE language='fr' AND surface ILIKE '% avoiri%';
DELETE FROM expression WHERE language='en' AND surface ILIKE 'himself is%';
DELETE FROM expression WHERE language='fr' AND surface ILIKE '% oubler%';
DELETE FROM expression WHERE language='fr' AND surface ILIKE '% avoer%';
DELETE FROM expression WHERE language='fr' AND surface ILIKE '% alleri%';
DELETE FROM expression WHERE language='fr' AND surface ILIKE '% anonciir%';
DELETE FROM expression WHERE language='fr' AND surface ILIKE '% caricere%';
DELETE FROM expression WHERE language='fr' AND surface ILIKE '% avoire%';
DELETE FROM expression WHERE language='fr' AND surface ILIKE $$'avoir$$;
DELETE FROM expression WHERE language='fr' AND surface ILIKE '% être%';
DELETE FROM expression WHERE language='it' AND surface ILIKE '% cercer%';
DELETE FROM expression WHERE language='it' AND surface ILIKE '% recouper%';
DELETE FROM expression WHERE language='en' AND surface ILIKE '% multiplys%';
DELETE FROM expression WHERE language='it' AND surface ILIKE '% capo';
DELETE FROM expression WHERE language='it' AND surface ILIKE '% capi';
DELETE FROM expression WHERE language='it' AND surface ILIKE '% cape';
DELETE FROM expression WHERE language='it' AND surface ILIKE '% capono';
DELETE FROM expression WHERE language='it' AND surface ILIKE '% colpo';
DELETE FROM expression WHERE language='it' AND surface ILIKE '% colpi';
DELETE FROM expression WHERE language='it' AND surface ILIKE '% colpe';
DELETE FROM expression WHERE language='it' AND surface ILIKE '% colpono';
DELETE FROM expression WHERE language='it' AND surface ILIKE '% sprecere%';
DELETE FROM expression WHERE language='it' AND surface ILIKE '% caricere%';
DELETE FROM expression WHERE language='fr' AND surface ILIKE '% allir%';
DELETE FROM expression WHERE language='fr' AND surface ILIKE '% allir%';
DELETE FROM expression WHERE language='fr' AND surface ILIKE '% devoer%';
DELETE FROM expression WHERE language='fr' AND surface ILIKE '% devoir%';
DELETE FROM expression WHERE language='fr' AND surface ILIKE '% devoiré%';
DELETE FROM expression WHERE language='fr' AND surface ILIKE 'tu boit';
DELETE FROM expression WHERE language='fr' AND surface ILIKE $$'avoira%$$;
DELETE FROM expression WHERE language='fr' AND surface ILIKE $$%avoi%$$;
DELETE FROM expression WHERE language='fr' AND surface ILIKE '% êtront';
DELETE FROM expression 
 WHERE id IN (SELECT id FROM 
                 (SELECT id,surface,structure->'root'->'italiano'->>'italiano' AS root, 
                         structure->'synsem'->'sem'->>'pred' AS pred FROM expression) AS wrong 
                   WHERE root='volere' AND pred='have-to');
DELETE FROM expression 
 WHERE id IN (SELECT id FROM 
                 (SELECT id,surface,structure->'root'->'italiano'->>'italiano' AS root, 
                         structure->'synsem'->'sem'->>'pred' AS pred FROM expression) AS wrong 
                   WHERE root='svegliarsi' AND pred='have-fun');
DELETE FROM expression WHERE language='it' AND surface ILIKE '%piegereb%';
DELETE FROM expression WHERE language='fr' AND surface ILIKE '% allerez%';

DELETE FROM expression
 WHERE id IN (SELECT id
  FROM (SELECT id,surface,
               structure->'comp'->'english'->>'english' AS subj_en,
               structure->'comp'->'english'->>'note' AS subj_note,
               structure->'synsem'->'sem'->>'pred' AS pred,
               language,
               structure->'synsem'->'sem'->'subj'->>'gender' AS subj_gender
          FROM expression) AS wrong
  WHERE subj_note='♀'
    AND subj_gender='masc');

DELETE FROM expression
 WHERE id IN (SELECT id
  FROM (SELECT id,surface,
               structure->'comp'->'english'->>'english' AS subj_en,
               structure->'comp'->'english'->>'note' AS subj_note,
               structure->'synsem'->'sem'->>'pred' AS pred,
               language,
               structure->'synsem'->'sem'->'subj'->>'gender' AS subj_gender
          FROM expression) AS wrong
  WHERE subj_note='♂'
    AND subj_gender='fem');



