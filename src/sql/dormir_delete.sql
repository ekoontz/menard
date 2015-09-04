DELETE FROM expression
      WHERE language='es'
        AND structure->'root'->'espanol'->>'espanol' = 'dormir'
	AND structure->'synsem'->>'infl' = 'preterito';

DELETE FROM expression
      WHERE language='es'
        AND structure->'root'->'espanol'->>'espanol' = 'dormir'
	AND structure->'synsem'->'sem'->>'tense' = 'present';


DELETE FROM expression
      WHERE language='fr'
        AND structure->'root'->'français'->>'français' = 'avoir';
