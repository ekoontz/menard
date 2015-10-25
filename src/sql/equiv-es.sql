SELECT es.surface AS espanol,
       en.surface AS english,
       es.structure->'synsem'->'sem'->>'pred' AS pred
     FROM expression AS es
LEFT JOIN expression AS en
       ON (es.structure->'synsem'->'sem'->>'pred') = (en.structure->'synsem'->'sem'->>'pred')
      AND ((es.structure->'synsem'->'sem') @> (en.structure->'synsem'->'sem')
           OR
	   (en.structure->'synsem'->'sem') @> (es.structure->'synsem'->'sem'))
      AND en.language='en' AND es.language='es'
WHERE es.language='es';





