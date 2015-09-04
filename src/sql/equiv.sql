SELECT espanol.surface AS es,
       english.surface AS en,
       espanol.structure->'synsem'->'sem' AS es_semantics
      FROM expression AS espanol
 LEFT JOIN expression AS english
        ON (espanol.language='es')
       AND (english.language='en')
       AND (espanol.structure->'synsem'->'sem') @>
           (english.structure->'synsem'->'sem') LIMIT 100;

SELECT espanol.surface AS es,
       english.surface AS en,
       espanol.structure->'synsem'->'sem' AS es_semantics
      FROM expression AS espanol
 LEFT JOIN expression AS english
        ON (espanol.language='es')
       AND (english.language='en')
       AND (espanol.structure->'synsem'->'sem') @>
           (english.structure->'synsem'->'sem') LIMIT 100 OFFSET 100;





