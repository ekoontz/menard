SELECT english.surface AS english,italiano.root AS italiano_root,italiano.surface AS italiano
  FROM expression_with_root AS english
INNER JOIN expression_with_root AS italiano
        ON 
(english.language='en') AND
(italiano.language='it') AND
(english.active=true) AND
(italiano.active=true) AND
((english.structure->'synsem'->'sem'->'pred') = 
 (italiano.structure->'synsem'->'sem'->'pred')) AND
((english.structure->'synsem'->'sem') @>
 (italiano.structure->'synsem'->'sem'))
ORDER BY italiano.root,english.surface;
