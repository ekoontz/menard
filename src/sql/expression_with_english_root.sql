BEGIN TRANSACTION;
DROP VIEW expression_with_root;
CREATE VIEW expression_with_root AS SELECT expression.id,
    expression.created,
    expression.language,
    expression.model,
    expression.surface,
    expression.structure,
    expression.serialized,
    expression.active,
    (((expression.structure -> 'synsem'::text) -> 'sem'::text) -> 'subj'::text) ->> 'pred'::text AS subj,
    COALESCE(
	(((expression.structure -> 'synsem'::text) -> 'sem'::text) -> 'obj'::text) ->> 'pred'::text,
	((expression.structure -> 'synsem'::text) -> 'sem'::text) ->> 'obj'::text
	) AS obj,
    ((expression.structure -> 'synsem'::text) -> 'sem'::text) ->> 'pred'::text AS pred,
    ((expression.structure -> 'synsem'::text) -> 'sem'::text) ->> 'tense'::text AS tense,
    ((expression.structure -> 'synsem'::text) -> 'sem'::text) ->> 'aspect'::text AS aspect,
    COALESCE(
	((expression.structure -> 'root'::text) -> 'italiano'::text) ->> 'italiano'::text,
        ((expression.structure -> 'root'::text) -> 'français'::text) ->> 'français'::text,
       	((expression.structure -> 'root'::text) -> 'english'::text) ->> 'root'::text,
       	((expression.structure -> 'root'::text) -> 'english'::text) ->> 'english'::text,
	((expression.structure -> 'root'::text) -> 'espanol'::text) ->> 'espanol'::text) AS root
   FROM expression;

END TRANSACTION;
