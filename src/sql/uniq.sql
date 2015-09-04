BEGIN TRANSACTION;

CREATE TEMPORARY TABLE expression_tmp (
       created TIMESTAMP, 
       language TEXT, model TEXT,
       surface TEXT, structure JSONB,
       serialized TEXT);

-- TODO: preserve earliest 'created' value
INSERT INTO expression_tmp (language,model,surface,structure,serialized)
     SELECT DISTINCT language,model,surface,structure,serialized
                FROM expression;

TRUNCATE expression;
INSERT INTO expression (language,model,surface,structure,serialized)
     SELECT language,model,surface,structure,serialized
       FROM expression_tmp;

END TRANSACTION;
