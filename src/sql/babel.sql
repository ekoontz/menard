DROP TABLE expression; 

CREATE TABLE expression (id INTEGER, PRIMARY KEY(id), 
       created TIMESTAMP DEFAULT now(), 
       language TEXT, model TEXT,
       surface TEXT, structure JSONB,
       serialized TEXT);

CREATE SEQUENCE expression_id_seq
                     START WITH 1
                     INCREMENT BY 1
                     NO MINVALUE
                     NO MAXVALUE
                     CACHE 1;

ALTER TABLE ONLY expression ALTER COLUMN id SET DEFAULT nextval('expression_id_seq'::regclass);

CREATE INDEX language_idx ON expression (language);
