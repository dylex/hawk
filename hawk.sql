CREATE SCHEMA hawk;

CREATE TABLE hawk.browse (
	id	serial PRIMARY KEY,
	uri	uri UNIQUE NOT NULL,
	title	text,
	last	timestamptz (0) NOT NULL DEFAULT now(),
	visits	integer NOT NULL DEFAULT 1
);
CREATE INDEX browse_domain_idx ON hawk.browse (((uri).domain));

CREATE TABLE hawk.mark (
	id	serial PRIMARY KEY,
	uri	uri UNIQUE NOT NULL,
	follow	boolean NOT NULL,
	browse	integer REFERENCES hawk.browse
);
CREATE INDEX mark_domain_idx ON hawk.mark (((uri).domain));

CREATE TABLE hawk.cookie (
	id	serial PRIMARY KEY,
	domain	domainname NOT NULL,
	path	text NOT NULL,
	name	text NOT NULL,
	value	text NOT NULL,
	secure	boolean NOT NULL Default false,
	httponly boolean NOT NULL Default false,
	expires	timestamptz (0),
	UNIQUE (domain, path, name)
);

CREATE OR REPLACE FUNCTION hawk.browse_add(uri, text) RETURNS integer LANGUAGE plpgsql STRICT AS
$$
DECLARE
	u ALIAS FOR $1;
	t ALIAS FOR $2;
	i INTEGER;
BEGIN
	INSERT INTO browse (uri, title) VALUES (u, t)
		ON CONFLICT DO UPDATE SET visits = visits + 1, title = t, last = now()
		RETURNING id INTO i;
	UPDATE mark SET browse = i WHERE uri = u OR follow AND uri @> u;
	RETURN i;
END;
$$
;

CREATE OR REPLACE FUNCTION hawk.mark_add(uri, boolean) RETURNS integer LANGUAGE plpgsql STRICT AS
$$
DECLARE
	u ALIAS FOR $1;
	f ALIAS FOR $2;
	i INTEGER;
BEGIN
	SELECT id INTO i FROM browse WHERE uri = u OR f AND uri <@ u ORDER BY last DESC;
	INSERT INTO mark (uri, follow, browse) VALUES (u, f, i) RETURNING id INTO i;
	RETURN i;
END;
$$
;
