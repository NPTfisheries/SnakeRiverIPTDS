/* SQLEditor (Postgres)*/


CREATE TABLE antenna_data
(
id INTEGER,
antenna_id INTEGER,
parameter_id INTEGER UNIQUE ,
read_at DATE,
value NUMERIC
);

CREATE TABLE parameter
(
id INTEGER,
slug VARCHAR(20),
name VARBIT(100),
units VARCHAR(10),
CONSTRAINT parameter_pkey PRIMARY KEY (id)
);

CREATE TABLE environment
(
id INTEGER,
reader_id INTEGER,
parameter_id INTEGER,
read_at DATE,
value NUMERIC
);

CREATE TABLE reader_data
(
id INTEGER,
reader_id INTEGER,
parameter_id INTEGER,
read_at DATE,
value NUMERIC,
CONSTRAINT reader_data_pkey PRIMARY KEY (id)
);

CREATE TABLE antenna
(
id INTEGER,
code VARCHAR(2),
latitude NUMERIC,
longitude NUMERIC,
comment TEXT UNIQUE ,
active BOOLEAN,
reader_id INTEGER,
CONSTRAINT antenna_pkey PRIMARY KEY (id)
);

CREATE TABLE reader_type
(
id INTEGER,
slug VARCHAR(25),
description VARCHAR(200),
CONSTRAINT reader_type_pkey PRIMARY KEY (id)
);

CREATE TABLE reader
(
id INTEGER,
code VARCHAR(2),
reader_type_id INTEGER,
comment VARCHAR(250),
site_id INTEGER,
CONSTRAINT reader_pkey PRIMARY KEY (id)
);

CREATE TABLE site
(
id INTEGER,
slug VARCHAR(5) UNIQUE ,
name VARCHAR(200),
latitude NUMERIC,
longitude NUMERIC,
country VARCHAR(2),
timezonde VARCHAR(100),
is_ptagis_site BOOLEAN,
CONSTRAINT site_pkey PRIMARY KEY (id)
);

CREATE TABLE tag
(
id INTEGER,
tag 14,
detected_at DATE,
antenna_id INTEGER,
CONSTRAINT tag_pkey PRIMARY KEY (id)
);

ALTER TABLE antenna_data ADD FOREIGN KEY (antenna_id) REFERENCES antenna (comment);

ALTER TABLE parameter ADD FOREIGN KEY (id) REFERENCES antenna_data (parameter_id);

ALTER TABLE environment ADD FOREIGN KEY (reader_id) REFERENCES reader (id);

ALTER TABLE environment ADD FOREIGN KEY (parameter_id) REFERENCES antenna_data (parameter_id);

ALTER TABLE reader_data ADD FOREIGN KEY (reader_id) REFERENCES reader (id);

ALTER TABLE reader_data ADD FOREIGN KEY (parameter_id) REFERENCES antenna_data (parameter_id);

ALTER TABLE antenna ADD FOREIGN KEY (comment) REFERENCES antenna (id);

ALTER TABLE antenna ADD FOREIGN KEY (reader_id) REFERENCES reader (id);

ALTER TABLE reader ADD FOREIGN KEY (reader_type_id) REFERENCES reader_type (id);

ALTER TABLE reader ADD FOREIGN KEY (site_id) REFERENCES site (id);

ALTER TABLE tag ADD FOREIGN KEY (antenna_id) REFERENCES antenna (id);
