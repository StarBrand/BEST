CREATE TABLE ic50(
	ref bigint,
	value double precision,
	max_value double precision,
	inhibitor varchar,
	commentary text,
	PRIMARY KEY (ref, value, max_value, inhibitor, commentary),
	FOREIGN KEY (ref) REFERENCES enzymes(ref)
);
CREATE TABLE temperature_range(
	ref bigint,
	value double precision,
	max_value double precision,
	commentary text,
	PRIMARY KEY (ref, value, max_value, commentary),
	FOREIGN KEY (ref) REFERENCES enzymes(ref)
);
CREATE TABLE ph_range(
	ref bigint,
	value double precision,
	max_value double precision,
	commentary text,
	PRIMARY KEY (ref, value, max_value, commentary),
	FOREIGN KEY (ref) REFERENCES enzymes(ref)
);
CREATE TABLE ph_optima(
	ref bigint,
	value double precision,
	max_value double precision,
	commentary text,
	PRIMARY KEY (ref, value, max_value, commentary),
	FOREIGN KEY (ref) REFERENCES enzymes(ref)
);
CREATE TABLE temperature_optima(
	ref bigint,
	value double precision,
	max_value double precision,
	commentary text,
	PRIMARY KEY (ref, value, max_value, commentary),
	FOREIGN KEY (ref) REFERENCES enzymes(ref)
);
CREATE TABLE turnover_number(
	ref bigint,
	value double precision,
	max_value double precision,
	substrate varchar,
	commentary text,
	PRIMARY KEY (ref, value, max_value, substrate, commentary),
	FOREIGN KEY (ref) REFERENCES enzymes(ref)
);
CREATE TABLE ki(
	ref bigint,
	value double precision,
	max_value double precision,
	inhibitor varchar,
	commentary text,
	PRIMARY KEY (ref, value, max_value, inhibitor, commentary),
	FOREIGN KEY (ref) REFERENCES enzymes(ref)
);
CREATE TABLE molecular_weight(
	ref bigint,
	value double precision,
	max_value double precision,
	commentary text,
	PRIMARY KEY (ref, value, max_value, commentary),
	FOREIGN KEY (ref) REFERENCES enzymes(ref)
);
CREATE TABLE km(
	ref bigint,
	value double precision,
	max_value double precision,
	substrate varchar,
	commentary text,
	PRIMARY KEY (ref, value, max_value, substrate, commentary),
	FOREIGN KEY (ref) REFERENCES enzymes(ref)
);
CREATE TABLE specific_activity(
	ref bigint,
	value double precision,
	max_value double precision,
	commentary text,
	PRIMARY KEY (ref, value, max_value, commentary),
	FOREIGN KEY (ref) REFERENCES enzymes(ref)
);
CREATE TABLE kcat_km(
	ref bigint,
	value double precision,
	max_value double precision,
	substrate varchar,
	commentary text,
	PRIMARY KEY (ref, value, max_value, substrate, commentary),
	FOREIGN KEY (ref) REFERENCES enzymes(ref)
);
CREATE TABLE pi(
	ref bigint,
	value double precision,
	max_value double precision,
	commentary text,
	PRIMARY KEY (ref, value, max_value, commentary),
	FOREIGN KEY (ref) REFERENCES enzymes(ref)
);
