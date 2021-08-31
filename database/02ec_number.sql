CREATE TABLE ec_top_level(
	top_level smallint,
	description varchar,
	PRIMARY KEY(top_level)
);
CREATE TABLE ec_subclass(
	top_level smallint,
	subclass smallint,
	description varchar,
	PRIMARY KEY (top_level, subclass),
	FOREIGN KEY (top_level) REFERENCES ec_top_level(top_level)
);
CREATE TABLE ec_subsubclass(
	top_level smallint,
	subclass smallint,
	subsubclass smallint,
	PRIMARY KEY (top_level, subclass, subsubclass),
	FOREIGN KEY (top_level, subclass) REFERENCES ec_subclass(top_level, subclass)
);
CREATE TABLE ec_number(
	ec_number varchar(15),
	top_level smallint,
	subclass smallint,
	subsubclass smallint,
	serial_digit varchar(5),
	systematic_name varchar,
	recommended_name varchar,
	PRIMARY KEY (ec_number),
	FOREIGN KEY (top_level) REFERENCES ec_top_level(top_level),
	FOREIGN KEY (top_level, subclass) REFERENCES ec_subclass(top_level, subclass),
	FOREIGN KEY (top_level, subclass, subsubclass) REFERENCES ec_subsubclass(top_level, subclass, subsubclass)

);
CREATE TABLE synonyms(
	ec_number varchar(13),
	synonyms varchar,
	PRIMARY KEY(synonyms),
	FOREIGN KEY (ec_number) REFERENCES ec_number(ec_number)
);
