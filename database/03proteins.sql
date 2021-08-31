CREATE TABLE literature(
	brenda int,
	pmed int,
	link varchar,
	PRIMARY KEY (brenda)
);
CREATE TABLE enzymes(
	ec_number varchar(13),
	organism varchar,
	organism_commentary varchar,
	uniprot varchar,
	commentary text,
	literature int,
	ref bigserial,
	PRIMARY KEY (ref),
	FOREIGN KEY (ec_number) REFERENCES ec_number(ec_number),
	FOREIGN KEY (organism) REFERENCES genus(species),
	FOREIGN KEY (literature) REFERENCES literature(brenda)
);
CREATE TABLE best_users(
	id serial not null,
	mail varchar,
	pass varchar,
	first_name varchar,
	last_name varchar,
	PRIMARY KEY (id)
);
CREATE TABLE user_cache(
	best_user bigint,
	ec_numbers text[],
	refs bigint[],
	fasta boolean DEFAULT false,
	pdb boolean DEFAULT false,
	mw boolean DEFAULT false,
	ic50 boolean DEFAULT false,
	kc boolean DEFAULT false,
	ki boolean DEFAULT false,
	km boolean DEFAULT false,
	pho boolean DEFAULT false,
	phr boolean DEFAULT false,
	pi boolean DEFAULT false,
	sa boolean DEFAULT false,
	"to" boolean DEFAULT false,
	tr boolean DEFAULT false,
	ton boolean DEFAULT false,
	PRIMARY KEY (best_user),
	FOREIGN KEY (best_user) REFERENCES best_users(id)
);
