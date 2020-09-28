CREATE TABLE superkingdom(
	phylum varchar,
	superkingdom varchar,
	PRIMARY KEY (phylum)
);
CREATE TABLE phylum(
	class varchar,
	phylum varchar,
	PRIMARY KEY (class),
	FOREIGN KEY (phylum) REFERENCES superkingdom(phylum)
);
CREATE TABLE class(
	"order" varchar,
	class varchar,
	PRIMARY KEY ("order"),
	FOREIGN KEY (class) REFERENCES phylum(class)
);
CREATE TABLE taxonomic_order(
	family varchar,
	"order" varchar,
	PRIMARY KEY (family),
	FOREIGN KEY ("order") REFERENCES class("order")
);
CREATE TABLE family(
	genus varchar,
	family varchar,
	PRIMARY KEY (genus),
	FOREIGN KEY (family) REFERENCES taxonomic_order(family)
);
CREATE TABLE genus(
	species varchar,
	genus varchar,
	PRIMARY KEY (species),
	FOREIGN KEY (genus) REFERENCES family(genus)
);
