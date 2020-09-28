"""parameter_tables.py: Generate the sql script to create parameters table"""

parameters = {
    "molecular_weight": (False, False),
    "km": (True, False),
    "turnover_number": (True, False),
    "kcat_km": (True, False),
    "ki": (False, True),
    "ic50": (False, True),
    "specific_activity": (False, False),
    "ph_optima": (False, False),
    "ph_range": (False, False),
    "temperature_optima": (False, False),
    "temperature_range": (False, False),
    "pi": (False, False)
}


def create_statement(name, substrate, inhibitor):
    # type: (str, bool, bool) -> str
    """Generates sql statement to create table with name"""
    sql_statement = "CREATE TABLE {}(\n\t".format(name)
    sql_statement += "ref bigint,\n\t"
    sql_statement += "value double precision,\n\t"
    sql_statement += "max_value double precision,\n\t"
    if substrate:
        sql_statement += "substrate varchar,\n\t"
    if inhibitor:
        sql_statement += "inhibitor varchar,\n\t"
    sql_statement += "commentary text,\n\t"
    sql_statement += "PRIMARY KEY (ref, value, max_value, "
    if substrate:
        sql_statement += "substrate, "
    if inhibitor:
        sql_statement += "inhibitor, "
    sql_statement += "commentary),\n\t"
    sql_statement += "FOREIGN KEY (ref) REFERENCES enzymes(ref)\n"
    sql_statement += ");\n"
    return sql_statement


if __name__ == '__main__':
    with open("04parameters.sql", "w") as file:
        for parameter, (sbt, ihb) in parameters.iteritems():
            file.write(create_statement(parameter, sbt, ihb))
