library(DBI)
source("resources/secrets.R")
source("methods/variables.R")

connect <- function(){
  conn <- dbConnect(RPostgres::Postgres(),
                    dbname = dbConfig$name,
                    host = dbConfig$host,
                    port = dbConfig$port,
                    user = dbConfig$user,
                    password = dbConfig$password)
  conn
}

standardQuery <- function(sql){
  conn <- connect()
  res <- dbSendQuery(conn, sql)
  out <- dbFetch(res)
  dbClearResult(res)
  dbDisconnect(conn)
  out
}

getEcNumber1 <- function(){
  standardQuery("SELECT top_level FROM ec_top_level order by top_level;")
}

getEcNumber2 <- function(ec1){
  standardQuery(
    sprintf(
      "SELECT subclass FROM ec_subclass WHERE top_level = %s;", ec1
    )
  )
}

getEcNumber3 <- function(ec1, ec2){
  standardQuery(
    sprintf(
      "SELECT subsubclass FROM ec_subsubclass WHERE top_level = %s AND subclass = %s;",
      ec1, ec2
    )
  )
}

getEcNumber4 <- function(ec1, ec2, ec3){
  standardQuery(
    sprintf(
      "SELECT serial_digit FROM ec_number WHERE top_level = %s AND subclass = %s AND subsubclass = %s;",
      ec1, ec2, ec3
    )
  )
}

getTopLevel <- function(){
  top <- standardQuery("SELECT * FROM ec_top_level;")
  setNames(as.list(as.character(top$top_level)), top$description)
}

getReaction <- function(ec1){
  sub <- standardQuery(
    sprintf("SELECT subclass, description FROM ec_subclass WHERE top_level = %s;", ec1)
  )
  setNames(as.list(as.character(sub$subclass)), sub$description)
}

getSynonyms <- function(ec1, ec2){
  standardQuery(
    sprintf(
      "SELECT synonyms, s.ec_number as ec_number, recommended_name
      FROM synonyms s JOIN ec_number e ON s.ec_number = e.ec_number
      WHERE e.top_level = %s AND e.subclass = %s;",
      ec1, ec2
    )
  )
}

getProteins <- function(ecno){
  table <- standardQuery(
    sprintf(
      "SELECT z.ec_number, e.systematic_name, e.recommended_name,
      z.uniprot, z.organism_commentary organism, z.commentary,
      l.link literature, ref, t1.species, t1.genus, t2.family,
      t3.\"order\", t4.class, t5.phylum, t6.superkingdom
      FROM enzymes z JOIN ec_number e ON z.ec_number = e.ec_number
      JOIN literature l ON z.literature = l.brenda LEFT JOIN genus t1
      ON z.organism = t1.species LEFT JOIN family t2 ON t1.genus = t2.genus
      LEFT JOIN taxonomic_order t3 ON t2.family = t3.family
      LEFT JOIN class t4 ON t3.\"order\" = t4.\"order\"
      LEFT JOIN phylum t5 ON t4.class = t5.class
      LEFT JOIN superkingdom t6 ON t5.phylum = t6.phylum
      WHERE z.ec_number = ANY(ARRAY[%s]);",
      ecno
    )
  )
  colnames(table) <- c("EC_Number", "Systematic_name", "Recommended_name", "UniProt", "Organism", "Commentary", "Literature.PubmedID.",
                       "Ref", "Species", "Genus", "Family", "Order", "Class", "Phylum", "Superkingdom")
  table
}

getProteinsRef <- function(ref){
  table <- standardQuery(
    sprintf(
      "SELECT z.ec_number, e.systematic_name, e.recommended_name,
      z.uniprot, z.organism_commentary organism, z.commentary,
      l.link literature, ref, t1.species, t1.genus, t2.family,
      t3.\"order\", t4.class, t5.phylum, t6.superkingdom
      FROM enzymes z JOIN ec_number e ON z.ec_number = e.ec_number
      JOIN literature l ON z.literature = l.brenda LEFT JOIN genus t1
      ON z.organism = t1.species LEFT JOIN family t2 ON t1.genus = t2.genus
      LEFT JOIN taxonomic_order t3 ON t2.family = t3.family
      LEFT JOIN class t4 ON t3.\"order\" = t4.\"order\"
      LEFT JOIN phylum t5 ON t4.class = t5.class
      LEFT JOIN superkingdom t6 ON t5.phylum = t6.phylum
      WHERE ref = ANY(ARRAY[%s]);",
      ref
    )
  )
  colnames(table) <- c("EC_Number", "Systematic_name", "Recommended_name", "UniProt", "Organism", "Commentary", "Literature.PubmedID.",
                       "Ref", "Species", "Genus", "Family", "Order", "Class", "Phylum", "Superkingdom")
  table
}

getParam <- function(ref, param, ...){
  op <- list(...)
  t <- table_name[param]
  molecule <- ""
  if(param %in% c(3, 5, 12)){
    molecule <- "substrate, "
  } else if (param %in% c(2, 4)){
    molecule <- "inhibitor, "
  }
  sql <- sprintf("SELECT ref, value, max_value, %scommentary
                 FROM %s WHERE ref = ANY(ARRAY[%s])",
                 molecule, t, ref)
  if("max" %in% names(op) & "min" %in% names(op)){
    sql <- paste(sql, sprintf(
      "AND (value = -999 OR (
      (max_value = -999 AND value BETWEEN %f AND %f) OR (
      max_value != -999 AND value >= %f AND max_value <= %f)));",
      op$min, op$max, op$min, op$max))
  } else sql <- paste(sql, ";", sep="")
  table <- standardQuery(sql)
  nas <- abs(table$max_value - -999.0) < 1e-8
  table$max_value[nas] <- ""
  table$max_value[!nas] <- paste(" -", table$max_value[!nas], sep = " ")
  table$value[abs(table$value - -999.0) < 1e-8] <- "Additional Information"
  table$value <- paste(table$value, table$max_value, sep = "")
  table$max_value <- NULL
  if(param %in% c(3, 5, 12)){
    colnames(table) <- c("Ref", "value", "Substrate", "Commentary")
  } else if (param %in% c(2, 4)){
    colnames(table) <- c("Ref", "value", "Inhibitor", "Commentary")
  } else {
    colnames(table) <- c("Ref", "value", "Commentary")
  }
  table
}

eraseCache <- function(ref){
  sql <- sprintf("DELETE FROM user_cache WHERE best_user = %s;", ref)
  standardQuery(sql)
}

saveCache <- function(ref, refs, ec){
  sql <- sprintf(
    "INSERT INTO user_cache(best_user, ec_numbers, refs) VALUES (%s, ARRAY['%s'], ARRAY[%s]);",
    ref, ec, refs
  )
  standardQuery(sql)
}

alterCacheParam <- function(ref, refs, param){
  boolean_param <- unlist(lapply(at, function(x){x %in% param}))
  sql <- do.call(
    "sprintf",
    as.list(c("UPDATE user_cache SET refs = ARRAY[%s], mw = %s, ic50 = %s, kc = %s,
    ki = %s, km = %s, pho = %s, phr = %s, pi = %s, sa = %s, \"to\" = %s,
    tr = %s, ton = %s WHERE best_user = %s;", refs, boolean_param, ref))
  )
  standardQuery(sql)
}

alterCacheFasta <- function(ref){
  sql <- sprintf("UPDATE user_cache SET fasta = TRUE WHERE best_user = %s;", ref)
  standardQuery(sql)
}

alterCachePDB <- function(ref){
  sql <- sprintf("UPDATE user_cache SET pdb = TRUE WHERE best_user = %s;", ref)
  standardQuery(sql)
}

checkCache <- function(ref){
  sql <- sprintf(
    "SELECT * FROM user_cache WHERE best_user = %s;", ref
  )
  standardQuery(sql)
}
