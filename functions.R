library(shinyalert)
source("variables.R")

# Standard readtable
new_table <- function(name){
  read.table(name, header = TRUE, na.strings = "null", sep = "\t")
}

# Standard do function for parameter
do_function <- function(n, fun1, fun2, fun3, attr, af, ...){
  if(at[n] %in% attr){
    if(af[n]){fun1(n, ...)}
    else{fun2(n, ...)}}
  else{fun3(n, ...)}
}

# Do nothing functions (made it for syntax)
# Absolutly nothing
do_nothing <- function(n, ...){}
# Same table in and out
addNoTable <- function(n, ...){
  op <- list(...)
  table <- op$table
  table
}
# Same vector in and out
addNoVector <- function(n, ...){
  op <- list(...)
  vector <- op$vector
  vector
}
# Same data in and out
addNoData <- function(n, ...){
  op <- list(...)
  data <- op$data
  data
}
# Same table in and out
noTable <- function(n, ...){
  tb <- data.frame(Ref = 0, data = NA)
  attributes(tb)$names[2] <- nat[n]
  tb
}

# Errors
noSession <- function(){
  shinyalert("Enter Brenda User",
             "You won't be available to search enzymes if we haven't your Brenda account",
             type = "error")
}
noSearch <- function(what){
  shinyalert(paste(what, " of what?", sep = ""),
             paste("We need a list of enzymes (or at least one) to look for ",
                   what,
                   sep = ""),
             type = "error")
}
wrongPassword <- function(){
  shinyalert("Wrong password",
             "Type it again or make sure your password is the same of your Brenda account",
             type = "error")
}
noAccount <- function(){
  shinyalert("This is not a Brenda Account",
             "Type it again or register an account on the Brenda page",
             type = "error")
}

# Endoder function for Parameter Query
encoder_param<-function(attr){
  cond <- 0
  for(n in 1:12){
    if(at[n] %in% attr){cond <- cond + (2**(n-1))}
  }
  cond
}

# Endoder function for Parameter Query
encoder_filter<-function(up){
  cond <- 0
  if(up){cond <- cond + 1}
  cond
}

# Process Fasta to download
processFasta <- function(folder, no_filter){
  file <- paste(folder, "fasta_output.txt", sep = "")
  if(no_filter){
    table <- read.table(file, header = FALSE, sep = "\t", col.names = "")
    return(table)
  }
  else{
    fasta <- readAAStringSet(file)
    s <- input$fastaTable_rows_selected
    int <- c()
    if(length(s)){int <- c(int, s)}
    table <- paste(">", names(fasta[int]), "\n", fasta[int], sep = "")
    return(table)
  }
}

# Create Link Function
# [thanks to williamsurles on StackOverflow]
createLink <- function(val) {
  paste("<a href=", val, ">", val, "</a>", sep = "")
}

# Collapse table
attr_collapse <- function(n, table, with_mol){
  out <- table
  if(with_mol){
    k <- 5
  } else{k <- 4}
  attributes(out)$names <- paste(nat[n], attributes(out)$names, sep ="_")
  attributes(out)$names[1] <- "Ref"
  out <- aggregate(out[,2:k], by=list(out$Ref), paste, collapse=";")
  attributes(out)$names[1] <- "Ref"
  out
}

# Gets the numerical value
numericalValue <- function(param){
  p <- with(param, param[value != "-999.0",])
  p <- with(p, p[value != "-999",])
  p$min <- lapply(p$value, function(i){
    str_split(i, "-")[[1]][1]})
  p$min <- as.double(p$min)
  p$max <- lapply(p$value, function(i){
    str_split(i, "-")[[1]][2]})
  p_s <- with(p, p[!grepl("-", value),])
  p_r <- with(p, p[grepl("-", value),])
  p_r$max <- as.double(p_r$max)
  p <- rbind(p_s, p_r)
}

# Get usable values
getValue <- function(param, name){
  p <- numericalValue(param)
  p_s <- with(p, p[!grepl("-", value),])
  p_r <- with(p, p[grepl("-", value),])
  p_s$min <- as.double(p_s$min)
  p_r$min <- as.double(p_r$min)
  p_r$max <- as.double(p_r$max)
  p_s[,name] <- p_s$min
  p_r[,name] <- (p_r$max - p_r$min)/2
  p <- rbind(p_s, p_r)
}

# Calculate the min max
min_max <- function(param){
  use1 <- unlist(param$min)
  use2 <- unlist(param$max)
  use <- c(use1, use2)
  use <- unlist(use)
  use <- lapply(use, "as.double")
  use <- unlist(use)
  a <- c(min(use, na.rm = TRUE), max(use, na.rm = TRUE))
}

extractNotFound <- function(n, ...){
  op <- list(...)
  table <- op$table
  out <- table
  out[,nat[n]] <- "Parameter Not Found"
  out
}

# Filter function
filterParam <- function(table, f1, f2, mol){
  param <- table
  p <- numericalValue(param)
  p_s <- with(p, p[!grepl("-", value),])
  p_r <- with(p, p[grepl("-", value),])
  # Filter range
  fmin <- as.double(f1)
  fmax <- as.double(f2)
  p_r <- with(p_r, p_r[min >= fmin & max <= fmax,])
  # Filter single value
  p_s <- with(p_s, p_s[min >= fmin & min <= fmax,])
  p <- rbind(p_s, p_r)
  p$min <- NULL
  p$max <- NULL
  p
}