library(shinyjs)
library(shinyalert)
source("variables.R")

# Standard readtable
new_table <- function(name){
  read.delim(name, header = TRUE, na.strings = "null", sep = "\t")
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
  tb <- data.frame(Ref = 0, data = NA, Mutant = FALSE)
  attributes(tb)$names[2] <- nat[n]
  tb
}
# Same list in and out
addNoList <- function(n, ...){
  op <- list(...)
  out <- op$listA
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

# Generar imagen
imageEnzymeTab <- function(n){
  file = paste("www\\0", n, " Subclases.png", sep = "")
  list(src = file, contentType = "image/png")
}

# Process Fasta to download
processFasta <- function(folder, no_filter, rows_selected){
  file <- paste(folder, "fasta_output.txt", sep = "")
  if(no_filter){
    table <- read.table(file, header = FALSE, sep = "\t", col.names = "")
    return(table)
  }
  else{
    fasta <- readAAStringSet(file)
    s <- rows_selected
    int <- c()
    if(length(s)){int <- c(int, s)}
    table <- paste(">", names(fasta[int]), "\n", fasta[int], sep = "")
    return(table)
  }
}

# Create Link Function
# [thanks to williamsurles on StackOverflow]
createLink <- function(val, show) {
  paste("<a href=", val, " target = \"_blank\">", show, "</a>", sep = "")
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
    str_split(i, " - ")[[1]][1]})
  p$min <- as.double(p$min)
  p$max <- lapply(p$value, function(i){
    str_split(i, " - ")[[1]][2]})
  p_s <- with(p, p[!grepl(" - ", value),])
  p_r <- with(p, p[grepl(" - ", value),])
  p_r$max <- as.double(p_r$max)
  p <- rbind(p_s, p_r)
}

# Get usable values
getValue <- function(param, name){
  p <- numericalValue(param)
  p_s <- with(p, p[!grepl(" - ", value),])
  p_r <- with(p, p[grepl(" - ", value),])
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

# Update filter
update_filter <- function(session, table, filterId, name, tag){
  label <- paste(name, " filter")
  table <- numericalValue(table)
  r <- min_max(table)
  a <- as.double(r[1])
  b <- as.double(r[2])
  s <- (b - a)/40
  updateSliderInput(session, filterId, label,
                    value = c(a, b), min = a,
                    max = b, step = s)
  
  shinyjs::show(filterId)
  shinyjs::show(paste(tag, "2", sep = ""))
}
# Not found
update_filter_notFound <- function(session, filterId, name){
  label <- paste(name, "not found")
  updateSliderInput(session, filterId, label = label,
                    min = NA, max = NA)
  shinyjs::show(filterId)
}

updateKmeans <- function(n, ...){
  op <- list(...)
  new <- list(n)
  new <- setNames(new, nat_to_show[n])
  if(length(op$listA) == 0){out <- new}
  else{out <- list.merge(op$listA, new)}
  out
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
  p_s <- with(p, p[!grepl(" - ", value),])
  p_r <- with(p, p[grepl(" - ", value),])
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

simplify <- function(param, name){
  tb <- getValue(param, name)
  tb <- tb[,c("Ref", name, "Commentary")]
  tb$Mutant <- unlist(lapply(tb$Commentary, function(x){
    if(grepl("mutant", x)){m <- TRUE}
    else{m <- FALSE}
    m
  }))
  tb$Commentary <- NULL
  tb
}

# Label data
labeling <- function(data, table){
  Ref <- unique(data$Ref)
  Recommended_name <- sapply(Ref, function(x){with(table, table[Ref == x, "Recommended_name"])})
  Organism <- sapply(Ref, function(x){with(table, table[Ref == x, "Organism"])})
  label <- data.frame(Ref, Recommended_name, Organism)
  out <- merge(data, label, x.all = TRUE)
  out$Ref <- NULL
  out
}

# Reduce data
reduceData <- function(long, data, message, session){
  datag <- data
  names <- attributes(datag)$names
  n <- nrow(datag)
  if(n > long){
    showNotification(message,
                     duration = NULL,
                     closeButton = TRUE,
                     session = session)
  }
  while(nrow(datag) > long){
    incProgress(-0.1, detail = "To much data, reducing")
    row.names(datag) <- 1:nrow(datag)
    datag <- with(datag, datag[as.integer(row.names(datag)) %% 2 == 1,])
    datag <- data.frame(datag)
    attributes(datag)$names <- names
    incProgress(0.1, detail = "To much data, reducing")
  }
  if(n > long){
    showNotification(paste("Data reduced, original rows:", n, "It was reduced to", nrow(datag), "rows."),
                    duration = NULL,
                    closeButton = TRUE,
                    type = "warning",
                    session = session)
  }
  datag
}

# Suggestion
# Subtype
generateChoices <- function(input){
  out <- list()
  if(input == "functions"){
    out <- list("New data from Brenda", "Improve the way the data is showed",
                "A useful visualization", "A useful analysis", "Improve an existing function")
  } else if(input == "tutorial"){
    out <- list("On the enzyme query", "On the Protein table", "On the Parameter query",
                "On the other queries", "On summary table", "On visualization", "On cluster",
                "On external tools", "On saved tables", "On errors")
  } else if(input == "graphics"){
    out <- list("On the web site aesthetics", "On the tables", "On the graphs", "On the links",
                "On the downloaded tables")
  } else if(input == "bug"){
    out <- list("Application stoped (indicates the point and as many details as possible)",
                "Redirected to a Not Found Page (indicates the last link or button you clicked)",
                "Incredible slow or the progress bar appears and never stop",
                "Wrong output (Mail us a screenshot if possible)")
  } else if(input == "wrong_data"){
    out <- list("Discrepancy on the generated data and the found on the Brenda page",
                "Discrepancy on the generated data and the found on the literature",
                "The plots don't show the correct data",
                "A text in the web is wrong or not accurate",
                "A link is not correct", "My package (or library) was used and the citation is misspelled",
                "My package or piece of code is used and the credit doesn't appear")
  } else if(input == "question"){
    out <- list("Of how to use the app", "Of how I can contibute")
  } else if(input == "unclasified"){
    out <- list("Because I don't know", "Because I don't care")
  }
  out <- c(out, list("None of the above (new one)", "Don't know how to clasify it"))
  out
}

# Show expceted time
showTime <- function(time){
  show <- ""
  if(time < 0) time <- 0
  if(time < 200) show <- paste(time%/%1 + 1, "sec", sep = " ")
  else{t <- time%/%60 + 1
  if(t < 200) show <- paste(t%/%1 + 1, "min", sep = " ")
  else show <- paste(t%/%60 + 1, "hours", sep = " ")}
  show
}

timeProtein <- function(n){(linealprotein[1] + linealprotein[2]*n)*10}
timePDB <- function(n){(linealpdb[1] + linealpdb[2]*n)*10}
timeParameters <- function(n){(linealparameters[1] + linealparameters[2]*n)*10}
timeFasta <- function(n){(fastatime*n)*10}