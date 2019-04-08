library(stringr)
library(rlist)
library(taxize)
library(ggtree)
library(shiny)
source("functions.R")

# Generate species name
speciesName <- function(species){
  a <- str_split(species, " ")
  s <- list.apply(a, function(x){
    out = ""
    if(length(x) == 2)out <- paste(x[1], x[2], sep = " ")
    else out <- NA
    out
  })
  s <- unlist(s)
}

# Generate de ID code from NCBI
getID <- function(species){
  incProgress(0.1, detail = paste("Searching ID, ", showTime(
    timeID( length(species) )
    )))
  s <- speciesName(species)
  s <- unique(s)
  uids <- lapply(s, function(x){
    get_uid(x, ask = FALSE, messages = FALSE)})
  uids
}

# Generate taxa
getTaxa <- function(uids){
  incProgress(0.4, detail = paste("Searching taxa, ", showTime(
    timeTaxa( length(uids) )
    )))
  u <- unlist(uids)
  u <- unique(na.omit(u))
  taxa <- lapply(u, function(x){
    classification(x, db = "ncbi")})
  taxa
}

# Genereta phylogenetic table
phyTable <- function(taxa){
  incProgress(0.4, detail = "Generating phylogeny table")
  out <- data.frame(superkingdom = c(), phylum = c(), class  = c(), order = c(), family = c(), genus = c(), species = c())
  a <- c("superkingdom", "phylum", "class", "order", "family", "genus", "species")
  
  c <- lapply(seq(1, length(taxa)), function(x){
    b <- data.frame(t(taxa[[x]][[1]][(taxa[[x]][[1]][,2] %in% a), c(1,2)]))
    attributes(b)$names <- t(b[2,])
    b[1,]
  })
  
  for(i in c){
    if(typeof(i) == "list")
      out <- rbind.fill(out, i)
  }
  
  out
}

# Add Column
phySelect <- function(table, phylogeny, select){
  out <- table
  out$species <- speciesName(table$Organism)
  if(select != 7){
    out <- merge(out, phylogeny[,c(as.numeric(select),7)], by = "species", all.x = TRUE)
    out$species <- NULL
  }
  out
}

#### PENDING IMPLEMENT TREE ####

# Generate tree
getTree <- function(taxa){
  t <- unique(taxa)
  t <- list.clean(t, function(x){sum(is.na(x)) == 1})
  t <- list.clean(t, function(x){dim(x)[1] < 3})
  tree <- class2tree(t)
  tree
}

# Generete tree plot
getTreePlot <- function(tree){
  groupInfo <- split(tree$phylo$tip.label, gsub("_\\w+", "", tree$phylo$tip.label))
  t <- groupOTU(tree$phylo, groupInfo)
  ggtree(t, aes(color=group) ,layout ='circular')
}

# Do all
getTreeSelective <- function(species){
  incProgress(0, detail = paste("Getting NCBI ID, this might takes ",
                               showTime(timeID(length(species))),
                               sep = ""))
  uids <- getID(species)
  incProgress(0.3, detail = paste("Getting taxa from ID, this might takes ",
                                showTime(timeTaxa(length(uids))),
                                sep = ""))
  taxa <- getTaxa(uids)
  incProgress(0.4, detail = paste("Generating tree, this might takes ",
                                showTime(timeTree(length(taxa))),
                                sep = ""))
  tree <- getTree(taxa)
  tplot <- getTreePlot(tree)
  incProgress(0.3, detail = "Ready")
  tplot
}

# Time functions
timeID <- function(n){(idLineal[1] + idLineal[2]*n)*5}
timeTaxa <- function(n){(taxaLineal[1] + taxaLineal[2]*n)*5}
timeTree <- function(n){treeLineal[1] + treeLineal[2]*n}