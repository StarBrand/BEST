library(stringr)
library(rlist)
library(taxize)
library(ggtree)
library(shiny)
source("variables.R")

# Generate de ID code from NCBI
getID <- function(species){
  a <- str_split(species, " ")
  s <- list.apply(a, function(x){
    out = ""
    if(length(x) == 2)out <- paste(x[1], x[2], sep = " ")
    else out <- NA
    out
  })
  s <- unlist(s)
  s <- unique(s)
  uids <- get_uid(s, ask = FALSE, messages = FALSE)
  uids
}

# Generate taxa
getTaxa <- function(uids){
  u <- unique(na.omit(uids))
  taxa <- classification(uids, db = "ncbi")
  taxa
}

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
  incProgress(0, detail = paste("Getting NCBI ID, this takes ",
                               showTime(timeID(length(species))),
                               sep = ""))
  uids <- getID(species)
  incProgress(0.3, detail = paste("Getting taxa from ID, this takes ",
                                showTime(timeTaxa(length(uids))),
                                sep = ""))
  taxa <- getTaxa(uids)
  incProgress(0.4, detail = paste("Generating tree, this takes ",
                                showTime(timeTree(length(taxa))),
                                sep = ""))
  tree <- getTree(taxa)
  tplot <- getTreePlot(tree)
  incProgress(0.3, detail = "Ready")
  tplot
}

# Time functions
timeID <- function(n){idLineal[1] + idLineal[2]*n}
timeTaxa <- function(n){taxaLineal[1] + taxaLineal[2]*n}
timeTree <- function(n){treeLineal[1] + treeLineal[2]*n}
showTime <- function(time){
  show <- ""
  if(time < 200) show <- paste(time%/%1 + 1, "sec", sep = " ")
  else{t <- time%/%60 + 1
    if(t < 200) show <- paste(t%/%1 + 1, "min", sep = " ")
    else show <- paste(t%/%60 + 1, "hours", sep = " ")}
  show
}
