library(stringr)
library(plyr)
library(shiny)
library(rlist)

# Makes the list of list of tables to be merge
mergeTables <- function(attr){
  x <- list(1:4, c(1:2,5:6), c(1:2, 7:8), c(1:2,9:10), c(1:2,11:12),
            5:8, c(3:4,7:8), c(3:4,9:10), c(3:4,11:12), 5:8,
            c(5:6,9:10), c(5:6,11:12), 7:10, c(7:8,9:10), 9:12)
  m <- list()
  m <- list.apply(x, function(i){
    list.append(m, list.apply(i, function(j){
      attr[[j]][, c("Ref", nat[j], "Mutant")]
    }))
  })
  m
}

# Merge every list of tables
doMerge <- function(mergeTable){
  m <- list()
  for(n in 1:15){
    temp <- join_all(mergeTable[[n]][[1]], by = "Ref", type = "full")
    temp$Ref <- NULL
    incProgress(0.25/15, detail = "Merging tables")
    m <- list.append(m, temp)
  }
  m
}

# Delete the Mutant columns
deleteMutantColumn <- function(listTable){
  listOut <- list.apply(listTable, function(table){
    table$Mutant <- NULL
    table
  })
  listOut
}

# Do the correlation to every table
correlation <- function(tables, method){
  list.apply(tables, cor, use = "pairwise.complete.obs", method = method)
}

# Bind the generated matrix into one
bindMatrix <- function(m){
  ma <- do.call("cbind", list(m[[2]][1:2,3:4], m[[3]][1:2,3:4], m[[4]][1:2,3:4], m[[5]][1:2,3:4]))
  mb <- do.call("cbind", list(m[[6]][1:2,3:4], m[[7]][1:2,3:4], m[[8]][1:2,3:4], m[[9]][1:2,3:4]))
  ma <- rbind(ma, mb)
  ma <- cbind(m[[1]], ma)
  mc <- cbind(m[[2]][3:4,1:2], m[[6]][3:4,1:2])
  md <- cbind(m[[3]][3:4,1:2], m[[7]][3:4,1:2])
  mb <- rbind(mc, md)
  mc <- cbind(m[[11]][1:2,3:4], m[[12]][1:2,3:4])
  md <- cbind(m[[13]][1:2,3:4], m[[14]][1:2,3:4])
  mc <- rbind(mc, md)
  mb <- do.call("cbind", list(mb,m[[10]],mc))
  mc <- do.call("cbind", list(m[[4]][3:4,1:2], m[[8]][3:4,1:2], m[[11]][3:4,1:2], m[[13]][3:4,1:2]))
  md <- do.call("cbind", list(m[[5]][3:4,1:2], m[[9]][3:4,1:2], m[[12]][3:4,1:2], m[[14]][3:4,1:2]))
  mc <- rbind(mc, md)
  mc <- cbind(mc, m[[15]])
  m_ <- do.call("rbind", list(ma, mb, mc))
}