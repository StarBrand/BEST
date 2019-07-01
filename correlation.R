library(stringr)
library(plyr)
library(shiny)
library(rlist)

pal <- seba_palette[c(1,15)]
pal <- setNames(pal, c("Mutant", "Wild Type"))

# Merge tables
mergeTable <- function(attr, table){
  tbl <- list.apply(1:12, function(n){
    tb <- labeling(attr[[n]], table)
    if(nrow(tb) != 0){
      tb <- tb[,c("Recommended_name", "Organism", nat[n], "Mutant")]
      tb <- unique(tb)
    } else{
      tb <- data.frame(Recommended_name = NA, Organism = NA, c = NA, Mutant = NA)
      attributes(tb)$names[3] <- nat[n]
    }
    tb
  })
}

# Makes the list of list of tables to be merge
groupTables <- function(attr){
  x = list(1:4, c(1:2,5:6), c(1:2, 7:8), c(1:2,9:10), c(1:2,11:12),
           5:8, c(3:4,7:8), c(3:4,9:10), c(3:4,11:12), 5:8,
           c(5:6,9:10), c(5:6,11:12), 7:10, c(7:8,9:10), 9:12)
  m <- list()
  m <- list.apply(x, function(i){
    list.append(m, list.apply(i, function(j){
      tb <- attr[[j]]
    }))
  })
  m
}

# Merge every list of tables
doMerge <- function(mergeTable){
  m <- list()
  for(n in 1:length(mergeTable)){
    temp <- join_all(mergeTable[[n]][[1]], type = "full")
    temp <- unique(temp)
    incProgress(0.25/15, detail = "Merging tables")
    m <- list.append(m, temp)
  }
  m
}

# Matrix Heatmap

## Delete the Mutant columns
deleteMutantColumn <- function(listTable){
  listOut <- list.apply(listTable, function(table){
    table$Mutant <- NULL
    table$Recommended_name <- NULL
    table$Organism <- NULL
    table
  })
  listOut
}

## Do the correlation to every table
correlation <- function(tables, method){
  list.apply(tables, cor, use = "pairwise.complete.obs", method = method)
}

## Bind the generated matrix into one
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

# Matrix Scatterplot

## Configure the mutant parameter
configureMutant <- function(atable){
  table <- atable
  table$Mutant <- unlist(sapply(table$Mutant, function(x){
    if(x){out <- "Mutant"}
    else{out <- "Wild Type"}
    out
  }))
  table
}

## Merged Plot
setPlot <- function(table1, table2, session){
  tb <- merge(table1, table2)
  tb <- reduceData(2000, tb, "The merged table of the selected parameters have too many rows to plot, we are reducing it to be able to show it", session)
  p <- plot_ly(data = tb, x = tb[,5], y = tb[,4],
               color = ~Mutant, colors = pal,
               type = "scatter", mode = "markers",
               text = ~paste(Recommended_name, Organism, sep = "\n"),
               showlegend = FALSE)
}

## Genereta axis
axisTitle <- function(numbers){
  lapply(seq_len(length(numbers)), function(n){
    list(title = nat_axis[numbers[n]],
         titlefont = list(size = 10),
         tickfont = list(size = 10))
  })
}

## Plotting
mergingScatter <- function(listTables, numbers, session){
  l <- length(listTables)
  p <- list()
  for(n in seq_len(l)){
    incProgress(0.35/l, detail = "Merging and plotting")
    for(m in seq_len(l)[(1+n):l]){
      incProgress(0.35/l, detail = "Merging and plotting")
      if(!is.na(m)) p <- c(p, list(setPlot(listTables[[n]], listTables[[m]], session)))
      else break
    }
  }
  m <- 2
  if(l <= 2){m <- 1}
  ax <- axisTitle(numbers)
  incProgress(0.1, detail = "Ploting")
  p <- subplot(p, shareX = FALSE, shareY = FALSE, nrows = m, margin = 0.05) %>%
    layout(xaxis = ax[[2]], yaxis = ax[[1]])
  if(l >= 3) p <- layout(p, xaxis2 = ax[[3]], yaxis2= ax[[1]])
  if(l == 3) p <- layout(p, xaxis3 = ax[[3]], yaxis3 = ax[[2]])
  if(l == 4) p <- layout(p, xaxis3 = ax[[4]], yaxis3= ax[[1]],
                         xaxis4 = ax[[3]], yaxis4 = ax[[2]],
                         xaxis5 = ax[[4]], yaxis5 = ax[[2]],
                         xaxis6 = ax[[4]], yaxis6 = ax[[3]])
  p
}