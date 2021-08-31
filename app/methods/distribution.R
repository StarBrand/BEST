library(shiny)
source("methods/variables.R")
source("methods/utils.R")

order <- c(1, 2, 3, 4, 4, 5, 5, 5, 2, 6, 6, 3)
ax <- list()
ax[[1]] <- list(title = paste("Data", units[1]))
ax[[2]] <- list(title = paste("Data", units[2]))
ax[[3]] <- list(title = paste("Data kcat/Km", units[3], "|TN", units[12]))
ax[[4]] <- list(title = paste("Data", units[4]))
ax[[5]] <- list(title = paste("Data"))
ax[[6]] <- list(title = paste("Data", units[10]))
ax[[7]] <- list(title = "", showticklabels = FALSE)

# Distribution function
distFunction <- function(n, ...){
  op <- list(...)
  table <- op$table
  data <- op$data
  ftable <- op$ftable
  param <- ftable[[n]]
  if(nrow(param) != 0){
    p <- numericalValue(param)
    p <- getValue(p, "data")
    p <- p[,c("Ref", "data")]
    p <- labeling(p, table)
  } else{p <- param}
  if(nrow(p) != 0)
    p$parameter <- paste(nat_to_show[n])
  rbind(data, p)
}

# Do the thing
generateDistribution <- function(input_attributes, attributeFound, table, fattrTables){
  data <- data.frame(parameter = c(), data = c(), Recommended_name = c(), Organism = c())
  dataList <- sapply(seq(1, 6), function(l) data)
  for(i in 1:12){
    incProgress(0.5/12, detail = "Analysing tables")
    dataList[[order[i]]] <- do_function(i, distFunction, addNoData, addNoData,
                                        input_attributes, attributeFound, table = table,
                                        data = dataList[[order[i]]], ftable = fattrTables)
  }
  dataList
}

# Plot
distPlot <- function(data, i){
  p <- plot_ly(data, x = ~data, color = ~parameter, colors = palDist,
               legendgroup = ~parameter, type = "box",
               text = ~paste(Recommended_name, Organism, sep = "\n"))
}

# Do the thing(plot ver)
generatePlots <- function(data){
  cleaned <- sapply(data, function(d){
    length(d) == 0
  })
  pfinal <- list()
  for(i in 1:6){
    incProgress(0.4/6, detail = "Plotting")
    if(!cleaned[i]){
      p <- distPlot(data[[i]], i)
      pfinal <- list.append(pfinal, p)
    }
  }
  n_p <- length(pfinal)
  if(n_p > 2) nrows <- 2
  else nrows <- 1
  p <- subplot(pfinal, shareX = FALSE, shareY = FALSE, nrows = nrows, margin = 0.07)
  axis <- seq(1, 6)[!cleaned]
  if(n_p >= 1) p <- layout(p, xaxis = ax[[axis[1]]], yaxis = ax[[7]])
  if(n_p >= 2) p <- layout(p, xaxis2 = ax[[axis[2]]], yaxis2 = ax[[7]])
  if(n_p >= 3) p <- layout(p, xaxis3 = ax[[axis[3]]], yaxis3 = ax[[7]])
  if(n_p >= 4) p <- layout(p, xaxis4 = ax[[axis[4]]], yaxis4 = ax[[7]])
  if(n_p >= 5) p <- layout(p, xaxis5 = ax[[axis[5]]], yaxis5 = ax[[7]])
  if(n_p == 6) p <- layout(p, xaxis6 = ax[[axis[6]]], yaxis6 = ax[[7]])
  p
}