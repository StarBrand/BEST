source("variables.R")
source("functions.R")

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
    p <- labeling(p, table)}
  else{p <- param}
  if(nrow(p) != 0){ p$parameter <- paste(nat_to_show[n]) }
  rbind(data, p)
}
