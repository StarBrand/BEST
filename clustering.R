set.seed(2)

# K means

# Information of the axes of the clustering
dimInfo <- function(s, atrbt){
  out <- div(tags$hr(), style = "text-align: center;",
             h4("Axes"),
             div(style = "float: left; font-weight: bold;",
                 "x: ", nat[as.integer(atrbt)[1]]),
             tags$br(),
             div(style = "float: left; font-weight: bold;",
                 "y: ", nat[as.integer(atrbt)[2]]),
             tags$br(),
             if(s == 3){
               div(style = "float: left; font-weight: bold;",
                   "z: ", nat[as.integer(atrbt)[3]])
             })
}

# Pre process the data, leaving just the necessary parameters
preKmeans <- function(cluster){
  to_cluster <- cluster
  to_cluster$Mutant <- NULL
  to_cluster$Ref <- NULL
  to_cluster$Organism <- NULL
  to_cluster$Recommended_name <- NULL
  to_cluster
}

# Elbow methods
elbow <- function(to_cluster){
  wss <- 0
  n <- nrow(to_cluster)
  n <- min(20, n - 1)
  k <- 1:n
  for (i in k) wss[i] <- sum(kmeans(to_cluster, centers=i)$withinss)
  i <- 1:(n-2)
  data <- data.frame(k = k, wss = wss)
  df <- unlist(lapply(i, function(x){(wss[x+2] - wss[x])/4}))
  m <- min(df)
  df_dx <- data.frame(i, df)
  df_dx <- with(df_dx, df_dx[df == m,])
  n <- wss[df_dx$i + 1] - m*(df_dx$i + 1)
  x <- df_dx$i + 0.5
  x_end <- df_dx$i + 1.5
  y <- m*x + n
  y_end <- m*x_end + n
  out <- list(k = df_dx$i + 1,
              p = ggplot(data = data, aes(x = k, y = wss)) +
                geom_point() +
                geom_segment(x = x, xend = x_end, y = y, yend = y_end, color = "red"))
}

# Clustering K-means algorithm
clusteringKmeans <- function(to_cluster, cluster, k){
  data <- cluster
  km.out <- kmeans(to_cluster, k, nstart = 100)
  data$cluster <- as.character(km.out$cluster)
  data
}

# Complite data to show
completeData <- function(data, table){
  Ref <- unique(data[,c("Recommended_name", "Organism")])
  Ref <- merge(Ref, table, x.all = TRUE)
  Ref$Ref <- NULL
  out <- merge(Ref, data, y.all = TRUE)
}

# Plot
# 3D
plotingKmeans <- function(clustered_data, a, b, c){
  data <- clustered_data
  data <- unique(data)
  p <- plot_ly(data, x = data[, a],
               y = data[, b],
               z = data[, c],
               color = ~cluster, colors = seba_palette,
               type = "scatter3d",
               mode = "markers",
               text = ~paste(Recommended_name, Organism, sep = "\n")
               )
}

#2D
plotingKmeans2d <- function(clustered_data, a, b){
  data <- clustered_data
  data <- unique(data)
  p <- plot_ly(data, x = data[, a],
               y = data[, b],
               color = ~cluster, colors = seba_palette,
               type = "scatter",
               mode = "markers",
               text = ~paste(Recommended_name, Organism, sep = "\n"))
}

#DBscan