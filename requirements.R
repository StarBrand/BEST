install.packages(c(
	'shiny',
	'fastmap',
	'sass',
	'mongolite',
	'xml2',
	'rmarkdown',
	'shinydashboard',
	'DT',
	'shinyjs',
	'shinyWidgets',
	'shinyalert',
	'openssl',
	'curl', 
	'httr',
	'plotly',
	'shinyBS',
	'rclipboard',
	'rJava',
	'BiocManager',
	'rlist',
	'dbscan',
	'V8',
	'sodium',
	'safer',
	'RPostgres'),
repos = "http://cran.rstudio.com/",
dependencies = TRUE)

install.packages(c(
	'shinyEventLogger'),
dependencies = TRUE)

BiocManager::install(c('XVector',
	'Biostrings',
	'BiocGenerics'))
