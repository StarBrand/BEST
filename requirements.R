install.packages(c('shiny',
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
	'XML',
	'rlist',
	'taxize',
	'dbscan',
	'xml2'),
repos='https://cloud.r-project.org/')"

BiocManager::install(c('XVector',
'Biostrings',
'ggtree'))