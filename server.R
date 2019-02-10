## This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(DT)
library(rJava)
library(stringr)
library(ggplot2)
library(shinyjs)
library(data.table)
library(shinyWidgets)
library(Biostrings)
library(shinyalert)
library(plotly)
.jinit('BrendaSOAP.jar')

# Define server logic
shinyServer(function(input, output, session) {
  
  # Color palette
  seba_palette <- c("#4c9e4a", "#124211", "#336931", "#68d565", "#a1fb9e",
                    "#083121", "#2e624d", "#499d7b", "#10784e", "#6cd1a8",
                    "#5964e5", "#333ec2", "#394093", "#0a0c26", "#202453")
  
  # Flags
  isThereUser <- reactiveVal(FALSE)
  proteinSearch <- reactiveVal(FALSE)
  pdbSearch <- reactiveVal(FALSE)
  fastaSearch <- reactiveVal(FALSE)
  paramSearch <- reactiveVal(FALSE)
  attributeFound <- reactiveVal(rep(FALSE, 12))
  
  # Function
  # Standard readtable
  new_table <- function(name){
    read.table(name, header = TRUE, na.strings = "null", sep = "\t")
  }
  
  # Attributtes
  at <- c("mw", "ic50", "kc", "ki", "km", "pho", "phr", "pi", "sa", "to", "tr", "ton")
  nat <- c("Molecular_Weight", "IC50", "Kcat/Km",
             "Ki", "Km", "pH_Optimum", "pH_Range",
             "pI", "Specific_Activity", "Temperature_Optimum",
             "Temperature_Range", "Turnover_Number")
  molList <- c("null", "Inhibitor", "Substrate", "Inhibitor", "Substrate", "null",
               "null", "null", "null", "null", "null", "Substrate")
  bool_mol <- molList != "null"
  
  # Standard do function for parameter
  do_function <- function(n, fun1, fun2, fun3, ...){
    if(at[n] %in% input$attributes){
      if(attributeFound()[n]){fun1(n, ...)}
      else{fun2(n, ...)}}
    else{fun3(n, ...)}
  }
  
  do_nothing <- function(n, ...){}
  
  # Tables
  # Protein Table
  proteinTable <- reactiveVal(NULL)
  # Fasta Table
  fastaTable <- reactiveVal(NULL)
  # PDB Table
  pdbTable <- reactiveVal(NULL)
  # Parameter Table
  parameterTable <- reactiveVal(NULL)
  # Summary Table
  summaryTable <- reactiveVal(NULL)
  # Subclass Table
  subclassTable <- reactiveVal(NULL)
  # Synonyms Table
  synonymsTable <- reactiveVal(NULL)
  # Parameters Table
  attrTable <- reactiveVal(list(NULL))
  
  # Plots
  # Histogram
  histPlot <- reactiveVal(NULL)
  # Distribution
  distributionPlot <- reactiveVal(NULL)
  
  # Errors
  noSession <- function(){
    shinyalert("Enter Brenda User",
                 "You won't be available to search enzymes if we haven't your Brenda account",
                 type = "error")
  }
  noSearch <- function(what){
    shinyalert(paste(what, " of what?", sep = ""),
               paste("We need a list of enzymes (or at least one) to look for ",
                     what,
                     sep = ""),
               type = "error")
  }
  
  # To tutorial section
  observeEvent(input$help, {
    updateTabItems(session, "inTabset", "help")
  })
  
  # Create user
  user <- eventReactive(input$logIn, {
    #.jnew("client.User",input$mail, input$pass)
  })
  # Define folder
  folder <- reactiveVal("_results\\")
  
  # Disable user
  observeEvent(input$logIn, {
    updateTabItems(session, "inTabset", "enzyme")
    isThereUser(TRUE)
    shinyjs::show("logOut")
    shinyjs::disable("mail")
    shinyjs::hide("pass")
    shinyjs::hide("logIn")
    new_folder <- paste(input$mail, "_results\\", sep = "")
    folder(new_folder)
  })
  
  # Enable user
  observeEvent(input$logOut, {
    isThereUser(FALSE)
    proteinSearch(FALSE)
    pdbSearch(FALSE)
    fastaSearch(FALSE)
    paramSearch(FALSE)
    shinyjs::show("logIn")
    shinyjs::enable("mail")
    shinyjs::show("pass")
    shinyjs::hide("logOut")
    shinyjs::hide("pick")
    shinyjs::hide("enzymeNameFinal")
    updateTextInput(session, "mail", value = "")
    updateTextInput(session, "pass", value = "")
    proteinTable(NULL)
    fastaTable(NULL)
    pdbTable(NULL)
    parameterTable(NULL)
    summaryTable(NULL)
    attrTable(list(NULL))
    attributeFound(rep(FALSE, 12))
    histPlot(NULL)
    distributionPlot(NULL)
  })
  
  # Endoder function for Parameter Query
  encoder_param<-function(){
    cond <- 0
    if(input$mw){cond <- cond + 1}
    if(input$ic50){cond <- cond + 2}
    if(input$kc){cond <- cond + 4}
    if(input$ki){cond <- cond + 8}
    if(input$km){cond <- cond + 16}
    if(input$pho){cond <- cond + 32}
    if(input$phr){cond <- cond + 64}
    if(input$pi){cond <- cond + 128}
    if(input$sa){cond <- cond + 256}
    if(input$to){cond <- cond + 512}
    if(input$tr){cond <- cond + 1024}
    if(input$ton){cond <- cond + 2048}
    cond
  }
  
  # Endoder function for Parameter Query
  encoder_filter<-function(){
    cond <- 0
    if(input$up){cond <- cond + 1}
    cond
  }
  
  # Enter EC Number functions
  # EC Number 2
  observeEvent(input$ec_number1, {
    updateSelectizeInput(session, "ec_number2",
                         choices = read.table(
                           paste("ecNumber//", input$ec_number1,
                                 "//select.txt", sep = ""),
                           sep = "\t",
                           header = TRUE,
                         col.names = "dig2")$dig2)
  })
  # EC Number3
  observeEvent(input$ec_number2, {
    updateSelectizeInput(session, "ec_number3",
                         choices = read.table(
                           paste("ecNumber//", input$ec_number1,
                                 "//", input$ec_number2,
                                 "//select.txt", sep = ""),
                           sep = "\t",
                           header = TRUE,
                           col.names = "dig3")$dig3)
  })
  # EC Number4
  observeEvent(input$ec_number3, {
    updateSelectizeInput(session, "ec_number4",
                         choices = read.table(
                           paste("ecNumber//", input$ec_number1,
                                 "//", input$ec_number2,
                                 "//", input$ec_number3,
                                 "//select.txt", sep = ""),
                           sep = "\t",
                           header = TRUE,
                           col.names = "dig4")$dig4)
  })
  
  # EC Number
  ec_number <- reactiveVal("1.1.1.1")
  
  # Update selector
  observeEvent(input$subclass, {
    file <- paste("synonyms", input$subclass, "\\subclasses.txt.txt", sep = "")
    table <- read.table(file,
                        sep = "\t",
                        header = TRUE)
    subclass <- as.list(table$EC)
    subclass <- setNames(subclass, table$subclass)
    updateSelectInput(session, "subsubclass",
                      choices = subclass)
  })
  observeEvent(input$subsubclass, {
    if(input$subsubclass != ""){
      file <- paste("synonyms", input$subclass, "\\", input$subsubclass , ".txt", sep = "")
      table <- read.table(file, sep = "\t", header = TRUE, fill = TRUE)
      table$value <- paste(table$ec_number, table$recommended_name, sep = ": ")
      table <- aggregate(table[,c("value")], by=list(table$synonyms), paste, collapse="\t")
      choices <- as.list(table$x)
      choices <- setNames(choices, table$Group.1)
      updateSelectizeInput(session, "enzyme_name",
                           choices = choices)
    }
  })
  
  
  # Search
  observeEvent(input$enzymeName, {
    if(!isThereUser()){noSession()}
    a<-str_split(input$enzyme_name, "\t")
    b <- unlist(a)
    c <- str_split_fixed(b, ": ", 2)
    choices <- as.list(c[,1])
    choices <- setNames(choices, b)
    updateRadioButtons(session, "pick", choices = choices)
    shinyjs::show("pick")
    shinyjs::show("enzymeNameFinal")
  })
  
  # Generate Protein Table
  generateProteinTable <- function(){
    updateTabItems(session, "inTabset", "proteinTable")
    proteinSearch(TRUE)
    fastaSearch(FALSE)
    pdbSearch(FALSE)
    paramSearch(FALSE)
    attributeFound(rep(FALSE, 12))
    #main <- .jnew("main.BrendaSOAP", ec_number(), user(), as.integer(0), as.integer(0))
    #main$getProtein()
    table <- new_table(paste(folder(), "table.txt", sep = ""))
    proteinTable(table)
  }
  
  # EC Number
  observeEvent(input$ecNumber, {
    if(!isThereUser()){noSession()}
    else{
      new_ec_number <- paste(input$ec_number1,
                             input$ec_number2,
                             input$ec_number3,
                             input$ec_number4,
                             sep = ".")
      ec_number(new_ec_number)
      generateProteinTable()
    }
  })
  
  # Enzyme Name
  observeEvent(input$enzymeNameFinal, {
    if(!isThereUser()){noSession()}
    else{ec_number(input$pick)
      generateProteinTable()}
  })
  
  # Protein Table
  output$distProteinTable <- DT::renderDT({
    table <- proteinTable()
    table$Ref <- NULL
    DT::datatable(proteinTable(), options = list(scrollX = TRUE, lengthMenu = c(5, 10, 50, 100), pageLength = 5))
  })
  
  # Download Protein Table
  output$downloadProtein <- downloadHandler(
    filename <- 'protein_table.csv',
    content <- function(name){
      table <- proteinTable()
      table$Ref <- NULL
      write.csv(table, name, quote = FALSE, row.names = FALSE)
    }
  )
  
  # Back to enzyme select
  observeEvent(input$toEnzyme, {
    updateTabItems(session, "inTabset", "enzyme")
  })
  
  # Generate Fasta Table
  observeEvent(input$toFasta, {
    if(!proteinSearch()){noSearch("Sequence")}
    else{
      fastaSearch(TRUE)
      updateTabItems(session, "inTabset", "fasta")
      #main <- .jnew("main.BrendaSOAP", ec_number(), user(), as.integer(0), as.integer(0))
      #main$getProtein()
      #main$getFastaSequence()
      table <- new_table(paste(folder(),"report_fasta.txt", sep = ""))
      fastaTable(table)
    }
  })
  
  # Fasta Table
  output$fastaTable <- DT::renderDT({
    DT::datatable(fastaTable(),
                  options = list(scrollX = TRUE,
                                 lengthMenu = c(5, 10, 50, 100),
                                 pageLength = 10))
  })
  
  # Download Fasta Sequence
  output$downloadFASTA <- downloadHandler(
    filename <- 'sequences.txt',
    content <- function(name){
      if(fastaSearch()){
        table <- processFasta()
      } else{
        table <- NULL
      }
      write.table(table, name, quote = FALSE, row.names = FALSE, col.names = FALSE, sep = "\t")
    }
  )
  
  # Process Fasta to download
  processFasta <- function(){
    file <- paste(folder(), "fasta_output.txt", sep = "")
    if(input$no_filter){
      table <- read.table(file, header = FALSE, sep = "\t", col.names = "")
      return(table)
    }
    else{
      fasta <- readAAStringSet(file)
      s <- input$fastaTable_rows_selected
      int <- c()
      if(length(s)){int <- c(int, s)}
      table <- paste(">", names(fasta[int]), "\n", fasta[int], sep = "")
      return(table)
    }
  }
  
  # Back to protein table
  observeEvent(input$toProtein1, {
    updateTabItems(session, "inTabset", "proteinTable")
  })
  
  # Create Link Function
  # [thanks to williamsurles on StackOverflow]
  createLink <- function(val) {
    paste("<a href=", val, ">", val, "</a>", sep = "")
  }
  
  # Generate PDB Table
  observeEvent(input$toPDB, {
    if(!proteinSearch()){noSearch("PDB")}
    else{
      pdbSearch(TRUE)
      updateTabItems(session, "inTabset", "pdb")
      #main <- .jnew("main.BrendaSOAP", ec_number(), user(), as.integer(0), as.integer(0))
      #main$getProtein()
      #main$getPDB()
      table <- new_table(paste(folder(),"pdb_table.txt", sep = ""))
      table$link <- lapply(table$link, function(i){
        createLink(i)})
      pdbTable(table)
    }
  })
  
  # PDB Table
  output$pdbTable <- DT::renderDT({
    DT::datatable(pdbTable(),
                  options = list(scrollX = TRUE,
                                 lengthMenu = c(5, 10, 50, 100),
                                 pageLength = 10))
  }, escape = FALSE)
  
  # Download PDB
  output$downloadPDB <- downloadHandler(
    filename <- 'pdb.txt',
    content <- function(name){
      file <- paste(folder(), "pdb_table.txt", sep = "")
      table <- read.table(file, header = TRUE, sep = "\t")
      write.csv(table, name, quote = FALSE, row.names = FALSE, sep = "\t")
    }
  )
  
  # Back to protein table
  observeEvent(input$toProtein2, {
    updateTabItems(session, "inTabset", "proteinTable")
  })
  
  # Create parameters table
  importParamTable <- function(n,  ...){
    af <- attributeFound()
    taf <- attrTable()
    attr_folder <- paste(folder(), "attributes\\", sep = "")
    file <- gsub("_", " ", nat[n])
    file <- gsub("/", "_", file)
    file <- paste(file, ".txt", sep ="")
    table_name <- paste(attr_folder, file, sep = "")
    filterId <- paste(at[n], "Filter", sep = "")
    if(file.exists(table_name)){
      af[n] <- TRUE
      table <- new_table(table_name)
      update_filter(table, filterId, nat[n])
      op <- list(...)
      if(!is.null(op$mol) & op$mol != "null"){
        table <- table[,c("Ref", "value", op$mol, "Commentary", "Literature.PubmedID.")]
      } else{table <- table[,c("Ref", "value", "Commentary", "Literature.PubmedID.")]}
      taf[[n]] <- table }
    else{ update_filter_notFound(filterId, nat[n])
      af[n] <- FALSE
      taf[[n]] <- NULL}
    attrTable(taf)
    attributeFound(af)
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
  
  # Add to output Table
  addTable <- function(n, ...){
    op <- list(...)
    table <- op$table
    with_mol <- op$with_mol
    out <- attr_collapse(n, attrTable()[[n]], with_mol)
    table <- merge(table, out, by="Ref", all = TRUE)
  }
  
  # No table to add
  addNoTable <- function(n, ...){
    op <- list(...)
    table <- op$table
    table
  }
  
  # Gets the numerical value
  numericalValue <- function(param){
    p <- with(param, param[value != "-999.0",])
    p <- with(p, p[value != "-999",])
    p$min <- lapply(p$value, function(i){
      str_split(i, "-")[[1]][1]})
    p$min <- as.double(p$min)
    p$max <- lapply(p$value, function(i){
      str_split(i, "-")[[1]][2]})
    p_s <- with(p, p[!grepl("-", value),])
    p_r <- with(p, p[grepl("-", value),])
    p_r$max <- as.double(p_r$max)
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
  update_filter <- function(table, filterId, name){
    label <- paste(name, " filter")
    table <- numericalValue(table)
    r <- min_max(table)
    a <- as.double(r[1])
    b <- as.double(r[2])
    s <- (b - a)/40
    updateSliderInput(session, filterId, label,
                      value = c(a, b), min = a,
                      max = b, step = s)
  }
  # Not fount
  update_filter_notFound <- function(filterId, name){
    label <- paste(name, "not found")
    updateSliderInput(session, filterId, label = label,
                      min = NA, max = NA)
  }
  
  # Select all
  observeEvent(input$allParameters, {
    if(input$allParameters){
      updateCheckboxGroupInput(session, "attributes", selected = at)
    }
    else{
      updateCheckboxGroupInput(session, "attributes", selected = c())
    }
  })
  
  # Generate Parameter Table
  observeEvent(input$parameters, {
    paramSearch(TRUE)
    if(proteinSearch()){
    shinyjs::disable("parameters")
    updateTabItems(session, "inTabset", "parameterTable")
    #main <- .jnew("main.BrendaSOAP", ec_number(), user(), as.integer(encoder_param()), as.integer(encoder_filter()))
    #main$getProtein()
    s <- input$distProteinTable_rows_selected
    int <- as.integer(c())
    if(length(s)){int <- as.integer(c(s - 1, int))}
    int <- .jarray(int)
    #main$getParameters(int, input$allProteins)
    attr_folder <- paste(folder(), "attributes\\", sep = "")
    table <- new_table(paste(folder(), "table.txt", sep = ""))
    proteinTable(table)
    for(i in 1:12){
      do_function(i, do_nothing, importParamTable, do_nothing, mol = molList[i])
      table <- do_function(i, addTable, addNoTable, addNoTable, table = table, with_mol = bool_mol[i])
    }
    table <- table[sort(table$Ref, decreasing = FALSE),]
    table <- data.frame(lapply(table, function(i){
      gsub("-999.0", "Aditional Information", i)}))
    table <- data.frame(lapply(table, function(i){
      gsub("-999", "Aditional Information", i)}))
    table$Ref <- NULL
    table
    shinyjs::enable("parameters")
    parameterTable(table)
    } else{
      noSearch("Parameters")
    }
  })
  
  # Parameter Table
  output$distParameterTable <- DT::renderDT({
    DT::datatable(parameterTable(),
                  options = list(scrollX = TRUE,
                                 lengthMenu = c(5, 10, 50, 100),
                                 pageLength = 5))
  })
  
  # Download Parameter Table
  output$downloadTable <- downloadHandler(
    filename <- 'table.csv',
    content <- function(name){
      if(paramSearch()){
        table <- parameterTable()
      }
      else{
        table <- NULL
      }
      write.csv(table, name, quote = FALSE, row.names = FALSE, sep = "\t")
    }
  )
  
  # Back to protein table
  observeEvent(input$toProtein3, {
    updateTabItems(session, "inTabset", "proteinTable")
  })

  # Extract amount information
  extract <- function(n, ...){
    op <- list(...)
    table <- op$table
    out <- attrTable()[[n]]
    out$value <- 1
    out <- aggregate(out[,"value"],
                     by = list(out$Ref),
                     sum)
    attributes(out)$names[1] <- "Ref"
    attributes(out)$names[2] <- nat[n]
    out <- merge(table, out, all = TRUE)
    out[is.na(out)] <- 0
    out$Parameters <- out$Parameters + out[,nat[n]]
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
  filterParam <- function(n, f1, f2){
    param <- attrTable()[[n]]
    p <- numericalValue(param)
    p_s <- with(p, p[!grepl("-", value),])
    p_r <- with(p, p[grepl("-", value),])
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
  
  filtering <- function(n,...){
    aTable <- attrTable()
    op <- list(...)
    bf <- op$bf
    f1 <- op$f1
    f2 <- op$f2
    table <- op$table
    with_mol <- op$with_mol
    if(bf){
      table2 <- filterParam(n, f1, f2)
      aTable[[n]] <- table2
      attrTable(aTable)
      if(nrow(table2) == 0){
        table2 <- attr_collapse(n, table2, with_mol)
        attributes(table2)$names <- paste(nat[n], attributes(table2)$names, sep ="_")
        attributes(table2)$names[1] <- "Ref"
        }
      table <- merge(table, table2)
    } else{
      table2 <- attr_collapse(n, aTable[[n]], with_mol)
      table <- merge(table, table2, all = TRUE)
    }
  }
  
  # Filter
  observeEvent(input$filter, {
    if(!paramSearch()){
      shinyalert("Cannot Filter",
                 "You need to look for parameters to filter them",
                 type = "error")
    } else{
    table <- proteinTable()
    bfList <- c(input$mw2, input$ic502, input$kc2, input$ki2,
                input$km2, input$pho2, input$phr2, input$pi2,
                input$sa2, input$to2, input$tr2, input$ton2)
    fL <- c(input$mwFilter, input$ic50Filter, input$kcFilter, input$kiFilter,
            input$kmFilter, input$phoFilter, input$phrFilter, input$piFilter,
            input$saFilter, input$toFilter, input$trFilter, input$tonFilter)
    for(n in 1:12){
      table <- do_function(n, filtering, addNoTable, addNoTable,
                           bf = bfList[n], f1 = fL[2*n-1], f2 = fL[2*n], table = table, with_mol = bool_mol[n])
    }
    table$Ref <- NULL
    parameterTable(table)}
  })
  
  # To Enzyme
  observeEvent(input$enzymeInput, {
    updateTabItems(session, "inTabset", "enzyme")
  })
  
  # To protein
  observeEvent(input$paramInput, {
    updateTabItems(session, "inTabset", "proteinTable")
  })
  
  # To protein
  observeEvent(input$fastaInput, {
    updateTabItems(session, "inTabset", "proteinTable")
  })
  
  # To protein
  observeEvent(input$pdbInput, {
    updateTabItems(session, "inTabset", "proteinTable")
  })
  
  # To parameter Table
  observeEvent(input$filterInput, {
    updateTabItems(session, "inTabset", "parameterTable")
  })
  
  # Generate Summary
  observeEvent(input$generateSummary, {
    if(!proteinSearch()){
      noSearch("Information")
    }
    else{
      table <- proteinTable()
      table <- table[,c("Ref", "Recommended_name", "Organism", "UniProt", "Commentary", "EC_Number")]
      table$Parameters <- 0
      for(n in 1:12){
        table <- do_function(n, extract, extractNotFound, addNoTable, table = table)
      }
      table$Found_info <- table$Parameters
      if(fastaSearch()){
        ft <- fastaTable()
        ft$Found_using_Brenda <- as.numeric(ft$Found_using_Brenda)
        ft <- aggregate(ft[,"Found_using_Brenda"],
                        by = list(ft$Enzyme,
                                  ft$Organism,
                                  ft$UniProt), sum)
        attributes(ft)$names[1] <- "Recommended_name"
        attributes(ft)$names[2] <- "Organism"
        attributes(ft)$names[3] <- "UniProt"
        attributes(ft)$names[4] <- "Sequence_FASTA"
        table <- merge(table, ft, all = TRUE)
        table[is.na(table)] <- 0
        table$Found_info <- table$Found_info + table$Sequence_FASTA
      }
      if(pdbSearch()){
        pdb <- pdbTable()
        pdb$PDB <- 1
        pdb <- aggregate(pdb[,"PDB"],
                         by = list(pdb$EC_Number,
                                   pdb$Organism),
                         sum)
        attributes(pdb)$names[1] <- "EC_Number"
        attributes(pdb)$names[2] <- "Organism"
        attributes(pdb)$names[3] <- "PDB"
        table <- merge(table, pdb, all = TRUE)
        table[is.na(table)] <- 0
        table$Found_info <- table$Found_info + table$PDB
      }
      table$EC_Number <- NULL
      table$Ref <- NULL
      summaryTable(table)
    }
  })
  
  # Summary Table
  output$distSummaryTable <- DT::renderDT({
    DT::datatable(summaryTable(),
                  options = list(scrollX = TRUE,
                                 lengthMenu = c(5, 10, 50, 100),
                                 pageLength = 5))
  })
  
  # Count Attributes
  countAttr <- function(n, ...){
    op <- list(...)
    vector <- op$vector
    table <- attrTable()[[n]]
    u <- table$value
    u <- gsub("-999.0", NA, u)
    u <- gsub("-999", NA, u)
    u <- rep(nat[n], sum(
      as.numeric( !is.na(u) )
      ))
    v <- c(vector, u)
  }
  addNoVector <- function(n, ...){
    op <- list(...)
    vector <- op$vector
    vector
  }
  
  # Generate Count plot
  observeEvent(input$visualize, {
    updateTabItems(session, "inTabset", "histogram")
    x <- c()
    for(i in 1:12){
      x <- do_function(i, countAttr, addNoVector, addNoVector, vector = x)
    }
    data <- data.frame(Parameters = x)
    p <- plot_ly(data, x = ~Parameters, color = ~Parameters, type = 'histogram', colors = seba_palette)
    histPlot(p)
  })
  
  # Count Plot
  output$histogram <- renderPlotly({
    histPlot()
  })
  
  # Distribution function
  distFunction <- function(n, ...){
    op <- list(...)
    table <- op$table
    data <- op$data
    param <- attrTable()[[n]]
    p <- numericalValue(param)
    p_s <- with(p, p[!grepl("-", value),])
    p_r <- with(p, p[grepl("-", value),])
    p_s$min <- as.double(p_s$min)
    p_r$min <- as.double(p_r$min)
    p_r$max <- as.double(p_r$max)
    p_s$data <- p_s$min
    p_r$data <- (p_r$max - p_r$min) / 2
    p <- rbind(p_s, p_r)
    p <- p[,c("Ref", "data")]
    p$Recommended_name <- sapply(p$Ref, function(x){with(table, table[Ref == x, "Recommended_name"])})
    p$Organism <- sapply(p$Ref, function(x){with(table, table[Ref == x, "Organism"])})
    p$Ref <- NULL
    if(nrow(p) != 0){ p$parameter <- nat[n] }
    rbind(data, p)
  }
  
  addNoData <- function(n, ...){
    op <- list(...)
    data <- op$data
    data
  }
  
  # Generate Distribution
  observeEvent(input$getDistribution, {
    updateTabItems(session, "inTabset", "distribution")
    table <- proteinTable()
    table <- table[,c("Recommended_name", "Organism", "Ref")]
    data <- data.frame(parameter = c(), data = c(), Recommended_name = c(), Organism = c())
    for(i in 1:12){
      data <- do_function(i, distFunction, addNoData, addNoData, table = table, data = data)
    }
    p <- plot_ly(data, x = ~data, color = ~parameter, colors = seba_palette,
                 type = "box", mode = "markers",
                 text = ~paste(Recommended_name, Organism, sep = "\n"))
    distributionPlot(p)
  })
  
  
  # Distribution
  output$distributionOut <- renderPlotly({
    distributionPlot()
  })
  
})