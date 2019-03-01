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
library(plyr)
library(rlist)
library(shinyBS)
library(rclipboard)
.jinit("BrendaSOAP.jar")
source("correlation.R")
source("functions.R")
source("tree.R")
source("clustering.R")
source("helpers.R")

# Define server logic
shinyServer(function(input, output, session) {
  
  hideStuffs <- function(){
    for(n in 1:12){shinyjs::hide(paste(at[n], "Filter", sep = ""))
      shinyjs::hide(paste(at[n], "2", sep = ""))}
    shinyjs::hide("getCorrelationMatrix")
    shinyjs::hide("helpCorrelationMatrix")
    shinyjs::hide("getCorrelationScatter")
    shinyjs::hide("helpCorrelationScatter")
  }
  
  hideStuffs()
  
  # Flags
  isThereUser <- reactiveVal(FALSE)
  proteinSearch <- reactiveVal(FALSE)
  addEcNumber <- reactiveVal(FALSE)
  pdbSearch <- reactiveVal(FALSE)
  fastaSearch <- reactiveVal(FALSE)
  paramSearch <- reactiveVal(FALSE)
  attributeFound <- reactiveVal(rep(FALSE, 12))
  merged <- reactiveVal(FALSE)
  ecNumbers <- reactiveVal(NULL)
  kmeansDim <- reactiveVal(NULL)
  # Saved
  proteinSaved <- reactiveVal(FALSE)
  pdbSaved <- reactiveVal(FALSE)
  fastaSaved <- reactiveVal(FALSE)
  paramSaved <- reactiveVal(FALSE)
  
  # Tables
  # Saved Tables
  summarySaved <- reactiveVal(NULL)
  # Protein Table
  proteinTable <- reactiveVal(NULL)
  # Fasta Table
  fastaTable <- reactiveVal(NULL)
  # PDB Table
  pdbTable <- reactiveVal(NULL)
  # Parameter Table
  parameterTable <- reactiveVal(NULL)
  # Summary Table
  infoTable <- reactiveVal(NULL)
  summaryTable <- reactiveVal(NULL)
  # Subclass Table
  subclassTable <- reactiveVal(NULL)
  # Synonyms Table
  synonymsTable <- reactiveVal(NULL)
  # Parameters Table
  attrTable <- reactiveVal(list(NULL))
  # Filtered (f_)
  fattrTable <- reactiveVal(list(NULL))
  fparameterTable <- reactiveVal(NULL)
  # Merge tables
  groupMerging <- reactiveVal(list(NULL))
  # Clusterized
  kmeansTable <- reactiveVal(NULL)
  
  # Plots
  # Image enzyme tab
  imageWordCloud <- reactiveVal(NULL)
  # Phylogeny
  treePlot <- reactiveVal(NULL)
  # Histogram
  histPlot <- reactiveVal(NULL)
  # Distribution
  distributionPlot <- reactiveVal(NULL)
  # Correlation
  correlationPlot <- reactiveVal(NULL)
  # Clustering: Kmeans
  elbowPlot <- reactiveVal(NULL)
  kPlot <- reactiveVal(NULL)
  
  # To tutorial section
  observeEvent(input$help, {
    updateTabItems(session, "inTabset", "help")
  })
  
  # Create user
  user <- eventReactive(input$logIn, {
    .jnew("client.User",input$mail, input$pass)
  })
  
  # Define folder
  folder <- reactiveVal("_results\\")
  
  # Principal buttons
  observeEvent(input$goHelp, {
    updateTabItems(session, "inTabset", "help")
  })
  observeEvent(input$goProtein, {
    updateTabItems(session, "inTabset", "proteinTable")
  })
  observeEvent(input$goSummary, {
    updateTabItems(session, "inTabset", "info")
  })
  observeEvent(input$goAccount, {
    updateTabItems(session, "inTabset", "account")
  })
  
  # Disable user
  observeEvent(input$logIn, {
    shinyjs::disable("logIn")
    shinyjs::disable("mail")
    shinyjs::disable("pass")
    withProgress(value = 0, message = "Verify account with Brenda", {
      keep <- user()$verifyAccount()
      incProgress(1)
    })
    if(keep == "You can search your enzyme"){
      isThereUser(TRUE)
      shinyjs::show("logOut")
      shinyjs::hide("pass")
      shinyjs::hide("logIn")
      new_folder <- paste(input$mail, "_results\\", sep = "")
      folder(new_folder)
      updateTabItems(session, "inTabset", "enzyme")
    } else if(keep == "Incorrect password"){
      wrongPassword()
      shinyjs::click("logOut")
    } else if (keep == "Not a Brenda User"){
      noAccount()
      shinyjs::click("logOut")
    }
    shinyjs::enable("logIn")
  })
  
  # Enable user
  observeEvent(input$logOut, {
    hideStuffs()
    proteinSearch(FALSE)
    addEcNumber(FALSE)
    pdbSearch(FALSE)
    fastaSearch(FALSE)
    paramSearch(FALSE)
    merged(FALSE)
    kmeansDim(NULL)
    proteinSaved(FALSE)
    pdbSaved(FALSE)
    fastaSaved(FALSE)
    paramSaved(FALSE)
    shinyjs::show("logIn")
    shinyjs::enable("mail")
    shinyjs::enable("pass")
    shinyjs::show("pass")
    shinyjs::hide("logOut")
    shinyjs::hide("pick")
    shinyjs::hide("enzymeNameFinal")
    if(isThereUser()){
      updateTextInput(session, "mail", value = "")
      isThereUser(FALSE)}
    updateTextInput(session, "pass", value = "")
    proteinTable(NULL)
    summarySaved(NULL)
    fastaTable(NULL)
    pdbTable(NULL)
    parameterTable(NULL)
    infoTable(NULL)
    summaryTable(NULL)
    attrTable(list(NULL))
    fattrTable(list(NULL))
    fparameterTable(NULL)
    kmeansTable(NULL)
    attributeFound(rep(FALSE, 12))
    treePlot(NULL)
    histPlot(NULL)
    distributionPlot(NULL)
    correlationPlot(NULL)
    elbowPlot(NULL)
    kPlot(NULL)
    groupMerging(list(NULL))
  })
  
  # Does she/he/they have a saved table
  output$savedTable <- renderUI({
    if(isThereUser() & !proteinSearch() & file.exists(paste(folder(), "table.txt", sep = ""))){
      out <- div(style = "background-color: #a1fb9e; font-weight: bold;",
                 h4(icon("exclamation-triangle"),
                    "You have a saved table ",
                    actionLink("savedTableLink","more info")))
    } else{out <- NULL}
    out
  })
  
  # Link to saved table
  observeEvent(input$savedTableLink, {
    updateTabItems(session, "inTabset", "savedTableTab")
    out <- data.frame(Table = c(), Available = c(), Details = c(), Specific_Details = c())
    out[1, "Table"] <- "Protein Table"
    out[1, "Details"] <- "This table has the ec numbers, recommended names, systematic names and organisms from BRENDA database"
    out[2, "Table"] <- "FASTA Sequence"
    out[2, "Details"] <- "This is a file contains the amino acids sequence of the enzyme you searched"
    out[3, "Table"] <- "PDB codes"
    out[3, "Details"] <- "Contains the PDB codes grouped by enzyme type (ec number) and organism"
    out[4, "Table"] <- "Functional Parameters Table"
    out[4, "Details"] <- "Contains the functional parameters, if it is available the details are below"
    table_file <- paste(folder(), "table.txt", sep = "")
    fasta_file <- paste(folder(), "report_fasta.txt", sep = "")
    pdb_file <- paste(folder(), "pdb_table.txt", sep = "")
    attribute_folder <- paste(folder(), "attributes\\", sep = "")
    if(file.exists(table_file)){
      out[1, "Available"] <- TRUE
      ecno <- as.character(unique(new_table(table_file)$EC_Number))
      ecNumbers(ecno)
      out[1, "Specific_Details"]  <- paste("EC Nuber:",
                                           paste(ecno, collapse = "; "),
                                           ". Total Organisms:",
                                           length(unique(new_table(table_file)$Organism)),
                                           sep = " ")
      
    } else{out[1, "Available"] <- FALSE}
    if(file.exists(fasta_file)){
      out[2, "Available"] <- TRUE
      out[2, "Specific_Details"] <- paste("Sequence found: ",
                                          nrow(new_table(fasta_file)),
                                          sep = "")
    } else{out[2, "Available"] <- FALSE}
    if(file.exists(pdb_file)){
      out[3, "Available"] <- TRUE
      out[3, "Specific_Details"] <- paste("Codes found: ",
                                          nrow(new_table(pdb_file)),
                                          sep = "")
    } else{out[3, "Available"] <- FALSE}
    nat_file <- unlist(list.apply(list(nat), gsub, pattern = "_", replacement = " "))
    nat_file <- unlist(list.apply(list(nat_file), gsub, pattern = "/", replacement = "_"))
    isThere <- FALSE
    detail <- "Parameters saved: "
    selected <- list()
    for(n in 1:12){
      file_name <- paste(attribute_folder, nat_file[n], ".txt", sep = "")
      if(file.exists(file_name)){
        detail <- paste(detail, nat[n], ", ", sep = "")
        isThere <- TRUE
      }
    }
    if(isThere){out[4, "Available"] <- TRUE
      out[4, "Specific_Details"] <- detail
      shinyjs::click("allParameters")
    }else{out[4, "Available"] <- FALSE}
    out
    summarySaved(out)
  })
  
  # Summary saved table
  output$summarySavedTable <- DT::renderDT({
    DT::datatable(summarySaved(), options = list(searching = FALSE))
  })
  
  # Load tables
  observeEvent(input$loadTable, {
    proteinSaved(TRUE)
    generateProteinTable()
    if(2 %in% input$summarySavedTable_rows_selected & summarySaved()[2, "Available"]){fastaSaved(TRUE)}
    if(3 %in% input$summarySavedTable_rows_selected & summarySaved()[3, "Available"]){pdbSaved(TRUE)}
    if(4 %in% input$summarySavedTable_rows_selected & summarySaved()[4, "Available"]){paramSaved(TRUE)}
    main <- .jnew("main.BrendaSOAP", "1.1.1.1", user(), as.integer(0), as.integer(0))
    main$erasePreviousOne(!fastaSaved(), !pdbSaved(), !paramSaved())
    if(fastaSaved()){shinyjs::click("toFasta")}
    if(pdbSaved()){shinyjs::click("toPDB")}
    if(paramSaved()){shinyjs::click("parameters")}
  })
  
  # Or don't
  observeEvent(input$toEnzyme2, {
    updateTabItems(session, "inTabset", "enzyme")
    addEcNumber(FALSE)
  })
  
  
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
  observeEvent(input$enterSubclass, {
    shinyjs::disable("enterSubclass")
    shinyjs::hide("enterSubclass")
    shinyjs::show("subsubclass")
    shinyjs::show("enterSubsubclass")
    shinyjs::enable("enterSubclass")
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
  observeEvent(input$enterSubsubclass, {
    shinyjs::disable("enterSubsubclass")
    shinyjs::hide("enterSubsubclass")
    shinyjs::show("enzyme_name")
    shinyjs::show("enzymeName")
    shinyjs::enable("enterSubsubclass")
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
    shinyjs::hide("enzymeName")
  })
  
  # WordCloud on enzyme tab
  observeEvent(input$ec_number1, {
    imageWordCloud(imageEnzymeTab(input$ec_number1))
  })
  
  observeEvent(input$subclass, {
    imageWordCloud(imageEnzymeTab(input$subclass))
  })
  
  output$subclass <- renderImage({
    imageWordCloud()
  }, deleteFile = FALSE)
  
  # Generate Protein Table
  generateProteinTable <- function(){
    updateTabItems(session, "inTabset", "proteinTable")
    withProgress(message = "Searching for enzymes", value = 0, {
      proteinSearch(TRUE)
      fastaSearch(FALSE)
      pdbSearch(FALSE)
      paramSearch(FALSE)
      attributeFound(rep(FALSE, 12))
      incProgress(0, detail = "Entering data")
      ecno <- as.character(ecNumbers())
      main <- .jnew("main.BrendaSOAP", ecno[1], user(), as.integer(0), as.integer(0))
      if(!proteinSaved()){
        main$erasePreviousOne(!fastaSaved(), !pdbSaved(), !paramSaved())
        n <- length(ecno)
        if(n > 1) lapply(ecno[2:n], function(e){
          main$addEnzyme(e)})
        incProgress(0.2, detail = paste("This might takes ",
                                        showTime(timeProtein(2000)*n),
                                        sep = " "))
        main$getProtein()}
      incProgress(0.7, detail = "Proteins found, showing...")
      table <- new_table(paste(folder(), "table.txt", sep = ""))
      incProgress(0.1, detail = "Ready!")
    })
    proteinSaved(FALSE)
    proteinTable(table)
    ecNumbers(unique(table$EC_Number))
  }
  
  # EC Number
  observeEvent(input$ecNumber, {
    withBusyIndicatorServer("ecNumber",{
    if(!isThereUser()){noSession()}
    else{
      shinyjs::disable("ecNumber")
      new_ec_number <- paste(input$ec_number1,
                             input$ec_number2,
                             input$ec_number3,
                             input$ec_number4,
                             sep = ".")
      if(addEcNumber()){ecno <- as.character(ecNumbers())
        ecno <- c(ecno, new_ec_number)
        ecNumbers(ecno)}
      else{ecNumbers(new_ec_number)}
      addEcNumber(FALSE)
      generateProteinTable()
      shinyjs::enable("ecNumber")
    }
    })
  })
  
  # Enzyme Name
  observeEvent(input$enzymeNameFinal, {
    if(!isThereUser()){noSession()}
    else{
      if(addEcNumber()){ecno <- as.character(ecNumbers())
      ecno <- c(ecno, input$pick)
      ecNumbers(ecno)}
      else{ecNumbers(input$pick)}
      addEcNumber(FALSE)
      shinyjs::disable("enzymeNameFinal")
      generateProteinTable()
      shinyjs::enable("enzymeNameFinal")
      shinyjs::hide("subsubclass")
      shinyjs::hide("enzyme_name")
      shinyjs::hide("enzymeName")
      shinyjs::hide("pick")
      shinyjs::hide("enzymeNameFinal")
      shinyjs::show("enterSubclass")}
  })
  
  # Add Enzyme
  observeEvent(input$addEnzyme, {
    if(!proteinSearch()){
      noProteins()
    } else{
      addEcNumber(TRUE)
      updateTabItems(session, "inTabset", "enzyme")}
  })
  
  # Message to the enzyme searcher
  output$enzymeWillBe <- renderUI({
    if(addEcNumber()){
      h4(icon("exclamation-triangle"), "The enzyme your enter will be added to the search")
    } else if(proteinSearch()){
      h4(icon("exclamation-triangle"), "The enzyme your enter will erase the previous ones")
    } else{
      div(style="text-align:center",
      h3("Welcome!!"),
      h4("Enter the type of enzyme(s) you want to work with"),
      p(icon("hand-point-left"), "From now on, we recommend hiding the dashboard menu to have a better visualization"))
    }
  })
  
  # Protein Table
  output$distProteinTable <- DT::renderDT({
    table <- proteinTable()
    table$Ref <- NULL
    if(!input$showComments1){table$Commentary <- NULL}
    if(!input$showLiterature1){table$Literature.PubmedID. <- NULL}
    DT::datatable(table, options = list(scrollX = TRUE, heigth = '20vh', lengthMenu = c(5, 10, 50, 100), pageLength = 5))
  })
  
  # Download Protein Table
  output$downloadProtein <- downloadHandler(
    filename <- 'protein_table.csv',
    content <- function(name){
      table <- proteinTable()
      table$Ref <- NULL
      write.table(table, name, quote = FALSE, row.names = FALSE, sep = "\t")
    }
  )
  
  # Generate phylogeny selector
  observeEvent(input$phylogeny, {
    if(!proteinSearch()){noSearch("Phylogeny")}
    else{table <- proteinTable()
      updateTabItems(session, "inTabset", "phylogenyTree")
      shinyjs::disable("phylogeny")
      withProgress(message = "Generating tree", value = 0, {
      tree <- getTreeSelective(table$Organism)})
      shinyjs::enable("phylogeny")
      treePlot(tree)
    }
  })
  
  output$treeOut <- renderPlot({
    treePlot()
  })
  
  # Back to enzyme select
  observeEvent(input$toEnzyme, {
    updateTabItems(session, "inTabset", "enzyme")
    addEcNumber(FALSE)
  })
  
  # Generate Fasta Table
  observeEvent(input$toFasta, {
    if(!proteinSearch()){noSearch("Sequence")}
    else{
      fastaSearch(TRUE)
      withProgress(message = "Searching sequence", value = 0, {
        shinyjs::disable("toFasta")
        updateTabItems(session, "inTabset", "fasta")
        incProgress(0, detail = "Entering parameters")
        if(!fastaSaved()){
          ecno <- as.character(ecNumbers())
          main <- .jnew("main.BrendaSOAP", ecno[1], user(), as.integer(0), as.integer(0))
          n <- length(ecno)
          if(n > 1) lapply(ecno[2:n], function(e){
            main$addEnzyme(e)
            })
          main$getProtein()
          incProgress(0.2, detail = paste("This might takes ",
                                          showTime(timeFasta(length(ecNumbers()))),
                                          sep = ""))
          main$getFastaSequence()}
        incProgress(0.7, detail = "Showing")
        table <- new_table(paste(folder(),"report_fasta.txt", sep = ""))
        incProgress(0.1, detail = "Ready")
        shinyjs::enable("toFasta")
      })
      fastaTable(table)
      fastaSaved(FALSE)
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
        table <- processFasta(folder(), input$no_filter, input$fastaTable_rows_selected)
      } else{
        table <- NULL
      }
      write.table(table, name, quote = FALSE, row.names = FALSE, col.names = FALSE, sep = "\t")
    }
  )
  
  # Back to protein table
  observeEvent(input$toProtein1, {
    updateTabItems(session, "inTabset", "proteinTable")
  })
  
  # Generate PDB Table
  observeEvent(input$toPDB, {
    if(!proteinSearch()){noSearch("PDB")}
    else{
      pdbSearch(TRUE)
      shinyjs::disable("toPDB")
      withProgress(message = "Searching PDB", value = 0, {
        updateTabItems(session, "inTabset", "pdb")
        incProgress(0, detail = "Enter parameter")
        if(!pdbSaved()){
          ecno <- as.character(ecNumbers())
          main <- .jnew("main.BrendaSOAP", ecno[1], user(), as.integer(0), as.integer(0))
          n <- length(ecno)
          if(n > 1) lapply(ecno[2:n], function(e){
            main$addEnzyme(e)
            })
          main$getProtein()
          incProgress(0.2, detail = paste("This might takes ",
                                          showTime(timePDB(nrow(proteinTable()))*length(ecNumbers())),
                                          sep = ""))
          main$getPDB()}
        incProgress(0.7, detail = "Showing")
        table <- new_table(paste(folder(),"pdb_table.txt", sep = ""))
        table$link <- lapply(table$link, function(i){
          createLink(i)})
        incProgress(0.1, detail = "Ready")
      })
      shinyjs::enable("toPDB")
      pdbTable(table)
      pdbSaved(FALSE)
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
      write.table(table, name, quote = FALSE, row.names = FALSE, sep = "\t")
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
    ftaf <- fattrTable()
    attr_folder <- paste(folder(), "attributes\\", sep = "")
    table_name <- paste(attr_folder, files_name[n], sep = "")
    filterId <- paste(at[n], "Filter", sep = "")
    if(file.exists(table_name)){
      af[n] <- TRUE
      table <- new_table(table_name)
      update_filter(session, table, filterId, nat[n], at[n])
      op <- list(...)
      if(!is.null(op$mol) & op$mol != "null"){
        table <- table[,c("Ref", "value", op$mol, "Commentary", "Literature.PubmedID.")]
      } else{table <- table[,c("Ref", "value", "Commentary", "Literature.PubmedID.")]}
      taf[[n]] <- table
      ftaf[[n]] <- table}
    else{ update_filter_notFound(session, filterId, nat[n])
      af[n] <- FALSE
      taf[[n]] <- NULL
      ftaf[[n]] <- NULL}
    attrTable(taf)
    fattrTable(ftaf)
    attributeFound(af)
  }
  eraseParam <- function(n, ...){
    af <- attributeFound()
    taf <- attrTable()
    ftaf <- fattrTable()
    af[n] <- FALSE
    taf[[n]] <- NULL
    ftaf[[n]] <- NULL
    attributeFound(af)
    attrTable(taf)
    fattrTable(ftaf)
  }
  
  # Add to output Table
  addTable <- function(n, ...){
    op <- list(...)
    table <- op$table
    with_mol <- op$with_mol
    out <- attr_collapse(n, attrTable()[[n]], with_mol)
    table <- merge(table, out, by="Ref", all = TRUE)
  }
  
  # Select all
  observeEvent(input$allParameters, {
    if(input$allParameters){
      updateCheckboxGroupInput(session, "attributes", selected = at)
    }
    else if(!input$allParameters){
      updateCheckboxGroupInput(session, "attributes", selected = c())
    }
  })
  
  # Generate Parameter Table
  observeEvent(input$parameters, {
    paramSearch(TRUE)
    if(proteinSearch()){
    shinyjs::disable("parameters")
    updateTabItems(session, "inTabset", "parameterTable")
    withProgress(message = "Searching numerical Parameters", value = 0, {
      incProgress(0, detail = "Entering parameters...")
      if(!paramSaved()){
        ecno <- as.character(ecNumbers())
        main <- .jnew("main.BrendaSOAP", ecno[1], user(), as.integer(encoder_param(input$attributes)), as.integer(encoder_filter(input$up)))
        n <- length(ecno)
        if(n > 1) lapply(ecno[2:n], function(e){
          main$addEnzyme(e)
          })
        main$getProtein()
        incProgress(0.1, detail = "Selecting proteins for search")
        s <- input$distProteinTable_rows_selected
        int <- as.integer(c())
        if(length(s)){int <- as.integer(c(s - 1, int))}
        int <- .jarray(int)
        if(input$allProteins){n_pro <- nrow(proteinTable())}
        else{n_pro <- s}
        incProgress(0.2, detail = paste("This might takes ",
                                        showTime(timeParameters(n_pro)*length(ecNumbers())),
                                        sep = ""))
        main$getParameters(int, input$allProteins)}
      attr_folder <- paste(folder(), "attributes\\", sep = "")
      table <- new_table(paste(folder(), "table.txt", sep = ""))
      proteinTable(table)
      incProgress(0.5)
      listA <- list()
      for(n in 1:12){
        incProgress(0.1/12, detail = "Showing results...")
        do_function(n, eraseParam, importParamTable, eraseParam, input$attributes, attributeFound(), mol = molList[n])
        table <- do_function(n, addTable, addNoTable, addNoTable, input$attributes, attributeFound(), table = table, with_mol = bool_mol[n])
        listA <- do_function(n, updateKmeans, addNoList, addNoList, input$attributes, attributeFound(), listA = listA)
      }
      updateCheckboxGroupInput(session, "kmeans", choices = listA)
      incProgress(0.05, detail = "Working on data")
      table <- as.data.frame(sapply(table,gsub,pattern="-999.0",replacement="Additional Information"))
      table <- as.data.frame(sapply(table,gsub,pattern="-999",replacement="Additional Information"))
      table <- table[sort(table$Ref, decreasing = FALSE),]
      table$Ref <- NULL
      incProgress(0.05, detail = "Ready")
      shinyjs::enable("parameters")
    })
    parameterTable(table)
    fparameterTable(table)
    paramSaved(FALSE)
    } else{
      noSearch("Parameters")
    }
  })
  
  # Parameter Table
  output$distParameterTable <- DT::renderDT({
    table <- fparameterTable()
    if(!input$showComments2){v <- grepl("Commentary", attributes(table)$names)
    table[,attributes(table)$names[v]] <- NULL}
    if(!input$showLiterature2){v <- grepl("Literature", attributes(table)$names)
    table[,attributes(table)$names[v]] <- NULL}
    DT::datatable(table,
                  options = list(scrollX = TRUE,
                                 lengthMenu = c(2, 5, 10, 50, 100),
                                 pageLength = 5))
  })
  
  # Download Parameter Table
  output$downloadTable <- downloadHandler(
    filename <- 'table.csv',
    content <- function(name){
      if(paramSearch()){
        table <- fparameterTable()
      }
      else{
        table <- NULL
      }
      write.table(table, name, quote = FALSE, row.names = FALSE, sep = "\t")
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
    out <- fattrTable()[[n]]
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
  
  # Filtering the data
  filtering <- function(n,...){
    table2 <- attrTable()[[n]]
    op <- list(...)
    table <- op$table
    if(!is.null(op$mol) & op$mol != "null"){
      table2 <- table2[,c("Ref", "value", op$mol, "Commentary", "Literature.PubmedID.")]
    } else{table2 <- table2[,c("Ref", "value", "Commentary", "Literature.PubmedID.")]}
    if(op$bf){
      table2 <- filterParam(table2, op$f1, op$f2, op$mol)
      if(nrow(table2) != 0){
        table2 <- attr_collapse(n, table2, op$with_mol)
      }
      table <- merge(table, table2)
    } else{
      table2 <- attr_collapse(n, table2, op$with_mol)
      table <- merge(table, table2, all.x = TRUE)
    }
    table
  }
  
  # Save the filtered Table
  saveFiltered <- function(n, ...){
    op <- list(...)
    table <- op$table
    NewTables <- fattrTable()
    table2 <- attrTable()[[n]]
    col <- paste(nat[n], "value", sep = "_")
    v <- table[,col]
    v <- unlist(strsplit(v, ";"), recursive = TRUE)
    NewTables[[n]] <- with(table2, table2[Ref %in% table$Ref & value %in% v,])
    fattrTable(NewTables)
  }
  
  # Filter
  observeEvent(input$filter, {
    if(!paramSearch()){
      shinyalert("Cannot Filter",
                 "You need to look for parameters to filter them",
                 type = "error")
    } else{
    shinyjs::disable("filter")
    withProgress(message = "Filtering", value = 0, {
      table <- proteinTable()
      bfList <- c(input$mw2, input$ic502, input$kc2, input$ki2,
                  input$km2, input$pho2, input$phr2, input$pi2,
                  input$sa2, input$to2, input$tr2, input$ton2)
      fLprev <- c(input$mwFilter, input$ic50Filter, input$kcFilter, input$kiFilter,
                  input$kmFilter, input$phoFilter, input$phrFilter, input$piFilter,
                  input$saFilter, input$toFilter, input$trFilter, input$tonFilter)
      fL <- c()
      i <- 1
      incProgress(0, detail = "Loading Parameters")
      for(n in 1:12){
        if(attributeFound()[n] & at[n] %in% input$attributes){
          fL <- c(fL, fLprev[i], fLprev[i+1])
          i <- i + 2}
        else{fL <- c(fL, NA, NA)}
        incProgress(0.25/12, detail = "Loading Parameters")
      }
      for(n in 1:12){
        table <- do_function(n, filtering, addNoTable, addNoTable, input$attributes, attributeFound(),
                             bf = bfList[n], f1 = fL[2*n-1], f2 = fL[2*n], table = table,
                             with_mol = bool_mol[n], mol = molList[n])
        incProgress(0.25/12, detail = "Filtering")
      }
      for(n in 1:12){
        do_function(n, saveFiltered, do_nothing, do_nothing, input$attributes, attributeFound(), table = table)
        incProgress(0.25/12, detail = "Saving changes")
      }
      table <- as.data.frame(sapply(table,gsub,pattern="-999.0",replacement="Additional Information"))
      table <- as.data.frame(sapply(table,gsub,pattern="-999",replacement="Additional Information"))
      table <- table[sort(table$Ref, decreasing = FALSE),]
      incProgress(0.25, detail = "Ready")
      table$Ref <- NULL
      shinyjs::enable("filter")
    })
    fparameterTable(table)}
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
  infoSummary <- function(n, ...){
    op <- list(...)
    y <- sum(op$table[,nat[n]])
    y
  }
  
  notfoundInfoSummary <- function(n, ...){
    "Not found"
  }
  
  notSelectInfoSummary <- function(n, ...){
    "No searched"
  }
  
  observeEvent(input$generateSummary, {
    if(!proteinSearch()){
      noSearch("Information")
    }
    else{
      table <- proteinTable()
      table <- table[,c("Ref", "Recommended_name", "Organism", "UniProt", "Commentary", "EC_Number")]
      table$Parameters <- 0
      for(n in 1:12){
        table <- do_function(n, extract, extractNotFound, addNoTable, input$attributes, attributeFound(), table = table)
      }
      table$Found_info <- table$Parameters
      if(fastaSearch()){
        ft <- fastaTable()
        ft$Found_using_Brenda <- as.numeric(ft$Found_using_Brenda)
        if(nrow(ft) != 0){
          ft <- aggregate(ft[,"Found_using_Brenda"],
                          by = list(ft$Enzyme,
                                    ft$Organism,
                                    ft$UniProt), sum)}
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
        if(nrow(pdb) != 0){
          pdb <- aggregate(pdb[,"PDB"],
                           by = list(pdb$EC_Number,
                                     pdb$Organism),
                           sum)}
        attributes(pdb)$names[1] <- "EC_Number"
        attributes(pdb)$names[2] <- "Organism"
        attributes(pdb)$names[3] <- "PDB"
        table <- merge(table, pdb, all = TRUE)
        table[is.na(table)] <- 0
        table$Found_info <- table$Found_info + table$PDB
      }
      infotable <- data.frame(Information = c(), n = c())
      infotable[1, "Information"] <- "Enzymes"
      infotable[1, "n"] <- paste(unique(table$EC_Number), sep = "; ", collapse = "; ")
      infotable[2, "Information"] <- "Organism"
      infotable[2, "n"] <- length(unique(table$Organism))
      infotable[3, "Information"] <- "Functional Parameters"
      total <- c()
      for(n in 1:12){
        infotable[n + 3, "Information"] <- paste("==>", nat[n])
        infotable[n + 3, "n"] <- do_function(n, infoSummary, notfoundInfoSummary, notSelectInfoSummary, input$attributes, attributeFound(), table = table)
        total <- c(total, as.double(infotable[n + 3, "n"]))
      }
      infotable[3, "n"] <- sum(na.omit(total))
      infotable[16, "Information"] <- "FASTA"
      if(fastaSearch()){infotable[16, "n"] <- sum(table$Sequence_FASTA)}
      else{infotable[16, "n"] <- "No searched"}
      infotable[17, "Information"] <- "PDB"
      if(pdbSearch()){infotable[17, "n"] <- sum(table$PDB)}
      else{infotable[17, "n"] <- "No searched"}
      table$EC_Number <- NULL
      table$Ref <- NULL
      summaryTable(table)
      infoTable(infotable)
    }
  })
  
  # Summary Table
  output$distSummaryTable <- DT::renderDT({
    DT::datatable(summaryTable(),
                  options = list(scrollX = TRUE,
                                 lengthMenu = c(5, 10, 50, 100),
                                 pageLength = 5))
  })
  
  output$informationTable <- renderTable({
    infoTable()
  },rownames = FALSE, colnames = FALSE)
  
  # Count Attributes
  countAttr <- function(n, ...){
    op <- list(...)
    vector <- op$vector
    table <- fattrTable()[[n]]
    u <- table$value
    u <- gsub("-999.0", NA, u)
    u <- gsub("-999", NA, u)
    u <- rep(nat[n], sum(
      as.numeric( !is.na(u) )
      ))
    v <- c(vector, u)
  }
  
  observeEvent(input$new_table, {
    if(input$new_table){
      shinyjs::show("uploadTable")
    } else {shinyjs::hide("uploadTable")}
  })
  
  # Generate Count plot
  observeEvent(input$visualize, {
    if(paramSearch()){
      updateTabItems(session, "inTabset", "histogram")
      x <- c()
      for(i in 1:12){
        x <- do_function(i, countAttr, addNoVector, addNoVector, input$attributes, attributeFound(), vector = x)
        }
      data <- data.frame(Parameters = x)
      p <- plot_ly(data, x = ~Parameters, color = ~Parameters, type = 'histogram', colors = seba_palette)
      histPlot(p)
    } else{noParameters("a", "Visualization")}
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
    param <- fattrTable()[[n]]
    if(nrow(param) != 0){
      p <- numericalValue(param)
      p <- getValue(p, "data")
      p <- p[,c("Ref", "data")]
      p <- labeling(p, table)}
    else{p <- param}
    if(nrow(p) != 0){ p$parameter <- nat[n] }
    rbind(data, p)
  }
  
  # Generate Distribution
  observeEvent(input$getDistribution, {
    if(paramSearch()){
      shinyjs::disable("getDistribution")
      updateTabItems(session, "inTabset", "distribution")
      withProgress(message = "Calculating distribution", value = 0, {
        incProgress(0, detail = "Loading tables")
        table <- proteinTable()
        table <- table[,c("Recommended_name", "Organism", "Ref")]
        data <- data.frame(parameter = c(), data = c(), Recommended_name = c(), Organism = c())
        for(i in 1:12){
          incProgress(0.5/12, detail = "Analysing tables")
          data <- do_function(i, distFunction, addNoData, addNoData, input$attributes, attributeFound(), table = table, data = data)
        }
        incProgress(0.4, detail = "Plotting")
        p <- plot_ly(data, x = ~data, color = ~parameter, colors = seba_palette,
                     type = "box",
                     text = ~paste(Recommended_name, Organism, sep = "\n"))
        incProgress(0.1, detail = "Ready")
        distributionPlot(p)
      })
      shinyjs::enable("getDistribution")
    } else{noParameters("a", "Distribution")}
  })
  
  # Distribution
  output$distributionOut <- renderPlotly({
    distributionPlot()
  })
  
  # Back to visualization
  observeEvent(input$backVisualization1, {
    updateTabItems(session, "inTabset", "histogram")
  })
  
  # Correlation
  correlationTable <- function(n, ...){
    tb <- simplify(fattrTable()[[n]], nat[n])
  }
  
  # Generate Correlation
  observeEvent(input$getCorrelation, {
    if(paramSearch()){
      shinyjs::toggle("getCorrelationMatrix")
      shinyjs::toggle("helpCorrelationMatrix")
      shinyjs::toggle("getCorrelationScatter")
      shinyjs::toggle("helpCorrelationScatter")
    } else{noParameters("a", "Correlation")}
  })
  
  # As a heatmap matrix
  observeEvent(input$getCorrelationMatrix, {#Check!!!
    if(paramSearch()){
      shinyjs::disable("getCorrelationMatrix")
      updateTabItems(session, "inTabset", "correlation")
      updateTabItems(session, "correlationPlot", "matrix")
      withProgress(message = "Calculating correlation...", value = 0, {
        if(!merged()){
          table <- proteinTable()[,c("Ref", "Recommended_name", "Organism")]
          tableList <- list()
          for(n in 1:12){
            tableList[[n]] <- do_function(n, correlationTable, noTable, noTable, input$attributes, attributeFound())
          }
          mergedTable <- mergeTable(tableList, table)
          groupMerging(mergedTable)
        }
        incProgress(0, detail = "Grouping tables")
        tableMerged <- groupTables(groupMerging())
        incProgress(0.25, detail = "Merging tables")
        tableMerged <- doMerge(tableMerged)
        tableMerged <- deleteMutantColumn(tableMerged)
        incProgress(0.25, detail = "Doing the correlation")
        m <- correlation(tableMerged, "pearson")
        incProgress(0.125, detail = "Binding matrices")
        m <- bindMatrix(m)
        incProgress(0.125, detail = "Ploting")
      })
      shinyjs::enable("getCorrelationMatrix")
      merged(TRUE)
      correlationPlot(m)
    } else{(noParameters("a", "Correlation"))}
  })
  
  # As a scatterplot
  observeEvent(input$getCorrelationScatter, { # THIS!!!
    if(paramSearch()){
      updateTabItems(session, "inTabset", "correlation")
      updateTabItems(session, "correlationPlot", "scatter")
      withProgress(message = "Ploting", value = 0, {
        if(!merged()){
          tableList <- list()
          for(n in 1:12){
            tableList[[n]] <- do_function(n, correlationTable, noTable, noTable, input$attributes, attributeFound())
          }
          incProgress(0, detail = "Grouping tables")
          table <- proteinTable()[,c("Ref", "Recommended_name", "Organism")]
          tableMerging <- mergeTables(tableList, table)
          incProgress(0.25, detail = "Merging tables")
          tableMerging <- doMerge(tableMerging)
          groupMerging(tableMerging)
        }
        ...
        incProgress(0.25, detail = "Doing the correlation")
        m <- correlation(tableMerging, "pearson")
        incProgress(0.125, detail = "Binding matrices")
        m <- bindMatrix(m)
        incProgress(0.125, detail = "Ploting")
      })
    } else{noParameters("a", "Correlation")}
  })
  
  # Back to visualization
  observeEvent(input$backVisualization2, {
    updateTabItems(session, "inTabset", "histogram")
  })
  observeEvent(input$backVisualization3, {
    updateTabItems(session, "inTabset", "histogram")
  })
  
  # Distribution
  output$correlationOut <- renderPlotly({
    m <- correlationPlot()
    if(input$correlationColor == "Default"){
      p <- plot_ly(x = colnames(m), y = rownames(m), z = m, colors = seba_palette2, type = 'heatmap')
    } else{
      p <- plot_ly(x = colnames(m), y = rownames(m), z = m, colors = input$correlationColor, type = 'heatmap')
    }
    p <- colorbar(p, limits = c(-1,1))
  })
  
  # Analysis
  observeEvent(input$analyze, {
    if(paramSearch()){
      updateTabItems(session, "inTabset", "cluster")
      updateTabItems(session, "cluster", "home")
    } else{noParameters("an", "Analysis")}
  })
  
  # Clustering
  observeEvent(input$toKMeans, {
    if(paramSearch()){updateTabItems(session, "clusterPlot", "k-means")}
    else{noParameters("a", "Clusterization")}
  })
  
  #K-means
  observeEvent(input$goKmeans, {
    s <- length(input$kmeans)
    if(!paramSearch()){noParameters("a", "Clusterization")}
    else if(s==2 | s==3){
      shinyjs::disable("goKmeans")
      withProgress(value = 0, message = "Clustering...", {
        incProgress(0, detail = "Determinating dimensions")
        kmeansDim(dimInfo(s, input$kmeans))
        table <- proteinTable()
        table <- table[,c("Ref", "Recommended_name", "Organism")]
        attr <- list()
        clu <- as.integer(input$kmeans)
        for(n in clu){
          tb <- correlationTable(n)
          tb <- unique(labeling(tb, table))
          attr <- list.append(attr, tb)
        }
        incProgress(0.1, detail = "Merging tables")
        cluster <- join_all(attr, type = "inner")
        cluster <- unique(cluster)
        incProgress(0.2, detail = "Preprocessing")
        to_cluster <- preKmeans(cluster)
        incProgress(0.1, detail = "Determinating k, number of clusters")
        if(nrow(to_cluster) <= 2){shinyalert("Not enough data", "We are sorry, but there are really very few common data beetween parameters")}
        else{
          out <- elbow(to_cluster)
          elbowPlot(out$p)
          incProgress(0.2, detail = "Do clusterization")
          data <- clusteringKmeans(to_cluster, cluster, out$k)
          incProgress(0.1, detail = "Generating and saving clusterization")
          to_save <- completeData(data, proteinTable())
          kmeansTable(to_save)
          incProgress(0.2, detail = "Ploting")
          datag <- data
          datag <- reduceData(2000, datag, session)
          if(s == 2){
            p <- plotingKmeans2d(datag, nat[clu[1]], nat[clu[2]])
          } else{
            p <- plotingKmeans(datag, nat[clu[1]], nat[clu[2]], nat[clu[3]])
          }
          kPlot(p)
          incProgress(0.1, detail = "Ready")
        }
      })
      shinyjs::enable("goKmeans")
    } else{kmeansError(s)
      kmeansDim(NULL)
      elbowPlot(NULL)
      kPlot(NULL)}
  })
  
  output$kmeansDimInfo <- renderUI({
    kmeansDim()
  })
  
  output$elbowKMeans <- renderPlot({
    elbowPlot()
  })
  output$kmeansPlot <- renderPlotly({
    kPlot()
  })
  
  # Download Parameter Table
  output$downloadKmeans <- downloadHandler(
    filename <- 'clusters.txt',
    content <- function(name){
      if(paramSearch()){
        table <- kmeansTable()
      }
      else{
        table <- NULL
      }
      write.table(table, name, quote = FALSE, row.names = FALSE, sep = "\t")
    }
  )
  
  # Links on the tutorial
  # Enzyme name section
  observeEvent(input$enzymeHelp, {
    updateTabItems(session, "inTabset", "enzyme")
  })
  
})