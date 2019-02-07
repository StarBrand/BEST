#
# This is the server logic of a Shiny web application. You can run the 
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
  
  isThereUser <- reactiveVal(FALSE)
  
  #Standard readtable
  new_table <- function(name){
    read.table(name, header = TRUE, na.strings = "null", sep = "\t")
  }
  
  # Errors
  # ECnumber
  observeEvent(input$ecNumber, {
    if(!isThereUser()){
      shinyalert("Enter Brenda User",
                 "You won't be avaiable to search enzymes if we haven't your Brenda account",
                 type = "error")
    }
  })
  
  # Enzyme Name Search
  observeEvent(input$enzymeName, {
      if(!isThereUser()){
        shinyalert("Enter Brenda User",
                   "You won't be avaiable to search enzymes if we haven't your Brenda account",
                   type = "error")
      }
  })
  
  # Enzyme Name
  observeEvent(input$enzymeNameFinal, {
      if(!isThereUser()){
        shinyalert("Enter Brenda User",
                   "You won't be avaiable to search enzymes if we haven't your Brenda account",
                   type = "error")
      }
  })
  
  # To tutorial section
  observeEvent(input$toUser, {
    updateTabItems(session, "inTabset", "account")
  })
  
  # To tutorial section
  observeEvent(input$help, {
    updateTabItems(session, "inTabset", "help")
  })
  
  # Create user
  user <- eventReactive(input$logIn, {
    .jnew("client.User",input$mail, input$pass)
  })
  
  
  # Disable user
  observeEvent(input$logIn, {
    isThereUser(TRUE)
    shinyjs::show("logOut")
    shinyjs::disable("mail")
    shinyjs::hide("pass")
    shinyjs::hide("logIn")
  })
  
  # Enable user
  observeEvent(input$logOut, {
    isThereUser(FALSE)
    shinyjs::show("logIn")
    shinyjs::enable("mail")
    shinyjs::show("pass")
    shinyjs::hide("logOut")
    shinyjs::hide("pick")
    shinyjs::hide("enzymeNameFinal")
    updateTextInput(session, "mail", value = "")
    updateTextInput(session, "pass", value = "")
  })
  
  # Folder
  folder <- reactiveVal("_results\\")
  
  # Set folder
  observeEvent(input$logIn, {
    new_folder <- paste(input$mail, "_results\\", sep = "")
    folder(new_folder)
  })
  
  # Endoder function for Parameter Query
  encoder_param<-function(){
    cond <- 0
    if(input$mw){
      cond <- cond + 1
    }
    if(input$ic50){
      cond <- cond + 2
    }
    if(input$kc){
      cond <- cond + 4
    }
    if(input$ki){
      cond <- cond + 8
    }
    if(input$km){
      cond <- cond + 16
    }
    if(input$pho){
      cond <- cond + 32
    }
    if(input$phr){
      cond <- cond + 64
    }
    if(input$pi){
      cond <- cond + 128
    }
    if(input$sa){
      cond <- cond + 256
    }
    if(input$to){
      cond <- cond + 512
    }
    if(input$tr){
      cond <- cond + 1024
    }
    if(input$ton){
      cond <- cond + 2048
    }
    cond
  }
  
  # Endoder function for Parameter Query
  encoder_filter<-function(){
    cond <- 0
    if(input$up){
      cond <- cond + 1
    }
    cond
  }
  
  # Change to enzyme
  observeEvent(input$logIn, {
    updateTabItems(session, "inTabset", "enzyme")
  })
  
  # Enter EC Number functions
  # EC Number 2
  observeEvent(input$ec_number1, {
    updateSelectizeInput(session, "ec_number2",
                         choices = read.table(
                           paste("ecNumber//", input$ec_number1,
                                 "//select.txt", sep = ""),
                           sep = "\t",
                           header = FALSE,
                         col.names = "2")[1])
  })
  # EC Number3
  observeEvent(input$ec_number2, {
    updateSelectizeInput(session, "ec_number3",
                         choices = read.table(
                           paste("ecNumber//", input$ec_number1,
                                 "//", input$ec_number2,
                                 "//select.txt", sep = ""),
                           sep = "\t",
                           header = FALSE,
                           col.names = "3")[1])
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
                           header = FALSE,
                           col.names = "4")[1])
  })
  
  # EC Number
  ec_number <- reactiveVal("1.1.1.1")
  
  # Update selector
  observeEvent(input$subclass, {
    file <- paste("synonyms", input$subclass, ".txt", sep = "")
    updateSelectizeInput(session, "enzyme_name",
                         choices = read.table(file,
                                        sep = "\t",
                                        header = FALSE)[,1]
    )
  })
  
  # Choice autocomplite
  choice <- reactiveVal(NULL)
  
  # Search
  observeEvent(input$enzymeName, {
    file <- paste("synonyms", input$subclass, ".txt", sep = "")
    choice1 <- read.table(file, sep = "\t", header = FALSE)
    new <- (with(choice1, choice1[V1 == input$enzyme_name,]))[2]
    new_choice <- new[[1]]
    choice(new_choice)
    updateRadioButtons(session, "pick",
                       choices = choice())
    shinyjs::show("pick")
    shinyjs::show("enzymeNameFinal")
  })
  
  # Change to Protein Table
  # EC Number
  observeEvent(input$ecNumber, {
    if(isThereUser()){
      new_ec_number <- paste(input$ec_number1,
                             input$ec_number2,
                             input$ec_number3,
                             input$ec_number4,
                             sep = ".")
      ec_number(new_ec_number)
      updateTabItems(session, "inTabset", "proteinTable")
    }
  })
  
  # Enzyme Name
  observeEvent(input$enzymeNameFinal, {
    if(isThereUser()){
      new_ec_number <- input$pick
      ec_number(new_ec_number)
      updateTabItems(session, "inTabset", "proteinTable")
    }
  })
  
  # Trigger Protein Table
  proteinTable <- reactiveVal(NULL)
  
  # EC Number
  observeEvent(input$ecNumber,{
    main <- .jnew("main.BrendaSOAP", ec_number(), user(), as.integer(0), as.integer(0))
    main$getProtein()
    table <- new_table(paste(folder(), "table.txt", sep = ""))
    proteinTable(table)
  })
  
  # Enzyme Name
  observeEvent(input$enzymeNameFinal,{
    main <- .jnew("main.BrendaSOAP", ec_number(), user(), as.integer(0), as.integer(0))
    main$getProtein()
    table <- new_table(paste(folder(), "table.txt", sep = ""))
    proteinTable(table)
  })
  
  # Protein Table
  output$distProteinTable <- DT::renderDT({
    DT::datatable(proteinTable(), options = list(scrollX = TRUE, lengthMenu = c(5, 10, 50, 100), pageLength = 5))
  })
  
  # Change to Fasta Table
  observeEvent(input$toFasta, {
    updateTabItems(session, "inTabset", "fasta")
  })
  
  # Trigger Fasta Table
  triggerFastaTable <- eventReactive(input$toFasta, {
    main <- .jnew("main.BrendaSOAP", ec_number(), user(), as.integer(0), as.integer(0))
    main$getProtein()
    main$getFastaSequence()
    table <- new_table(paste(folder(),"report_fasta.txt", sep = ""))
  })
  
  # Fasta Table
  output$fastaTable <- DT::renderDT({
    DT::datatable(triggerFastaTable(),
                  options = list(scrollX = TRUE,
                                 lengthMenu = c(5, 10, 50, 100),
                                 pageLength = 10))
  })
  
  
  # Change to PDB
  observeEvent(input$toPDB, {
    updateTabItems(session, "inTabset", "pdb")
  })
  
  # Create Link Function
  # [thanks to williamsurles on StackOverflow]
  createLink <- function(val) {
    paste("<a href=", val, ">", val, "</a>", sep = "")
  }
  
  # Trigger PDB Table
  triggerPDBTable <- eventReactive(input$toPDB, {
    main <- .jnew("main.BrendaSOAP", ec_number(), user(), as.integer(0), as.integer(0))
    main$getProtein()
    main$getPDB()
    table <- new_table(paste(folder(),"pdb_table.txt", sep = ""))
    table$link <- lapply(table$link, function(i){
      createLink(i)})
    table
  })
  
  # PDB Table
  output$pdbTable <- DT::renderDT({
    DT::datatable(triggerPDBTable(),
                  options = list(scrollX = TRUE,
                                 lengthMenu = c(5, 10, 50, 100),
                                 pageLength = 10))
  }, escape = FALSE)
  
  # Change to Parameter Table
  observeEvent(input$parameters, {
    updateTabItems(session, "inTabset", "parameterTable")
  })
  
  # Trigger Parameter Table
  triggerParamTable <- eventReactive(input$parameters, {
    main <- .jnew("main.BrendaSOAP", ec_number(), user(), as.integer(encoder_param()), as.integer(encoder_filter()))
    main$getProtein()
    s <- input$distProteinTable_rows_selected
    int <- as.integer(c())
    if(length(s)){
      int <- as.integer(c(s - 1, int))
    }
    int <- .jarray(int)
    main$getParameters(int, input$allProteins)
    attr_folder <- paste(folder(), "attributes\\", sep = "")
    table2 <- new_table(paste(folder(),"table.txt", sep = ""))
    if(input$mw){
      table2 <- addTable("Molecular_Weight",
                         attr_folder,
                         "Molecular Weight.txt",
                         table2,
                         "mwFilter")
    }
    if(input$ic50){
      table2 <- addTable_withMol("IC50",
                                 attr_folder,
                                 "IC50.txt",
                                 "Inhibitor",
                                 table2,
                                 "ic50Filter")
    }
    if(input$kc){
      table2 <- addTable_withMol("Kcat_Km",
                                 attr_folder,
                                 "Kcat_Km.txt",
                                 "Substrate",
                                 table2,
                                 "kcFilter")
    }
    if(input$ki){
      table2 <- addTable_withMol("Ki",
                                 attr_folder,
                                 "Ki.txt",
                                 "Inhibitor",
                                 table2,
                                 "kiFilter")
    }
    if(input$km){
      table2 <- addTable_withMol("Km",
                                 attr_folder,
                                 "Km.txt",
                                 "Substrate",
                                 table2,
                                 "kmFilter")
      table2$Km_Substrate <- gsub("more", "1,4-beta-D-xylan", table2$Km_Substrate)
    }
    if(input$pho){
      table2 <- addTable("pH_Optimum",
                         attr_folder,
                         "pH Optimum.txt",
                         table2,
                         "phoFilter")
    }
    if(input$phr){
      table2 <- addTable("pH_Range",
                         attr_folder,
                         "pH Range.txt",
                         table2,
                         "phrFilter")
    }
    if(input$pi){
      table2 <- addTable("pI",
                         attr_folder,
                         "pI.txt",
                         table2,
                         "piFilter")
    }
    if(input$sa){
      table2 <- addTable("Specific_Activity",
                         attr_folder,
                         "Specific Activity.txt",
                         table2,
                         "saFilter")
    }
    if(input$to){
      table2 <- addTable("Temperature_Optimum",
                         attr_folder,
                         "Temperature Optimum.txt",
                         table2,
                         "toFilter")
    }
    if(input$tr){
      table2 <- addTable("Temperature_Range",
                         attr_folder,
                         "Temperature Range.txt",
                         table2,
                         "trFilter")
    }
    if(input$ton){
      table2 <- addTable_withMol("Turnover_Number",
                                 attr_folder,
                                 "Turnover Number.txt",
                                 "Substrate",
                                 table2,
                                 "tonFilter")
    }
    table2 <- table2[sort(table2$Ref, decreasing = FALSE),]
    table2 <- data.frame(lapply(table2, function(i){
      gsub("-999", "Aditional Information", i)}))
    table2$Ref <- NULL
    table2
  })
  
  # Collapse table
  # With Molecule
  attr_collapse_mol <- function(name, table, molecule){
    out <- table[,c("Ref", "value", molecule, "Commentary", "Literature.PubmedID.")]
    attributes(out)$names <- paste(name, attributes(out)$names, sep ="_")
    attributes(out)$names[1] <- "Ref"
    out <- aggregate(out[,2:5], by=list(table$Ref), paste, collapse=";")
    attributes(out)$names[1] <- "Ref"
    out
  }
  
  # Without Molecule
  attr_collapse <- function(name, table){
    out <- table[,c("Ref", "value", "Commentary", "Literature.PubmedID.")]
    attributes(out)$names <- paste(name, attributes(out)$names, sep ="_")
    attributes(out)$names[1] <- "Ref"
    out <- aggregate(out[,2:4], by=list(table$Ref), paste, collapse=";")
    attributes(out)$names[1] <- "Ref"
    out
  }
  
  # Calculate the min max
  min_max <- function(vectorIn){
    use <- gsub("-999", NA, vectorIn)
    use <- str_split(use, ";")
    use <- unlist(use, recursive = TRUE)
    use <- str_split(use, "-")
    use <- unlist(use, recursive = TRUE)
    use <- sapply(use, 'as.double')
    a <- c(min(use, na.rm = TRUE), max(use, na.rm = TRUE))
  }
  
  # Add to output Table
  # Table with molecule
  addTable_withMol <- function(name, attr_folder, file, mol, table, filterId){
    table1 <- new_table(paste(attr_folder, file, sep = ""))
    update_filter(table1, filterId)
    table1 <- attr_collapse_mol(
      name,
      table1,
      mol)
    table <- merge(table, table1, by="Ref", all = TRUE)
  }
  
  # Table without molecule
  addTable <- function(name, attr_folder, file, table, filterId){
    table1 <- new_table(paste(attr_folder, file, sep = ""))
    update_filter(table1, filterId)
    table1 <- attr_collapse(
      name,
      table1)
    table <- merge(table, table1, by="Ref", all = TRUE)
  }
  
  # Update filter
  update_filter <- function(table, filterId){
  a <- min_max(table$value)
  s <- (a[2] - a[1])/40
  updateSliderInput(session, filterId, value = a,
                    min = a[1], max = a[2], step = s)
  }
  
  # Parameter Table
  output$distParameterTable <- DT::renderDT({
    DT::datatable(triggerParamTable(),
                  options = list(scrollX = TRUE,
                                 lengthMenu = c(5, 10, 50, 100),
                                 pageLength = 5))
  })
  
  # Count Attributes
  countAttr <- function(file, name, vector){
    attr_folder <- paste(folder(), "attributes\\", sep = "")
    table <- new_table(paste(attr_folder, file, sep = ""))
    u <- rep(name,
             sum( as.numeric( !is.na(
               gsub("-999", NA, table$value)
               ))))
    v <- c(vector, u)
  }
  
  output$histogram <- renderPlotly({
    x = c()
    if(input$mw){
      x <- countAttr("Molecular Weight.txt",
                     "Molecular_Weight",
                     x)
    }
    if(input$ic50){
      x <- countAttr("IC50.txt",
                     "IC50",
                     x)
    }
    if(input$kc){
      x <- countAttr("Kcat_Km.txt",
                     "Kcat_Km",
                     x)
    }
    if(input$ki){
      x <- countAttr("Ki.txt",
                     "Ki",
                     x)
    }
    if(input$km){
      x <- countAttr("Km.txt",
                     "Km",
                     x)
    }
    if(input$pho){
      x <- countAttr("pH Optimum.txt",
                     "pH_Optimum",
                     x)
    }
    if(input$phr){
      x <- countAttr("pH Range.txt",
                     "pH_Range",
                     x)
    }
    if(input$pi){
      x <- countAttr("pI.txt",
                     "pI",
                     x)
    }
    if(input$sa){
      x <- countAttr("Specific Activity.txt",
                     "Specific_Activity",
                     x)
    }
    if(input$to){
      x <- countAttr("Temperature Optimum.txt",
                     "Temperature_Optimum",
                     x)
    }
    if(input$tr){
      x <- countAttr("Temperature Range.txt",
                     "Temperature_Range",
                     x)
    }
    if(input$ton){
      x <- countAttr("Temperature Range.txt",
                     "Temperature_Range",
                     x)
    }
    plot_ly(x = x, type = 'histogram')
    })
  
})