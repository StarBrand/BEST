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
  
  # Flags
  isThereUser <- reactiveVal(FALSE)
  proteinSearch <- reactiveVal(FALSE)
  pdbSearch <- reactiveVal(FALSE)
  fastaSearch <- reactiveVal(FALSE)
  paramSearch <- reactiveVal(FALSE)
  attributeFound <- reactiveVal(rep(FALSE, 12))
  
  #Standard readtable
  new_table <- function(name){
    read.table(name, header = TRUE, na.strings = "null", sep = "\t")
  }
  
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
  mwTable <- reactiveVal(NULL)
  ic50Table <- reactiveVal(NULL)
  kcTable <- reactiveVal(NULL)
  kiTable <- reactiveVal(NULL)
  kmTable <- reactiveVal(NULL)
  phoTable <- reactiveVal(NULL)
  phrTable <- reactiveVal(NULL)
  piTable <- reactiveVal(NULL)
  saTable <- reactiveVal(NULL)
  toTable <- reactiveVal(NULL)
  trTable <- reactiveVal(NULL)
  tonTable <- reactiveVal(NULL)
  
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
  
  
  # Disable user
  observeEvent(input$logIn, {
    isThereUser(TRUE)
    shinyjs::show("logOut")
    shinyjs::disable("mail")
    shinyjs::hide("pass")
    shinyjs::hide("logIn")
    updateTabItems(session, "inTabset", "enzyme")
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
    mwTable(NULL)
    ic50Table(NULL)
    kcTable(NULL)
    kiTable(NULL)
    kmTable(NULL)
    phoTable(NULL)
    phrTable(NULL)
    piTable(NULL)
    saTable(NULL)
    toTable(NULL)
    trTable(NULL)
    tonTable(NULL)
    histPlot(NULL)
    attributeFound(rep(FALSE, 12))
    histPlot(NULL)
    distributionPlot(NULL)
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
                      choices = subclass
                      )
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
    if(!isThereUser()){
      noSession()
    }
    a<-str_split(input$enzyme_name, "\t")
    b <- unlist(a)
    c <- str_split_fixed(b, ": ", 2)
    choices <- as.list(c[,1])
    choices <- setNames(choices, b)
    updateRadioButtons(session, "pick",
                       choices = choices)
    shinyjs::show("pick")
    shinyjs::show("enzymeNameFinal")
  })
  
  # Generate Protein Table
  generateProteinTable <- function(){
    proteinSearch(TRUE)
    fastaSearch(FALSE)
    pdbSearch(FALSE)
    paramSearch(FALSE)
    attributeFound(rep(FALSE, 12))
    updateTabItems(session, "inTabset", "proteinTable")
    #main <- .jnew("main.BrendaSOAP", ec_number(), user(), as.integer(0), as.integer(0))
    #main$getProtein()
    table <- new_table(paste(folder(), "table.txt", sep = ""))
    proteinTable(table)
  }
  
  # EC Number
  observeEvent(input$ecNumber, {
    if(!isThereUser()){
      noSession()
    }
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
    if(!isThereUser()){
      noSession()
    }
    else{
      ec_number(input$pick)
      generateProteinTable()
    }
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
    if(!proteinSearch()){
      noSearch("Sequence")
    }
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
      write.csv(table, name, quote = FALSE, row.names = FALSE, col.names = FALSE)
    }
  )
  
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
    if(!proteinSearch()){
      noSearch("PDB")
    }
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
  
  # Back to protein table
  observeEvent(input$toProtein2, {
    updateTabItems(session, "inTabset", "proteinTable")
  })
  
  # Create parameters table
  # With Molecule
  newParamTableMol <- function(name, tbl, attr_folder, file, mol, filterId, n){
    af <- attributeFound()
    table_name <- paste(attr_folder, file, sep = "")
    if(file.exists(table_name)){
      af[n] <- TRUE
      table <- new_table(table_name)
      update_filter(table, filterId, name)
      table <- table[,c("Ref", "value", mol, "Commentary", "Literature.PubmedID.")]
      tbl(table)
    }
    else{
      update_filter_notFound(filterId, name)
      af[n] <- FALSE
      tbl(NULL)
    }
    attributeFound(af)
  }
  
  # Without Molecule
  newParamTable <- function(name, tbl, attr_folder, file, filterId, n){
    af <- attributeFound()
    table_name <- paste(attr_folder, file, sep = "")
    if(file.exists(table_name)){
      af[n] <- TRUE
      table <- new_table(table_name)
      update_filter(table, filterId, name)
      table <- table[,c("Ref", "value", "Commentary", "Literature.PubmedID.")]
      tbl(table)
    }
    else{
      update_filter_notFound(filterId, name)
      af[n] <- FALSE
      tbl(NULL)
    }
    attributeFound(af)
  }
  
  # Collapse table
  # With Molecule
  attr_collapse_mol <- function(name, table, molecule){
    out <- table
    attributes(out)$names <- paste(name, attributes(out)$names, sep ="_")
    attributes(out)$names[1] <- "Ref"
    out <- aggregate(out[,2:5], by=list(table$Ref), paste, collapse=";")
    attributes(out)$names[1] <- "Ref"
    out
  }
  
  # Without Molecule
  attr_collapse <- function(name, table){
    out <- table
    attributes(out)$names <- paste(name, attributes(out)$names, sep ="_")
    attributes(out)$names[1] <- "Ref"
    out <- aggregate(out[,2:4], by=list(table$Ref), paste, collapse=";")
    attributes(out)$names[1] <- "Ref"
    out
  }
  
  # Add to output Table
  # Table with molecule
  addTable_withMol <- function(name, paramTable, mol, table){
    out <- attr_collapse_mol(
      name,
      paramTable,
      mol)
    table <- merge(table, out, by="Ref", all = TRUE)
  }
  
  # Table without molecule
  addTable <- function(name, paramTable, table){
    out <- attr_collapse(
      name,
      paramTable)
    table <- merge(table, out, by="Ref", all = TRUE)
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
  
  # Update filter
  update_filter <- function(table, filterId, name){
    label <- paste(name, " filter")
    a <- min_max(table$value)
    s <- (a[2] - a[1])/40
    updateSliderInput(session, filterId, label,
                      value = a, min = a[1],
                      max = a[2], step = s)
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
      updateCheckboxInput(session, "mw", value = TRUE)
      updateCheckboxInput(session, "ic50", value = TRUE)
      updateCheckboxInput(session, "kc", value = TRUE)
      updateCheckboxInput(session, "ki", value = TRUE)
      updateCheckboxInput(session, "km", value = TRUE)
      updateCheckboxInput(session, "pho", value = TRUE)
      updateCheckboxInput(session, "phr", value = TRUE)
      updateCheckboxInput(session, "pi", value = TRUE)
      updateCheckboxInput(session, "sa", value = TRUE)
      updateCheckboxInput(session, "to", value = TRUE)
      updateCheckboxInput(session, "tr", value = TRUE)
      updateCheckboxInput(session, "ton", value = TRUE)
    }
    else{
      updateCheckboxInput(session, "mw", value = FALSE)
      updateCheckboxInput(session, "ic50", value = FALSE)
      updateCheckboxInput(session, "kc", value = FALSE)
      updateCheckboxInput(session, "ki", value = FALSE)
      updateCheckboxInput(session, "km", value = FALSE)
      updateCheckboxInput(session, "pho", value = FALSE)
      updateCheckboxInput(session, "phr", value = FALSE)
      updateCheckboxInput(session, "pi", value = FALSE)
      updateCheckboxInput(session, "sa", value = FALSE)
      updateCheckboxInput(session, "to", value = FALSE)
      updateCheckboxInput(session, "tr", value = FALSE)
      updateCheckboxInput(session, "ton", value = FALSE)
    }
  })
  
  # Generate Parameter Table
  observeEvent(input$parameters, {
    paramSearch(TRUE)
    if(proteinSearch()){
    updateTabItems(session, "inTabset", "parameterTable")
    #main <- .jnew("main.BrendaSOAP", ec_number(), user(), as.integer(encoder_param()), as.integer(encoder_filter()))
    #main$getProtein()
    s <- input$distProteinTable_rows_selected
    int <- as.integer(c())
    if(length(s)){
      int <- as.integer(c(s - 1, int))
    }
    int <- .jarray(int)
    #main$getParameters(int, input$allProteins)
    attr_folder <- paste(folder(), "attributes\\", sep = "")
    table <- new_table(paste(folder(),"table.txt", sep = ""))
    proteinTable(table)
    if(input$mw){
      newParamTable("Molecular Weight", mwTable, attr_folder, "Molecular Weight.txt", "mwFilter", 1)
      if(attributeFound()[1]){
        table <- addTable("Molecular_Weight",
                          mwTable(),
                          table)
      }
    }
    if(input$ic50){
      newParamTableMol("IC50", ic50Table, attr_folder, "IC50.txt", "Inhibitor", "ic50Filter", 2)
      if(attributeFound()[2]){
        table <- addTable_withMol("IC50",
                                  ic50Table(),
                                  "Inhibitor",
                                  table)
      }
    }
    if(input$kc){
      newParamTableMol("Kcat/Km", kcTable, attr_folder, "Kcat_Km.txt", "Substrate", "kcFilter", 3)
      if(attributeFound()[3]){
        table <- addTable_withMol("Kcat_Km",
                                  kcTable(),
                                  "Substrate",
                                  table)
      }
    }
    if(input$ki){
      newParamTableMol("Ki", kiTable, attr_folder, "Ki.txt", "Inhibitor", "kiFilter", 4)
      if(attributeFound()[4]){
        table <- addTable_withMol("Ki",
                                  kcTable(),
                                  "Inhibitor",
                                  table)
      }
    }
    if(input$km){
      newParamTableMol("Km", kmTable, attr_folder, "Km.txt", "Substrate", "kmFilter", 5)
      #kmTable()$Substrate <- gsub("more", "1,4-beta-D-xylan", kmTable()$Substrate)
      if(attributeFound()[5]){
        table <- addTable_withMol("Km",
                                  kmTable(),
                                  "Substrate",
                                  table)
      }
    }
    if(input$pho){
      newParamTable("pH Optimum", phoTable, attr_folder, "pH Optimum.txt", "phoFilter", 6)
      if(attributeFound()[6]){
        table <- addTable("pH_Optimum",
                          phoTable(),
                          table)
      }
    }
    if(input$phr){
      newParamTable("pH Range", phrTable, attr_folder, "pH Range.txt", "phrFilter", 7)
      if(attributeFound()[7]){
        table <- addTable("pH_Range",
                          phoTable(),
                          table)
      }
    }
    if(input$pi){
      newParamTable("pI", piTable, attr_folder, "pI.txt", "piFilter", 8)
      if(attributeFound()[8]){
        table <- addTable("pI",
                          piTable(),
                          table)
      }
    }
    if(input$sa){
      newParamTable("Specific Activity", saTable, attr_folder, "Specific Activity.txt", "saFilter", 9)
      if(attributeFound()[9]){
        table <- addTable("Specific_Activity",
                          saTable(),
                          table)
      }
    }
    if(input$to){
      newParamTable("Temperature Optimum", toTable, attr_folder, "Temperature Optimum.txt", "toFilter", 10)
      if(attributeFound()[10]){
        table <- addTable("Temperature_Optimum",
                          toTable(),
                          table)
      }
    }
    if(input$tr){
      newParamTable("Temperature Range", trTable, attr_folder, "Temperature Range.txt", "trFilter", 11)
      if(attributeFound()[11]){
        table <- addTable("Temperature_Range",
                          trTable(),
                          table)
      }
    }
    if(input$ton){
      newParamTableMol("Turnover Number", tonTable, attr_folder, "Turnover Number.txt", "Substrate", "tonFilter", 12)
      if(attributeFound()[12]){
        table <- addTable_withMol("Turnover Number",
                                  tonTable(),
                                  "Substrate",
                                  table)
      }
    }
    table <- table[sort(table$Ref, decreasing = FALSE),]
    table <- data.frame(lapply(table, function(i){
      gsub("-999.0", "Aditional Information", i)}))
    table <- data.frame(lapply(table, function(i){
      gsub("-999", "Aditional Information", i)}))
    table$Ref <- NULL
    table
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
  
  # Back to protein table
  observeEvent(input$toProtein3, {
    updateTabItems(session, "inTabset", "proteinTable")
  })

  # Extract amount information
  extract <- function(name, attr_table, table, n){
    if(attributeFound()[n]){
      out <- attr_table
      out$value <- 1
      out <- aggregate(out[,"value"],
                       by = list(out$Ref),
                       sum)
      attributes(out)$names[1] <- "Ref"
      attributes(out)$names[2] <- name
      out <- merge(table, out, all = TRUE)
      out[is.na(out)] <- 0
      out$Parameters <- out$Parameters + out[,name]
    }
    else{
      out <- table
      out[,name] <- "Parameter Not Found"
    }
    out
  }
  
  # Filter function
  filterParam <- function(param, param_filter, n){
    if(attributeFound()[n]){
      p <- with(param, param[value != "-999.0",])
      p <- with(p, p[value != "-999",])
      p$min <- lapply(p$value,
                      function(i){
                        str_split(i, "-")[[1]][1]
                        })
      p$min <- as.double(p$min)
      p$max <- lapply(p$value,
                      function(i){
                        str_split(i, "-")[[1]][2]
                        })
      p_s <- with(p, p[!grepl("-", value),])
      p_r <- with(p, p[grepl("-", value),])
      # Filter range
      p_r$max <- as.double(p_r$max)
      p_r <- with(p_r, p_r[min >= f[1] & max <= f[2],])
      # Filter single value
      p_s <- with(p_s, p_s[min >= f[1] & min <= f[2],])
      p <- rbind(p_s, p_r)
      p$min <- NULL
      p$max <- NULL
      p
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
    if(input$mw & input$mw2){
      table2 <- filterParam(mwTable(), input$mwFilter, 1)
      mwTable(table2)
      table2 <- attr_collapse("Molecular_Weight", table2)
      table <- merge(table, table2)
    } else{
      table2 <- attr_collapse("Molecular_Weight", mwTable())
      table <- merge(table, table2, all = TRUE)
    }
    #if(input$ic50 & input$ic502){
      #table2 <- filterParam(ic50Table(), input$ic50Filter, 2)
      #ic50Table(table2)
      #table2 <- attr_collapse_mol("IC50", table2, "Inhibitor")
      #table <- merge(table, table2)
    #} else{
      #table2 <- attr_collapse_mol("IC50", ic50Table(), "Inhibitor")
      #table <- merge(table, table2, all = TRUE)
    #}
    if(input$kc & input$kc2){
      table2 <- filterParam(kcTable(), input$kcFilter, 3)
      kcTable(table2)
      table2 <- attr_collapse_mol("Kcat/Km", table2, "Substrate")
      table <- merge(table, table2)
    } else{
      table2 <- attr_collapse_mol("Kcat/Km", kcTable(), "Substrate")
      table <- merge(table, table2, all = TRUE)
    }
    if(input$ki & input$ki2){
      table2 <- filterParam(kiTable(), input$kiFilter, 4)
      kiTable(table2)
      table2 <- attr_collapse_mol("Ki", table2, "Substrate")
      table <- merge(table, table2)
    } else{
      table2 <- attr_collapse_mol("Ki", kiTable(), "Substrate")
      table <- merge(table, table2, all = TRUE)
    }
    if(input$km & input$km2){
      table2 <- filterParam(kmTable(), input$kmFilter, 5)
      kmTable(table2)
      table2 <- attr_collapse_mol("Km", table2, "Substrate")
      table <- merge(table, table2)
    } else{
      table2 <- attr_collapse_mol("Km", kmTable(), "Substrate")
      table <- merge(table, table2, all = TRUE)
    }
    if(input$pho & input$pho2){
      table2 <- filterParam(phoTable(), input$phoFilter, 6)
      phoTable(table2)
      table2 <- attr_collapse("pH_Optimum", table2)
      table <- merge(table, table2)
    } else{
      table2 <- attr_collapse("pH_Optimum", phoTable())
      table <- merge(table, table2, all = TRUE)
    }
    if(input$phr & input$phr2){
      table2 <- filterParam(phrTable(), input$phrFilter, 7)
      phrTable(table2)
      table2 <- attr_collapse("pH_Range", table2)
      table <- merge(table, table2)
    } else{
      table2 <- attr_collapse("pH_Range", phrTable())
      table <- merge(table, table2, all = TRUE)
    }
    if(input$pi & input$pi2){
      table2 <- filterParam(piTable(), input$piFilter, 8)
      piTable(table2)
      table2 <- attr_collapse("pI", table2)
      table <- merge(table, table2)
    } else{
      table2 <- attr_collapse("pi", piTable())
      table <- merge(table, table2, all = TRUE)
    }
    if(input$sa & input$sa2){
      table2 <- filterParam(saTable(), input$saFilter, 9)
      saTable(table2)
      table2 <- attr_collapse("Specific_Activity", table2)
      table <- merge(table, table2)
    } else{
      table2 <- attr_collapse("Specific_Activity", saTable())
      table <- merge(table, table2, all = TRUE)
    }
    if(input$to & input$to2){
      table2 <- filterParam(toTable(), input$toFilter, 10)
      toTable(table2)
      table2 <- attr_collapse("Temperature_Optimum", table2)
      table <- merge(table, table2)
    } else{
      table2 <- attr_collapse("Temperature_Optimum", toTable())
      table <- merge(table, table2, all = TRUE)
    }
    if(input$tr & input$tr2){
      table2 <- filterParam(trTable(), input$trFilter, 11)
      trTable(table2)
      table2 <- attr_collapse("Temperature_Range", table2)
      table <- merge(table, table2)
    } else{
      table2 <- attr_collapse("Temperature_Range", trTable())
      table <- merge(table, table2, all = TRUE)
    }
    if(input$ton & input$ton2){
      table2 <- filterParam(tonTable(), input$tonFilter, 12)
      tonTable(table2)
      table2 <- attr_collapse_mol("Turnover_Number", table2, "Substrate")
      table <- merge(table, table2)
    } else{
      table2 <- attr_collapse_mol("Turnover_Number", tonTable(), "Substrate")
      table <- merge(table, table2, all = TRUE)
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
      if(input$mw){
        table <- extract("Molecular_Weight", mwTable(), table, 1)
      }
      if(input$ic50){
        table <- extract("IC50", ic50Table(), table, 2)
      }
      if(input$kc){
        table <- extract("Kcat/Km", kcTable(), table, 3)
      }
      if(input$ki){
        table <- extract("Ki", kiTable(), table, 4)
      }
      if(input$km){
        table <- extract("Km", kmTable(), table, 5)
      }
      if(input$pho){
        table <- extract("pH_Optimum", phoTable(), table, 6)
      }
      if(input$phr){
        table <- extract("pH_Range", phrTable(), table, 7)
      }
      if(input$pi){
        table <- extract("pI", piTable(), table, 8)
      }
      if(input$sa){
        table <- extract("Specific_Activity", saTable(), table, 9)
      }
      if(input$to){
        table <- extract("Temperature_Optimum", toTable(), table, 10)
      }
      if(input$tr){
        table <- extract("Temperature_Range", trTable(), table, 11)
      }
      if(input$ton){
        table <- extract("Turnover_Number", tonTable(), table, 12)
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
  countAttr <- function(table, name, vector){
    u <- rep(name,
             sum( as.numeric( !is.na(
               gsub("-999", NA, table$value)
               ))))
    v <- c(vector, u)
  }
  
  # Generate Count plot
  observeEvent(input$visualize, {
    updateTabItems(session, "inTabset", "histogram")
    x = c()
    if(input$mw){
      x <- countAttr(mwTable(),
                     "Molecular_Weight",
                     x)
    }
    if(input$ic50){
      x <- countAttr(ic50Table(),
                     "IC50",
                     x)
    }
    if(input$kc){
      x <- countAttr(kcTable(),
                     "Kcat_Km",
                     x)
    }
    if(input$ki){
      x <- countAttr(kiTable(),
                     "Ki",
                     x)
    }
    if(input$km){
      x <- countAttr(kmTable(),
                     "Km",
                     x)
    }
    if(input$pho){
      x <- countAttr(phoTable(),
                     "pH_Optimum",
                     x)
    }
    if(input$phr){
      x <- countAttr(phrTable(),
                     "pH_Range",
                     x)
    }
    if(input$pi){
      x <- countAttr(piTable(),
                     "pI",
                     x)
    }
    if(input$sa){
      x <- countAttr(saTable(),
                     "Specific_Activity",
                     x)
    }
    if(input$to){
      x <- countAttr(toTable(),
                     "Temperature_Optimum",
                     x)
    }
    if(input$tr){
      x <- countAttr(trTable(),
                     "Temperature_Range",
                     x)
    }
    if(input$ton){
      x <- countAttr(tonTable(),
                     "Temperature_Range",
                     x)
    }
    data <- data.frame("Parameters" = x)
    p <- plot_ly(data, x = ~Parameters, type = 'histogram', colors = "BuGn")
    histPlot(p)
  })
  
  # Count Plot
  output$histogram <- renderPlotly({
    histPlot()
  })
  
  # Distribution function
  distFunction <- function(param, table, name){
    p <- with(param, param[value != "-999.0",])
    p <- with(p, p[value != "-999",])
    p$min <- lapply(p$value,
                    function(i){
                      str_split(i, "-")[[1]][1]
                    })
    p$min <- as.double(p$min)
    p$max <- lapply(p$value,
                    function(i){
                      str_split(i, "-")[[1]][2]
                    })
    p_s <- with(p, p[!grepl("-", value),])
    p_r <- with(p, p[grepl("-", value),])
    # Filter range
    p_r$max <- as.double(p_r$max)
    p_s$data <- p_s$min
    p_r$data <- (p_r$max - p_r$min) / 2
    p <- rbind(p_s, p_r)
    p <- p[,c("Ref", "data")]
    p$Recommended_name <- sapply(p$Ref, function(x){with(table, table[Ref == x, "Recommended_name"])})
    p$Organism <- sapply(p$Ref, function(x){with(table, table[Ref == x, "Organism"])})
    p$Ref <- NULL
    p$parameter <- name
    p
  }
  
  # Generate Distribution
  observeEvent(input$getDistribution, {
    updateTabItems(session, "inTabset", "distribution")
    table <- proteinTable()
    table <- table[,c("Recommended_name", "Organism", "Ref")]
    data <- data.frame(parameter = c(), data = c(), Recommended_name = c(), Organism = c())
    if(input$mw){
      data <- rbind(data, distFunction(mwTable(), table, "Molecular Weight"))
    }
    if(input$ic50){
      data <- rbind(data, distFunction(ic50Table(), table, "IC50"))
    }
    if(input$kc){
      data <- rbind(data, distFunction(kcTable(), table, "Kcat/Km"))
    }
    if(input$ki){
    }
    if(input$km){
      data <- rbind(data, distFunction(kmTable(), table, "Km"))
    }
    if(input$pho){
    }
    if(input$phr){
    }
    if(input$pi){
    }
    if(input$sa){
    }
    if(input$to){
    }
    if(input$tr){
    }
    if(input$ton){
    }
    p <- plot_ly(data, x = ~data, color = ~parameter, colors = c("Blue", "Purple", "Green", "Red", "Black"),
                 type = "box", mode = "markers",
                 text = ~paste(Recommended_name, Organism, sep = "\n"))
    distributionPlot(p)
  })
  
  
  # Distribution
  output$distributionOut <- renderPlotly({
    distributionPlot()
  })
  
})