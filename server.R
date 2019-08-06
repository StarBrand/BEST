## This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

source("resources/libs.R")
.jinit("resources/BrendaSOAP.jar")
source("methods/initials.R")
source("methods/utils.R")
source("methods/errors.R")
source("methods/tree.R")
source("methods/distribution.R")
source("methods/correlation.R")
source("methods/clustering.R")

# Define server logic
shinyServer(function(input, output, session) {
  
  # Functions on server
  # *** This function needs server values, therefore, can't ***
  # ******** be defined outside shiny server function *********
  
  ## Default elements hidden on ui
  hideStuffs <- function(){
    for(n in 1:12){shinyjs::hide(paste(at[n], "Filter", sep = ""))
      shinyjs::hide(paste(at[n], "2", sep = ""))}
    shinyjs::hide("getCorrelationMatrix")
    shinyjs::hide("helpCorrelationMatrix")
    shinyjs::hide("getCorrelationScatter")
    shinyjs::hide("helpCorrelationScatter")
    shinyjs::hide("copyAAClipboard")
    shinyjs::hide("showLiterature1")
    shinyjs::hide("phylogy")
    shinyjs::hide("eps")
    shinyjs::hide("minPts")
    shinyjs::hide("goDbscan")
  }
  
  ## Generate Protein Table
  generateProteinTable <- function(){
    updateTabItems(session, "inTabset", "proteinTable")
    withProgress(message = "Searching for enzymes", value = 0, {
      assign_reactive_val(flags, "proteinSearch", TRUE)
      assign_reactive_val(flags, "fastaSearch", FALSE)
      assign_reactive_val(flags, "pdbSearch", FALSE)
      assign_reactive_val(flags, "paramSearch", FALSE)
      assign_reactive_val(flags, "attributeFound", nullFlags$attributeFound)
      incProgress(0, detail = "Entering data")
      error <- 0
      if(!saved()$proteinSaved){
        ecno <- as.character(ecNumbers())
        main <- .jnew("main.BrendaSOAP", ecno[1],
                      user(), as.integer(0), as.integer(0))
        main$erasePreviousOne(
          !saved()$fastaSaved, !saved()$pdbSaved, !saved()$paramSaved)
        n <- length(ecno)
        if(n > 1) lapply(ecno[2:n], function(e){
          main$addEnzyme(e)})
        incProgress(0.2, detail = showTime(timeProtein(2000)*n))
        error <- main$getProtein(FALSE)
        if(error == -1){javaError("Protein", session)}
      }
      incProgress(0.7, detail = "Proteins found, showing...")
      table <- new_table(paste(folder(), "table.txt", sep = ""))
      incProgress(0.1, detail = "Ready!")
    })
    assign_reactive_val(saved, "proteinSaved", FALSE)
    assign_reactive_val(savedTables, "proteinTable", table)
    ecNumbers(unique(table$EC_Number))
    assign_reactive_val(flags, "merged", FALSE)
    hideStuffs()
    shinyjs::enable("phylogeny")
  }
  
  ## Create parameters table
  importParamTable <- function(n,  ...){
    af <- flags()$attributeFound
    taf <- savedTables()$attrTable
    ftaf <- savedTables()$fattrTable
    attr_folder <- paste(folder(), "attributes/", sep = "")
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
    assign_reactive_val(savedTables, "attrTable", taf)
    assign_reactive_val(savedTables, "fattrTable", ftaf)
    assign_reactive_val(flags, "attributeFound", af)
  }
  
  ## Erase parameter
  eraseParam <- function(n, ...){
    af <- flags()$attributeFound
    taf <- savedTables()$attrTable
    ftaf <- savedTables()$fattrTable
    af[n] <- FALSE
    taf[[n]] <- NULL
    ftaf[[n]] <- NULL
    assign_reactive_val(flags, "attributeFound", af)
    assign_reactive_val(savedTables, "attrTable", taf)
    assign_reactive_val(savedTables, "fattrTable", ftaf)
  }
  
  ## Add to output Table
  addTable <- function(n, ...){
    op <- list(...)
    table <- op$table
    with_mol <- op$with_mol
    out <- attr_collapse(n, savedTables()$attrTable[[n]], with_mol)
    table <- merge(table, out, by="Ref", all = TRUE)
  }
  
  ## End the generation of parameter table
  endGeneration <- function(){
    attr_folder <- paste(folder(), "attributes/", sep = "")
    table <- new_table(paste(folder(), "table.txt", sep = ""))
    table$link <- clickable(table$Literature.PubmedID.)
    assign_reactive_val(savedTables, "proteinTable", table)
    table <- table[,c("EC_Number", "Organism", "Systematic_name",
                      "Recommended_name", "UniProt", "Commentary",
                      "Literature.PubmedID.", "link", "Ref")]
    incProgress(0.5)
    listA <- list()
    for(n in 1:12){
      incProgress(0.1/12, detail = "Showing results...")
      do_function(
        n, eraseParam, importParamTable, eraseParam,
        input$attributes, flags()$attributeFound, mol = molList[n]
      )
      table <- do_function(n, addTable, addNoTable, addNoTable,
                           input$attributes, flags()$attributeFound,
                           table = table, with_mol = bool_mol[n])
      listA <- do_function(n, updateKmeans, addNoList, addNoList,
                           input$attributes, flags()$attributeFound,
                           listA = listA)
    }
    updateCheckboxGroupInput(session, "kmeans", choices = listA)
    updateCheckboxGroupInput(session, "dbscanSelector", choices = listA)
    updateCheckboxGroupInput(session, "corScat", choices = listA)
    incProgress(0.05, detail = "Working on data")
    table <- as.data.frame(
      sapply(table,gsub,pattern="-999.0",replacement="Additional Information")
    )
    table <- as.data.frame(
      sapply(table,gsub,pattern="-999",replacement="Additional Information")
    )
    table <- table[sort(table$Ref, decreasing = FALSE),]
    table$Ref <- NULL
    shinyjs::show("showLiterature1")
    incProgress(0.05, detail = "Ready")
    shinyjs::enable("parameters")
    assign_reactive_val(savedTables, "parameterTable", table)
    assign_reactive_val(savedTables, "fparameterTable", table)
    shinyjs::hide("phylogy")
    assign_reactive_val(flags, "phylogenySearch", FALSE)
    assign_reactive_val(flags, "paramSaved", FALSE)
    assign_reactive_val(flags, "merged", FALSE)
  }
  
  ## Extract amount information
  extract <- function(n, ...){
    op <- list(...)
    table <- op$table
    out <- savedTables()$fattrTable[[n]]
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
  
  ## Filtering the data
  filtering <- function(n,...){
    table2 <- savedTables()$attrTable[[n]]
    op <- list(...)
    table <- op$table
    if(!is.null(op$mol) & op$mol != "null"){
      table2 <- table2[,c("Ref", "value", op$mol,
                          "Commentary", "Literature.PubmedID.")]
    } else{table2 <- table2[,c("Ref", "value",
                               "Commentary", "Literature.PubmedID.")]}
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
  
  ## Save the filtered Table
  saveFiltered <- function(n, ...){
    op <- list(...)
    table <- op$table
    NewTables <- savedTables()$fattrTable
    table2 <- savedTables()$attrTable[[n]]
    col <- paste(nat[n], "value", sep = "_")
    v <- table[,col]
    v <- unlist(strsplit(v, ";"), recursive = TRUE)
    NewTables[[n]] <- with(table2, table2[Ref %in% table$Ref & value %in% v,])
    assign_reactive_val(savedTables, "fattrTable", NewTables)
  }
  
  ## Generate Summary Table
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
  
  ## Count Attributes
  countAttr <- function(n, ...){
    op <- list(...)
    vector <- op$vector
    table <- savedTables()$fattrTable[[n]]
    u <- table$value
    u <- gsub("-999.0", NA, u)
    u <- gsub("-999", NA, u)
    u <- rep(nat[n], sum(
      as.numeric( !is.na(u) )
    ))
    v <- c(vector, u)
  }
  
  # Function call on start!
  hideStuffs()

  # Reactive Values  

  ## Flags
  flags <- reactiveVal(nullFlags)
  
  ## Saved
  saved <- reactiveVal(nullSaved)
  
  ## Tables
  savedTables <- reactiveVal(nullTables)
  
  ## Plots
  savedPlots <- reactiveVal(nullSavedPlots)
  
  ## Create user
  user <- eventReactive(input$logIn, {
    .jnew("client.User", input$mail, input$pass)
  })
  
  ## Define folder
  folder <- reactiveVal("results/")
  
  ## Ec Numbers
  ecNumbers <- reactiveVal(NULL)
  
  # Shortcuts
  
  ## To tutorial section on principal sidebar
  observeEvent(input$help, {
    updateTabItems(session, "inTabset", "help")
  })
  
  ## Principal buttons
  observeEvent(input$goHelp, {
    updateTabItems(session, "inTabset", "help")
  })
  observeEvent(input$goProtein, {
    updateTabItems(session, "inTabset", "proteinTable")
  })
  observeEvent(input$goExternal, {
    updateTabItems(session, "inTabset", "external")
  })
  observeEvent(input$goSummary, {
    updateTabItems(session, "inTabset", "info")
  })
  observeEvent(input$goAccount, {
    updateTabItems(session, "inTabset", "account")
  })
  
  # Home
  
  ## Account
  
  ### Log In
  observeEvent(input$logIn, {
    shinyjs::disable("logIn")
    shinyjs::disable("mail")
    shinyjs::disable("pass")
    withProgress(value = 0, message = "Verify account with Brenda", {
      keep <- user()$verifyAccount()
      incProgress(1)
    })
    if(keep == "You can search your enzyme"){
      assign_reactive_val(flags, "isThereUser", TRUE)
      shinyjs::show("logOut")
      shinyjs::hide("pass")
      shinyjs::hide("logIn")
      new_folder <- paste("results/", input$mail, "/", sep = "")
      folder(new_folder)
      updateTabItems(session, "inTabset", "enzyme")
    } else if(keep == "Incorrect password"){
      wrongPassword()
      shinyjs::click("logOut")
    } else if(keep == "Not Activated Account"){
      noActivated()
      shinyjs::click("logOut")
    } else if (keep == "Not a Brenda User"){
      noAccount()
      shinyjs::click("logOut")
    }
    shinyjs::enable("logIn")
  })
  
  ### Log Out
  observeEvent(input$logOut, {
    hideStuffs()
    shinyjs::show("logIn")
    shinyjs::enable("mail")
    shinyjs::enable("pass")
    shinyjs::show("pass")
    shinyjs::hide("logOut")
    shinyjs::hide("pick")
    shinyjs::hide("enzymeNameFinal")
    if(flags()$isThereUser){
      updateTextInput(session, "mail", value = "")
      assign_reactive_val(flags, "isThereUser", FALSE)}
    updateTextInput(session, "pass", value = "")
    updateActionButton(session, "generateSummary", label = "Generate", icon = character(0))
    # Flags
    flags(nullFlags)
    # Saved
    saved(nullSaved)
    # Tables
    savedTables(nullTables)
    # Plots
    savedPlots(nullSavedPlots)
  })
  
  # Protein Search
  
  ## Select Enzyme
  ### Does she/he/they have a saved table
  output$savedTable <- renderUI({
    if(flags()$isThereUser & !flags()$proteinSearch
       & file.exists(paste(folder(), "table.txt", sep = ""))){
      out <- div(style = "background-color: #a1fb9e; font-weight: bold;",
                 h4(icon("exclamation-triangle"),
                    "You have a saved table ",
                    actionLink("savedTableLink","more info")))
    } else{out <- NULL}
    out
  })
  
  ### Link to saved table
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
    attribute_folder <- paste(folder(), "attributes/", sep = "")
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
    ndetail <- list()
    selected <- c()
    for(n in 1:12){
      file_name <- paste(attribute_folder, nat_file[n], ".txt", sep = "")
      if(file.exists(file_name)){
        ndetail <- c(ndetail, list(nat_to_show[n]))
        selected <- c(selected, at[n])
        isThere <- TRUE
      }
    }
    if(isThere){out[4, "Available"] <- TRUE
      out[4, "Specific_Details"] <- paste(detail, 
                                          paste(ndetail, collapse = ", "),
                                          sep = "")
      updateCheckboxGroupInput(session, "attributes", selected = selected)
    }else{out[4, "Available"] <- FALSE}
    out
    assign_reactive_val(saved, "summaryTable", out)
  })

  
  ### Enter EC Number functions
  ### Enter EC Number
  #### EC Number 2
  observeEvent(input$ec_number1, {
    choices <- read.table(paste("resources/ecNumber/", input$ec_number1, "/select.txt", sep = ""),
                         sep = "\t",
                         header = TRUE,
                         col.names = "dig2")$dig2
    choices <- choices[base::order(as.numeric(choices), decreasing = FALSE)]
    updateSelectizeInput(session, "ec_number2", choices = choices)
  })
  #### EC Number3
  observeEvent(input$ec_number2, {
    choices <- read.table(paste("resources/ecNumber/", input$ec_number1, "/", input$ec_number2, "/select.txt", sep = ""),
                          sep = "\t",
                          header = TRUE,
                          col.names = "dig3")$dig3
    choices <- choices[base::order(as.numeric(choices), decreasing = FALSE)]
    updateSelectizeInput(session, "ec_number3", choices = choices)
  })
  #### EC Number4
  observeEvent(input$ec_number3, {
    choices <- read.table(paste("resources/ecNumber/", input$ec_number1, "/", input$ec_number2, "/", input$ec_number3, "/select.txt", sep = ""),
                          sep = "\t",
                          header = TRUE,
                          col.names = "dig4")$dig4
    choices <- choices[base::order(as.numeric(choices), decreasing = FALSE)]
    updateSelectizeInput(session, "ec_number4", choices = choices)
  })
  #### Enter EC Number
  observeEvent(input$ecNumber, {
    if(!flags()$isThereUser){noSession()}
    else{
      shinyjs::disable("ecNumber")
      new_ec_number <- paste(input$ec_number1,
                             input$ec_number2,
                             input$ec_number3,
                             input$ec_number4,
                             sep = ".")
      if(flags()$addEcNumber){ecno <- as.character(ecNumbers())
      ecno <- c(ecno, new_ec_number)
      ecNumbers(ecno)}
      else{ecNumbers(new_ec_number)}
      assign_reactive_val(flags, "addEcNumber", FALSE)
      generateProteinTable()
      shinyjs::enable("ecNumber")
    }
  })
  
  ### Enzyme Name
  #### Subclass selector
  observeEvent(input$subclass, {
    file <- paste("resources/synonyms/synonyms", input$subclass, "/subclasses.txt.txt", sep = "")
    table <- read.table(file,
                        sep = "\t",
                        header = TRUE)
    subclass <- as.list(table$EC)
    subclass <- setNames(subclass, table$subclass)
    updateSelectInput(session, "subsubclass",
                      choices = subclass)
  })
  #### Subclass button
  observeEvent(input$enterSubclass, {
    shinyjs::disable("enterSubclass")
    shinyjs::hide("enterSubclass")
    shinyjs::show("subsubclass")
    shinyjs::show("enterSubsubclass")
    shinyjs::enable("enterSubclass")
  })
  #### Functional group selector
  observeEvent(input$subsubclass, {
    if(input$subsubclass != ""){
      file <- paste("resources/synonyms/synonyms", input$subclass, "/", input$subsubclass , ".txt", sep = "")
      table <- read.table(file, sep = "\t", header = TRUE, fill = TRUE)
      table$value <- paste(table$ec_number, table$recommended_name, sep = ": ")
      table <- aggregate(table[,c("value")], by=list(table$synonyms), paste, collapse="\t")
      choices <- as.list(table$x)
      choices <- setNames(choices, table$Group.1)
      updateSelectizeInput(session, "enzyme_name",
                           choices = choices)
    }
  })
  #### Functional group button
  observeEvent(input$enterSubsubclass, {
    shinyjs::disable("enterSubsubclass")
    shinyjs::hide("enterSubsubclass")
    shinyjs::show("enzyme_name")
    shinyjs::show("enzymeName")
    shinyjs::enable("enterSubsubclass")
  })
  #### Enzyme name selector
  observeEvent(input$enzymeName, {
    if(!flags()$isThereUser){noSession()}
    a <- str_split(input$enzyme_name, "\t")
    b <- unlist(a)
    c <- str_split_fixed(b, ": ", 2)
    choices <- as.list(c[,1])
    choices <- setNames(choices, b)
    updateRadioButtons(session, "pick", choices = choices)
    shinyjs::show("pick")
    shinyjs::show("enzymeNameFinal")
    shinyjs::hide("enzymeName")
  })
  #### Enter Enzyme Name
  observeEvent(input$enzymeNameFinal, {
    if(!flags()$isThereUser){noSession()}
    else{
      if(flags()$addEcNumber){ecno <- as.character(ecNumbers())
      ecno <- c(ecno, input$pick)
      ecNumbers(ecno)}
      else{ecNumbers(input$pick)}
      assign_reactive_val(flags, "addEcNumber", FALSE)
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
  
  ### Message to the enzyme searcher
  output$enzymeWillBe <- renderUI({
    if(flags()$addEcNumber){
      h4(icon("exclamation-triangle"), "The enzyme your enter will be added to the search")
    } else if(flags()$proteinSearch){
      h4(icon("exclamation-triangle"), "The enzyme your enter will erase the previous ones")
    } else{
      div(style="text-align:center",
          h3("Welcome!!"),
          h4("Enter the type of enzyme(s) you want to work with"),
          p(icon("hand-point-left"), "From now on, we recommend hiding the dashboard menu to have a better visualization"))
    }
  })
  
  ### WordCloud on enzyme tab
  #### First digit EC Number
  observeEvent(input$ec_number1, {
    assign_reactive_val(savedPlots, "imageWordCloud",
                        imageEnzymeTab(input$ec_number1))
  })
  #### Subclass enzyme
  observeEvent(input$subclass, {
    assign_reactive_val(savedPlots, "imageWordCloud",
                        imageEnzymeTab(input$subclass))
  })
  #### Show WordCloud Image
  output$subclass <- renderImage({
    savedPlots()$imageWordCloud
  }, deleteFile = FALSE)
  
  ## Protein Table
  ## Sidebar options
  ### Add Enzyme
  observeEvent(input$addEnzyme, {
    if(!flags()$proteinSearch){
      noProteins()
    } else{
      assign_reactive_val(flags, "addEcNumber", TRUE)
      updateTabItems(session, "inTabset", "enzyme")}
  })
  
  ### Phylogeny
  #### Generate phylogeny
  observeEvent(input$phylogeny, {
    if(!flags()$proteinSearch){noSearch("Phylogeny")}
    else{
      table <- savedTables()$proteinTable
      shinyjs::disable("phylogeny")
      withProgress(message = "Generating tree", value = 0, {
        species <- table$Organism
        uids <- getID(species)
        taxa <- getTaxa(uids)
        phytable <- phyTable(taxa)
        assign_reactive_val(savedTables, "phylogenyTable", phytable)
        incProgress(0.1, detail = "Ready")
      })
      shinyjs::show("phylogy")
      assign_reactive_val(flags, "phylogenySearch", TRUE)
    }
  })
  
  ### Select all
  observeEvent(input$allParameters, {
    if(input$allParameters){
      updateCheckboxGroupInput(session, "attributes", selected = at)
    }
    else{
      updateCheckboxGroupInput(session, "attributes", selected = c(NULL))
    }
  })
  
  ### Generate Parameter Table
  observeEvent(input$parameters, {
    assign_reactive_val(flags, "paramSearch", TRUE)
    if(flags()$proteinSearch){
      shinyjs::disable("parameters")
      hideStuffs()
      updateTabItems(session, "inTabset", "parameterTable")
      withProgress(message = "Searching numerical Parameters", value = 0, {
        incProgress(0, detail = "Entering parameters...")
        error <- 0
        if(!saved()$paramSaved){
          ecno <- as.character(ecNumbers())
          assign_reactive_val(flags, "attributeFound", nullFlags$attributeFound)
          main <- .jnew("main.BrendaSOAP", ecno[1], user(),
                        as.integer(encoder_param(input$attributes)),
                        as.integer(encoder_filter(input$up)))
          main$erasePreviousOne(FALSE, FALSE, TRUE)
          n <- length(ecno)
          if(n > 1) lapply(ecno[2:n], function(e){
            main$addEnzyme(e)
          })
          main$getProtein(FALSE)
          incProgress(0.1, detail = "Selecting proteins for search")
          if(flags()$phylogenySearch){
            s <- input$distProteinTable_rows_all
            cond <- FALSE
          } else{
            s <- input$distProteinTable_rows_selected
            cond <- input$allProteins
          }
          int <- as.integer(c())
          if(length(s)){int <- as.integer(c(s - 1, int))}
          int <- .jarray(int)
          proteinTable <- savedTables()$proteinTable
          incProgress(0.2, detail = showTime(
            timeParameters(nrow(proteinTable))*length(ecNumbers())
          ))
          error <- main$getParameters(int, cond)
        }
        if(error == -1){
          handledJavaError(session, "Param")
          shinyjs::enable("parameters")
        } else{
          endGeneration()
        }
      })
    } else{
      noSearch("Parameters")
    }
  })
  
  ### Exception handler
  observeEvent(input$handlerJavaEParam, {
    if(input$handlerJavaEParam){
      shinyjs::disable("parameters")
      withProgress(message = "Searching numerical Parameters", value = 0, {
        incProgress(0, detail = "Entering parameters...")
        ecno <- as.character(ecNumbers())
        main <- .jnew("main.BrendaSOAP", ecno[1], user(), as.integer(encoder_param(input$attributes)), as.integer(encoder_filter(input$up)))
        main$erasePreviousOne(FALSE, FALSE, TRUE)
        n <- length(ecno)
        if(n > 1) lapply(ecno[2:n], function(e){
          main$addEnzyme(e)
        })
        main$getProtein(FALSE)
        incProgress(0.1, detail = "Selecting proteins for search")
        if(flags()$phylogenySearch){
          s <- input$distProteinTable_rows_all
          cond <- FALSE
        } else{
          s <- input$distProteinTable_rows_selected
          cond <- input$allProteins
        }
        int <- as.integer(c())
        if(length(s)){int <- as.integer(c(s - 1, int))}
        int <- .jarray(int)
        proteinTable <- savedTables()$proteinTables
        if(input$allProteins){n_pro <- nrow(proteinTable)}
        else{n_pro <- s}
        incProgress(0.2, detail = paste("This might take ",
                                        "60 minutes",
                                        sep = ""))
        error <- main$getParameters2(int, cond)
        if(error == -1){
          javaError("Parameters", session)
        } else{
          endGeneration()
        }
        assign_reactive_val(flags, "paramSearch", TRUE)
      })}
  })
  
  
  ## Main panel 
  ### Protein Table Title
  output$proteinTableTitle <- renderUI({
    if(is.null(savedTables()$proteinTable)){
      out <- div(style = "font-weight: bold;", "Proteins")
    } else {
      available_ec <- paste(
        unique(savedTables()$proteinTable$EC_Number),
        collapse = "; "
      )
      out <- div(div(style = "display: inline-block; font-weight: bold;", "Working Proteins: "),
                 div(style = "display: inline-block;", available_ec))
    }
  })
  
  ### Add phylogeny to table
  observeEvent(input$phylogy, {
    if(flags()$phylogenySearch & !flags()$paramSearch){
      shinyjs::disable("phylogy")
      shinyjs::disable("showComments1")
      shinyjs::disable("showLiterature1")
      table <- savedTables()$proteinTable
      table <- table[,c("EC_Number", "Systematic_name",
                        "Recommended_name", "UniProt",
                        "Organism", "Commentary", "Ref")]
      table <- phySelect(table, savedTables()$phylogenyTable, input$phylogy)
      assign_reactive_val(savedTables, "proteinTable", table)
      shinyjs::enable("phylogy")
      shinyjs::enable("showComments1")
      shinyjs::enable("showLiterature1")
    }
  })
  
  ### Protein Table
  output$distProteinTable <- DT::renderDT({
    table <- savedTables()$proteinTable
    table$Ref <- NULL
    if(flags()$paramSearch){
      table$Literature.PubmedID. <- NULL
      attributes(table)$names <- lapply(attributes(table)$names, gsub, pattern ="link", replacement = "Literature.PubmedID.")
    }
    if(!input$showComments1){table$Commentary <- NULL}
    if(!input$showLiterature1){table$Literature.PubmedID. <- NULL}
    if(flags()$phylogenySearch)
      selection <- list(target = "none")
    else
      selection <- list(target = "row")
    DT::datatable(table, escape = FALSE, filter = list(position = 'top', plain = TRUE),
                  selection = selection,
                  options = list(scrollX = TRUE,
                                 scrollY = 250,
                                 dom = "tip",
                                 pageLength = 10))
  })
  
  ### Download Protein Table
  output$downloadProtein <- downloadHandler(
    filename <- 'protein_table.txt',
    content <- function(name){
      table <- savedTables()$proteinTable
      table$Ref <- NULL
      table$link <- NULL
      write.table(table, name, quote = FALSE, row.names = FALSE, sep = "\t")
    }
  )
  
  ### Not implemented
  output$treeOut <- renderPlot({
    savedPlots()$treePlot
  })
  ### ***---***
  
  ### Search another enzyme
  observeEvent(input$toEnzyme, {
    updateTabItems(session, "inTabset", "enzyme")
    assign_reactive_val(flags, "addEcNumber", FALSE)
  })
  
  ### Generate Fasta Table
  observeEvent(input$toFasta, {
    if(!flags()$proteinSearch){noSearch("Sequence")}
    else{
      assign_reactive_val(flags, "fastaSearch", TRUE)
      withProgress(message = "Searching sequence", value = 0, {
        shinyjs::disable("toFasta")
        updateTabItems(session, "inTabset", "fasta")
        incProgress(0, detail = "Entering parameters")
        error <- 0
        if(!saved()$fastaSaved){
          ecno <- as.character(ecNumbers())
          main <- .jnew("main.BrendaSOAP", ecno[1], user(), as.integer(0), as.integer(0))
          n <- length(ecno)
          if(n > 1) lapply(ecno[2:n], function(e){
            main$addEnzyme(e)
            })
          main$getProtein(TRUE)
          incProgress(0.2, detail = showTime(timeFasta(length(ecNumbers()))))
          error <- main$getFastaSequence()
        }
        if(error == -1){handledJavaError(session, "Fasta")
          table <- NULL
        } else{
          incProgress(0.7, detail = "Showing")
          table <- new_table(paste(folder(),"report_fasta.txt", sep = ""))
          incProgress(0.1, detail = "Ready")
          shinyjs::enable("toFasta")}
      })
      assign_reactive_val(savedTables, "fastaTable", table)
      assign_reactive_val(saved, "fastaSaved", FALSE)
    }
  })
  
  ### Exception handler Fasta
  observeEvent(input$handlerJavaEFasta, {
    if(input$handlerJavaEFasta){
    withProgress(message = "Searching sequence", value = 0, {
      shinyjs::disable("toFasta")
      incProgress(0, detail = "Entering parameters")
      ecno <- as.character(ecNumbers())
      main <- .jnew("main.BrendaSOAP", ecno[1], user(), as.integer(0), as.integer(0))
      n <- length(ecno)
      if(n > 1) lapply(ecno[2:n], function(e){
        main$addEnzyme(e)
      })
      main$getProtein(TRUE)
      incProgress(0.2, detail = "This might take 40 minutes")
      error <- main$getFastaSequence2()
      if(error == -1){javaError("FASTA", session)
        table <- NULL
      } else{
        incProgress(0.7, detail = "Showing")
        table <- new_table(paste(folder(),"report_fasta.txt", sep = ""))
        incProgress(0.1, detail = "Ready")
        shinyjs::enable("toFasta")}
    })
      assign_reactive_val(savedTables, "fastaTable", table)
      assign_reactive_val(saved, "fastaSaved", FALSE)
    }
  })
  
  ### Generate PDB Table
  observeEvent(input$toPDB, {
    if(!flags()$proteinSearch){noSearch("PDB")}
    else{
      assign_reactive_val(flags, "pdbSearch", TRUE)
      shinyjs::disable("toPDB")
      withProgress(message = "Searching PDB", value = 0, {
        updateTabItems(session, "inTabset", "pdb")
        incProgress(0, detail = "Enter parameter")
        error <- 0
        if(!saved()$pdbSaved){
          ecno <- as.character(ecNumbers())
          main <- .jnew("main.BrendaSOAP", ecno[1], user(), as.integer(0), as.integer(0))
          n <- length(ecno)
          if(n > 1) lapply(ecno[2:n], function(e){
            main$addEnzyme(e)
          })
          main$getProtein(TRUE)
          proteinTable <- savedTables()$proteinTable
          incProgress(0.2, detail = showTime(timePDB(nrow(proteinTable))*length(ecNumbers())))
          error <- main$getPDB()}
        if(error == 0){
          incProgress(0.7, detail = "Showing")
          table <- new_table(paste(folder(),"pdb_table.txt", sep = ""))
          table$link <- lapply(table$link, function(i){
            createLink(i, i)})
          incProgress(0.1, detail = "Ready")
        }
      })
      shinyjs::enable("toPDB")
      if(error == -1){
        javaError("PDB", session)
        table <- NULL
      }
      assign_reactive_val(savedTables, "pdbTable", table)
      assign_reactive_val(saved, "pdbSaved", FALSE)
    }
  })
  
  
  ## FASTA
  ### Fasta Table
  output$fastaTable <- DT::renderDT({
    DT::datatable(savedTables()$fastaTable,
                  options = list(scrollX = TRUE,
                                 scrollY = 375,
                                 lengthMenu = c(5, 10, 50, 100),
                                 pageLength = 10))
  })
  
  ### Download Fasta Sequence
  output$downloadFASTA <- downloadHandler(
    filename <- 'sequences.txt',
    content <- function(name){
      if(flags()$fastaSearch){
        table <- processFasta(folder(), input$no_filter, input$fastaTable_rows_selected)
      } else{
        table <- NULL
      }
      write.table(table, name, quote = FALSE, row.names = FALSE, col.names = FALSE, sep = "\t")
    }
  )
  
  ### Back to protein table
  observeEvent(input$toProtein1, {
    updateTabItems(session, "inTabset", "proteinTable")
  })
  
  ## PDB
  ### PDB Table
  output$pdbTable <- DT::renderDT({
    DT::datatable(savedTables()$pdbTable,
                  selection = list(target = 'none'),
                  options = list(scrollX = TRUE,
                                 scrollY = 375,
                                 lengthMenu = c(5, 10, 50, 100),
                                 pageLength = 10))
  }, escape = FALSE)
  
  ### Download PDB
  output$downloadPDB <- downloadHandler(
    filename <- 'pdb.txt',
    content <- function(name){
      file <- paste(folder(), "pdb_table.txt", sep = "")
      table <- read.table(file, header = TRUE, sep = "\t")
      write.table(table, name, quote = FALSE, row.names = FALSE, sep = "\t")
    }
  )
  
  ### Back to protein table
  observeEvent(input$toProtein2, {
    updateTabItems(session, "inTabset", "proteinTable")
  })
  
  
  ## Parameter Table
  ### Show Parameter Table
  output$distParameterTable <- DT::renderDT({
    table <- savedTables()$fparameterTable
    v <- grepl("Literature", attributes(table)$names)
    table[,attributes(table)$names[v]] <- NULL
    attributes(table)$names <- lapply(
      attributes(table)$names,
      gsub, pattern ="link", replacement = "Literature.PubmedID."
    )
    if(flags()$paramSearch){
      if(!input$showLiterature2){v <- grepl("Literature", attributes(table)$names)
      table[,attributes(table)$names[v]] <- NULL}
      if(!input$showComments2){v <- grepl("Commentary", attributes(table)$names)
      table[,attributes(table)$names[v]] <- NULL}
      DT::datatable(table, escape = FALSE,
                    extensions = 'FixedColumns',
                    selection = list(target = 'none'),
                    options = list(scrollX = TRUE,
                                   fixedColumns = list(leftColumns = 3),
                                   scrollY = 300,
                                   lengthMenu = c(2, 5, 10, 50, 100),
                                   pageLength = 5))
    } else{NULL}
  }, escape = FALSE)
  
  ### Download Parameter Table
  output$downloadTable <- downloadHandler(
    filename <- 'table.txt',
    content <- function(name){
      if(flags()$paramSearch){
        table <- savedTables()$fparameterTable
        v <- grepl("link", attributes(table)$names)
        table[,attributes(table)$names[v]] <- NULL
      }
      else{
        table <- NULL
      }
      write.table(table, name, quote = FALSE, row.names = FALSE, sep = "\t")
    }
  )
  
  ### Back to protein table
  observeEvent(input$toProtein3, {
    updateTabItems(session, "inTabset", "proteinTable")
  })
  
  ### Filter
  observeEvent(input$filter, {
    if(!flags()$paramSearch){
      shinyalert("Cannot Filter",
                 "You need to look for parameters to filter them",
                 type = "error")
    } else{
    shinyjs::disable("filter")
    withProgress(message = "Filtering", value = 0, {
      table <- savedTables()$proteinTable 
      table <- table[,c("EC_Number", "Organism", "Systematic_name",
                        "Recommended_name", "UniProt", "Commentary",
                        "Literature.PubmedID.", "link", "Ref")]
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
        if(flags()$attributeFound[[n]] & at[n] %in% input$attributes){
          fL <- c(fL, fLprev[i], fLprev[i+1])
          i <- i + 2}
        else{fL <- c(fL, NA, NA)}
        incProgress(0.25/12, detail = "Loading Parameters")
      }
      for(n in 1:12){
        table <- do_function(n, filtering, addNoTable, addNoTable,
                             input$attributes, flags()$attributeFound,
                             bf = bfList[n], f1 = fL[2*n-1], f2 = fL[2*n],
                             table = table, with_mol = bool_mol[n], mol = molList[n])
        incProgress(0.25/12, detail = "Filtering")
      }
      for(n in 1:12){
        do_function(
          n, saveFiltered, do_nothing, do_nothing,
          input$attributes, flags()$attributeFound, table = table
        )
        incProgress(0.25/12, detail = "Saving changes")
      }
      table <- as.data.frame(sapply(
        table, gsub, pattern="-999.0", replacement="Additional Information")
      )
      table <- as.data.frame(sapply(
        table, gsub, pattern="-999", replacement="Additional Information")
      )
      table <- table[sort(table$Ref, decreasing = FALSE),]
      incProgress(0.25, detail = "Ready")
      table$Ref <- NULL
      shinyjs::enable("filter")
      assign_reactive_val(flags, "merged", FALSE)
    })
    assign_reactive_val(savedTables, "fparameterTable", table)}
  })
  
  ## Available Information
  ### To Enzyme
  observeEvent(input$enzymeInput, {
    updateTabItems(session, "inTabset", "enzyme")
  })
  
  ### To protein
  observeEvent(input$paramInput, {
    updateTabItems(session, "inTabset", "proteinTable")
  })
  
  ### To protein
  observeEvent(input$fastaInput, {
    updateTabItems(session, "inTabset", "proteinTable")
  })
  
  ### To protein
  observeEvent(input$pdbInput, {
    updateTabItems(session, "inTabset", "proteinTable")
  })
  
  ### To parameter Table
  observeEvent(input$filterInput, {
    updateTabItems(session, "inTabset", "parameterTable")
  })
  
  ### Generate Summary table
  observeEvent(input$generateSummary, {
    if(!flags()$proteinSearch){
      noSearch("Information")
    }
    else{
      table <- savedTables()$proteinTable 
      table <- table[,c("Ref", "Recommended_name", "Organism", "UniProt", "Commentary", "EC_Number")]
      table$Parameters <- 0
      for(n in 1:12){
        table <- do_function(n, extract, extractNotFound, addNoTable, input$attributes, flags()$attributeFound, table = table)
      }
      table$Found_info <- table$Parameters
      if(flags()$fastaSearch){
        ft <- savedTables()$fastaTable
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
      if(flags()$pdbSearch){
        pdb <- savedTables()$pdbTable
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
        infotable[n + 3, "n"] <- do_function(n, infoSummary, notfoundInfoSummary, notSelectInfoSummary, input$attributes, flags()$attributeFound, table = table)
        total <- c(total, as.double(infotable[n + 3, "n"]))
      }
      infotable[3, "n"] <- sum(na.omit(total))
      infotable[16, "Information"] <- "FASTA"
      if(flags()$fastaSearch){infotable[16, "n"] <- sum(table$Sequence_FASTA)}
      else{infotable[16, "n"] <- "No searched"}
      infotable[17, "Information"] <- "PDB"
      if(flags()$pdbSearch){infotable[17, "n"] <- sum(table$PDB)}
      else{infotable[17, "n"] <- "No searched"}
      table$EC_Number <- NULL
      table$Ref <- NULL
      assign_reactive_val(savedTables, "summaryTable", table)
      assign_reactive_val(savedTables, "infoTable", infotable)
      updateActionButton(session, "generateSummary", label = "Refresh", icon = icon("refresh"))
    }
  })
  
  ### Show Summary Table
  output$distSummaryTable <- DT::renderDT({
    DT::datatable(saved()$summaryTable,
                  options = list(scrollX = TRUE,
                                 scrollY = 375,
                                 lengthMenu = c(5, 10, 50, 100),
                                 pageLength = 10))
  })
  
  output$informationTable <- renderTable({
    savedTables()$infoTable
  },rownames = FALSE, colnames = FALSE)
  
  # Visualization
  
  ## Histogram
  ### Generate Count plot
  observeEvent(input$visualize, {
    if(flags()$paramSearch){
      updateTabItems(session, "inTabset", "histogram")
      withProgress(message = "Generate visualization", value = 0, {
        x <- c()
        incProgress(0, detail = "Counting found data")
        for(i in 1:12){
          x <- do_function(i, countAttr, addNoVector, addNoVector, input$attributes, flags()$attributeFound, vector = x)
          incProgress(0.07, detail = "Counting found data")
        }
        data <- data.frame(Parameters = x)
        incProgress(0, detail = "Plotting")
        p <- plot_ly(data, x = ~Parameters, color = ~Parameters, type = 'histogram', colors = seba_palette)
        assign_reactive_val(savedPlots, "histPlot", p)
        incProgress(0.16, detail = "Ready")
      })
    } else{noParameters("a", "Visualization")}
  })
  
  ### Count Plot
  output$histogram <- renderPlotly({
    savedPlots()$histPlot
  })
  
  ### Back to parameter table
  observeEvent(input$backParameter1, {
    updateTabItems(session, "inTabset", "parameterTable")
  })
  
  ### Generate Distribution
  observeEvent(input$getDistribution, {
    if(flags()$paramSearch){
      shinyjs::disable("getDistribution")
      updateTabItems(session, "inTabset", "distribution")
      withProgress(message = "Calculating distribution", value = 0, {
        incProgress(0, detail = "Loading tables")
        table <- savedTables()$proteinTable 
        table <- table[,c("Recommended_name", "Organism", "Ref")]
        dataList <- generateDistribution(input$attributes, flags()$attributeFound, table, savedTables()$fattrTable)
        p <- generatePlots(dataList)
        incProgress(0.1, detail = "Ready")
        assign_reactive_val(savedPlots, "distributionPlot", p)
      })
      shinyjs::enable("getDistribution")
    } else{noParameters("a", "Distribution")}
  })
  
  ## Distribution
  ### Show Distribution
  output$distributionOut <- renderPlotly({
    savedPlots()$distributionPlot
  })
  
  ### Back to visualization
  observeEvent(input$backVisualization1, {
    updateTabItems(session, "inTabset", "histogram")
  })
  
  ### Correlation
  correlationTable <- function(n, ...){
    tb <- simplify(savedTables()$fattrTable[[n]], nat[n])
  }
  
  ## Generate Correlation
  observeEvent(input$getCorrelation, {
    if(flags()$paramSearch){
      shinyjs::toggle("getCorrelationMatrix")
      shinyjs::toggle("helpCorrelationMatrix")
      shinyjs::toggle("getCorrelationScatter")
      shinyjs::toggle("helpCorrelationScatter")
    } else{noParameters("a", "Correlation")}
  })
  
  commonCorrelation <- function(){
    table <- savedTables()$proteinTable [,c("Ref", "Recommended_name", "Organism")]
    tableList <- list()
    for(n in 1:12){
      tableList[[n]] <- do_function(n, correlationTable, noTable, noTable, input$attributes, flags()$attributeFound)
    }
    mergedTable <- mergeTable(tableList, table)
    mergedTable
  }
  
  # As a heatmap matrix
  observeEvent(input$getCorrelationMatrix, {
    if(flags()$paramSearch){
      shinyjs::disable("getCorrelationMatrix")
      updateTabItems(session, "inTabset", "correlation")
      updateTabItems(session, "correlationPlot", "matrix")
      withProgress(message = "Calculating correlation...", value = 0, {
        if(!flags()$merged){
          assign_reactive_val(savedTables, "groupMerging", commonCorrelation())
        }
        incProgress(0, detail = "Grouping tables")
        tableMerged <- groupTables(savedTables()$groupMerging)
        incProgress(0.25, detail = "Merging tables")
        tableMerged <- doMerge(tableMerged)
        tableMerged <- deleteMutantColumn(tableMerged)
        incProgress(0.25, detail = "Doing the correlation")
        m <- correlation(tableMerged, "pearson")
        incProgress(0.125, detail = "Binding matrices")
        m <- bindMatrix(m)
        m <- m[unlist(flags()$attributeFound), unlist(flags()$attributeFound)]
        incProgress(0.125, detail = "Ploting")
        incProgress(0, detail = "Ready")
      })
      shinyjs::enable("getCorrelationMatrix")
      assign_reactive_val(flags, "merged", TRUE)
      assign_reactive_val(savedPlots, "correlationPlot", m)
    } else{(noParameters("a", "Correlation"))}
  })
  
  # As a scatterplot
  observeEvent(input$getCorrelationScatter, {
    if(flags()$paramSearch){
      updateTabItems(session, "inTabset", "correlation")
      updateTabItems(session, "correlationPlot", "scatter")
    } else{noParameters("a", "Correlation")}
  })
  
  # As a scatterplot
  observeEvent(input$getCorrelationScatter2, {
    s <- length(input$corScat)
    if(s <= 1 | s > 4){
      corScatterError(s)
    } else if(flags()$paramSearch){
      shinyjs::disable("getCorrelationScatter2")
      withProgress(message = "Ploting", value = 0, {
        if(!flags()$merged){
          assign_reactive_val(savedTables, "groupMerging", commonCorrelation())
        }
        incProgress(0, detail = "Entering selection")
        numbers <- unlist(as.integer(input$corScat))
        listTables <- savedTables()$groupMerging[numbers]
        listTables <- lapply(listTables, function(tb) configureMutant(tb) )
        incProgress(0.1, detail = "Merging")
        p <- mergingScatter(listTables, numbers, session)
        assign_reactive_val(savedPlots, "correlationPlotScatter", p)
        incProgress(0.1, detail = "Ready")
        shinyjs::enable("getCorrelationScatter2")
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
    m <- savedPlots()$correlationPlot
    if(input$correlationColor == "Default"){
      p <- plot_ly(x = colnames(m), y = rownames(m), z = m, colors = seba_palette2, type = 'heatmap')
    } else{
      p <- plot_ly(x = colnames(m), y = rownames(m), z = m, colors = input$correlationColor, type = 'heatmap')
    }
    p <- colorbar(p, limits = c(-1,1))
  })
  
  # Distribution
  output$correlationOut2 <- renderPlotly({
    savedPlots()$correlationPlotScatter
  })
  
  # Analysis
  observeEvent(input$analyze, {
    if(flags()$paramSearch){
      updateTabItems(session, "inTabset", "cluster")
      updateTabItems(session, "cluster", "home")
    } else{noParameters("an", "Analysis")}
  })
  
  # Back to Parameter Table
  observeEvent(input$backParameter2, {
    updateTabItems(session, "inTabset", "parameterTable")
  })
  
  # Clustering
  observeEvent(input$toKMeans, {
    if(flags()$paramSearch){updateTabItems(session, "clusterPlot", "k-means")}
    else{noParameters("a", "Clusterization")}
  })
  
  #K-means
  observeEvent(input$goKmeans, {
    s <- length(input$kmeans)
    if(!flags()$paramSearch){noParameters("a", "Clusterization")}
    else if(s==2 | s==3){
      shinyjs::disable("goKmeans")
      shinyjs::disable("kmeans")
      withProgress(value = 0, message = "Clustering...", {
        incProgress(0, detail = "Determinating dimensions")
        table <- savedTables()$proteinTable 
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
          assign_reactive_val(savedPlots, "elbowPlot", out$p)
          incProgress(0.2, detail = "Do clusterization")
          data <- clusteringKmeans(to_cluster, cluster, out$k)
          incProgress(0.1, detail = "Generating and saving clusterization")
          to_save <- completeData(data, savedTables()$proteinTable )
          assign_reactive_val(savedTables, "kmeansTable", to_save)
          incProgress(0.2, detail = "Ploting")
          datag <- data
          datag <- reduceData(2000, datag, "Your clusterized data has too many rows to plot, we are reducing it to be able to show it. The whole data is still available to download", session)
          if(s == 2){
            p <- plotingKmeans2d(datag, clu[1], clu[2])
          } else{
            p <- plotingKmeans(datag, clu[1], clu[2], clu[3])
          }
          assign_reactive_val(savedPlots, "kPlot", p)
          incProgress(0.1, detail = "Ready")
        }
      })
      shinyjs::enable("goKmeans")
      shinyjs::enable("kmeans")
    } else{kmeansError(s)
      assign_reactive_val(savedPlots, "elbowPlot", NULL)
      assign_reactive_val(savedPlots, "kPlot", NULL)}
  })
  
  output$elbowKMeans <- renderPlot({
    savedPlots()$elbowPlot
  })
  output$kmeansPlot <- renderPlotly({
    savedPlots()$kPlot
  })
  
  # Download Cluster Parameter Table
  output$downloadKmeans <- downloadHandler(
    filename <- 'clusters.txt',
    content <- function(name){
      if(flags()$paramSearch){
        table <- savedTables()$kmeansTable
      }
      else{
        table <- NULL
      }
      write.table(table, name, quote = FALSE, row.names = FALSE, sep = "\t")
    }
  )
  
  # DBSCAN
  
  # Clustering
  observeEvent(input$toDBSCAN, {
    if(flags()$paramSearch){updateTabItems(session, "clusterPlot", "dbscan")}
    else{noParameters("a", "Clusterization")}
  })
  
  # DBSCAN
  observeEvent(input$dbscanSelector, {
    shinyjs::disable("goDbscan")
  })
  
  observeEvent(input$dbscanMerge, {
    s <- length(input$dbscanSelector)
    if(!flags()$paramSearch){noParameters("a", "Clusterization")}
    else if(s==2 | s==3){
      shinyjs::disable("dbscanMerge")
      shinyjs::disable("dbscanSelector")
      withProgress(value = 0, message = "Merging...", {
        incProgress(0, detail = "Determinating dimensions")
        table <- savedTables()$proteinTable 
        table <- table[,c("Ref", "Recommended_name", "Organism")]
        attr <- list()
        clu <- as.integer(input$dbscanSelector)
        for(n in clu){
          tb <- correlationTable(n)
          tb <- unique(labeling(tb, table))
          attr <- list.append(attr, tb)
        }
        incProgress(0.7, detail = "Merging tables")
        cluster <- join_all(attr, type = "inner")
        cluster <- unique(cluster)
        if(nrow(cluster) <= 2){
          shinyalert("Not enough data", "We are sorry, but there are really very few common data beetween parameters")
          shinyjs::enable("dbscanSelector")
          shinyjs::hide("eps")
          shinyjs::hide("minPts")
          shinyjs::hide("goDbscan")
        } else{
          cluster <- reduceData(8000, cluster, "Data reduced to calculate distance and to clusterize, this data is not going to be used in clustering", session)
          assign_reactive_val(savedTables, "dbscanTable", cluster)
          to_cluster <- preKmeans(cluster)
          d <- dist(to_cluster, "euclidean")
          d <- as.matrix(d)
          j <- hist(d, plot = FALSE)
          assign_reactive_val(savedPlots, "distDBSCAN", d)
          h <- j$breaks
          min <- h[2]
          max <- h[length(h)]
          value <- h[3]
          step <- h[2] - h[1]
          updateSliderInput(session, "eps", value = value, min = min, max = max, step = step)
          updateSliderInput(session, "minPts", value = s + 2, min = s + 1, max = nrow(to_cluster), step = 1)
          shinyjs::show("eps")
          shinyjs::show("minPts")
          shinyjs::show("goDbscan")
          shinyjs::enable("goDbscan")
        }
        incProgress(0.3, detail = "Ready")
      })
      shinyjs::enable("dbscanMerge")
    } else{kmeansError(s)
    }
  })
  
  observeEvent(input$goDbscan, {
    shinyjs::disable("goDbscan")
    shinyjs::disable("dbscanSelector")
    withProgress(value = 0, message = "Clustering...", {
      incProgress(0, detail = "Loading clustering")
      s <- length(input$dbscanSelector)
      clu <- as.integer(input$dbscanSelector)
      cluster <- savedTables()$dbscanTable
      to_cluster <- preKmeans(cluster)
      incProgress(0.3, detail = "Do clusterization")
      data <- clusteringDBSCAN(to_cluster, cluster, input$eps, input$minPts)
      incProgress(0.3, detail = "Generating and saving clusterization")
      to_save <- completeData(data, savedTables()$proteinTable )
      assign_reactive_val(savedTables, "dbscanSaveTable", to_save)
      incProgress(0.2, detail = "Ploting")
      datag <- data
      datag <- reduceData(2000, datag, "Your clusterized data has too many rows to plot, we are reducing it to be able to show it. The whole data is still available to download", session)
      if(s == 2){
        p <- plotingKmeans2d(datag, clu[1], clu[2])
      } else{
        p <- plotingKmeans(datag, clu[1], clu[2], clu[3])
      }
      assign_reactive_val(savedPlots, "dbscanPlot", p)
      incProgress(0.2, detail = "Ready")
      })
      shinyjs::enable("goDbscan")
      shinyjs::enable("dbscanSelector")
  })
  
  output$histDBSCAN <- renderImage({
    outfile <- tempfile(fileext='.png')
    distance <- savedPlots()$distDBSCAN
    png(outfile, width=230, height=230)
    if (!is.null(distance)){
      hist(distance)
    }
    dev.off()
    list(src = outfile)
  }, deleteFile = TRUE)
  
  output$dbscanPlot <- renderPlotly({
    savedPlots()$dbscanPlot
  })
  
  # Download Cluster Parameter Table
  output$downloadDBSCAN <- downloadHandler(
    filename <- 'clusters.txt',
    content <- function(name){
      if(flags()$paramSearch){
        table <- savedTables()$dbscanSaveTable
      }
      else{
        table <- NULL
      }
      write.table(table, name, quote = FALSE, row.names = FALSE, sep = "\t")
    }
  )
  
  # External Link
  observeEvent(input$proteinPredictor, {
    updateActionButton(session, "proteinPredictor", icon = icon(""))
  })
  
  observeEvent(input$copySequence, {
    if(!flags()$fastaSearch){
      noSearch("Sequence")
    } else{
      updateTabItems(session, "inTabset", "fasta")
      shinyjs::show("copyAAClipboard")}
  })
  
  observeEvent(input$copyAAClipboard, {
    if(length(input$fastaTable_rows_selected) != 1){
      copyAAError()
    }
    else{
      updateTabItems(session, "inTabset", "external")
      text <- processFasta(folder(), FALSE, input$fastaTable_rows_selected)
      text <- str_split_fixed(text, "\n", 2)
      text <- text[[2]]
      clipr::write_clip(text, allow_non_interactive = TRUE)
      updateActionButton(session, "proteinPredictor", icon = icon("check"))
      shinyjs::hide("copyAAClipboard")
    }
  })
  
  observeEvent(input$toFasta2, {
    if(!flags()$fastaSearch){
      noSearch("Sequence")
    } else{
      updateTabItems(session, "inTabset", "fasta")
    }
  })
  
  # Suggestions
  observeEvent(input$suggestionType, {
    if(input$suggestionType == "others"){
      updateSelectInput(session, "suggestionSubtype", choices = NULL)
      shinyjs::hide("suggestionSubtype")
      shinyjs::show("newType")
    } else{
      choices <- generateChoices(input$suggestionType)
      shinyjs::show("suggestionSubtype")
      shinyjs::hide("newType")
      updateSelectInput(session, "suggestionSubtype", choices = choices)
    }
  })
  
  observeEvent(input$allowMail, {
    if(input$allowMail){
      updateCheckboxInput(session, "showMail", value = TRUE)
      shinyjs::disable("showMail")
    }else {
      updateCheckboxInput(session, "showMail", value = FALSE)
      shinyjs::enable("showMail")}
  })
  
  observeEvent(input$enterSuggestion, {
    if(input$showMail & !flags()$isThereUser){
      shinyalert("No mail", "We don't have your mail, enter Brenda user or unclick the mail option to submit", type = "error")
    } else if(input$suggestionText == ""){
      shinyalert("No suggestion", "There is no suggestion to submit", type = "error")
    } else{
      withProgress(message = "Submitting", value = 0, {
        id <- read.table("suggestion_box/SuggestionNames.txt", header = FALSE)
        line <- paste("ID:", id[1,1])
        inFile <- input$imageSuggestion
        if(!is.null(inFile)) file.copy( inFile$datapath, file.path("suggestion_box", paste(id[1,1], ".jpeg", sep = "")) )
        if(input$showMail){line <- paste(line,
                                         paste("Mail:", user()$getMail()),
                                         sep = "\n")}
        write.table(as.double(id[1,1])+1, "suggestion_box/SuggestionNames.txt", quote = FALSE, row.names = FALSE, col.names = FALSE)
        if(input$suggestionType == "others"){
          line <- paste(line, paste("Subject:", input$newType), sep="\n")
        } else{
            line <- paste(line, paste("Subject:", input$suggestionSubtype), sep="\n")}
        if(input$allowMail){line <- paste(line, "***** Want to receive an email *****", sep ="\n")}
        line <- paste(line, input$suggestionText, sep = "\n")
        line <- paste(line, "--------------------------------------", sep = "\n")
        write(line, paste("suggestion_box/",input$suggestionType,".txt",sep=""), append = TRUE)
        updateTextAreaInput(session, "suggestionText", value = "")
        incProgress(1, detail = "Ready!")
      })
      showNotification(h4(icon("smile-beam"), "Thank you!, your suggestion was submitted"),type = "message")
    }
  })
  
  # Links on the tutorial
  # Enzyme name section
  observeEvent(input$enzymeHelp, {
    updateTabItems(session, "inTabset", "enzyme")
  })
  observeEvent(input$enzymeHelp2, {
    updateTabItems(session, "inTabset", "enzyme")
  })
  # Protein table section
  observeEvent(input$proteinHelp, {
    updateTabItems(session, "inTabset", "proteinTable")
  })
  observeEvent(input$proteinHelp2, {
    updateTabItems(session, "inTabset", "proteinTable")
  })
  # Faq section
  observeEvent(input$faqhelp, {
    updateTabItems(session, "inTabset", "faq")
  })
  # Suggestions section
  reportBug <- function(){
    updateTabItems(session, "inTabset", "suggestions")
    updatePickerInput(session, "suggestionType", selected = "bug")
    updateSelectInput(session, "suggestionSubtype",
                      selected = "Application stoped (indicates the point and as many details as possible)")
  }
  observeEvent(input$sugghelp, {
    reportBug()
  })
  observeEvent(input$sugghelp2, {
    reportBug()
  })
  observeEvent(input$sugghelp3, {
    reportBug()
  })
  observeEvent(input$toReportBug, {
    if(input$toReportBug) reportBug()
  })
  
  # Saved Table
  
  ### Summary saved table
  output$summarySavedTable <- DT::renderDT({
    DT::datatable(saved()$summaryTable,
                  options = list(searching = FALSE))
  })
  
  ### Load tables
  observeEvent(input$loadTable, {
    assign_reactive_val(saved, "proteinSaved", TRUE)
    generateProteinTable()
    if(2 %in% input$summarySavedTable_rows_selected &
       saved()$summaryTable[2, "Available"]){
      assign_reactive_val(saved, "fastaSaved", TRUE)
    }
    if(3 %in% input$summarySavedTable_rows_selected &
       saved()$summaryTable[3, "Available"]){
      assign_reactive_val(saved, "pdbSaved", TRUE)
    }
    if(4 %in% input$summarySavedTable_rows_selected &
       saved()$summaryTable[4, "Available"]){
      assign_reactive_val(saved, "paramSaved", TRUE)
    }
    main <- .jnew("main.BrendaSOAP", "1.1.1.1", user(), as.integer(0), as.integer(0))
    main$erasePreviousOne(!saved()$fastaSaved, !saved()$pdbSaved, !saved()$paramSaved)
    if(saved()$fastaSaved){shinyjs::click("toFasta")}
    if(saved()$pdbSaved){shinyjs::click("toPDB")}
    if(saved()$paramSaved){shinyjs::click("parameters")}
  })
  
  ### Or don't
  observeEvent(input$toEnzyme2, {
    updateTabItems(session, "inTabset", "enzyme")
    assign_reactive_val(flags, "addEcNumber", FALSE)
  })
  
})