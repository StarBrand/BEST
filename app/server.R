## This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

source("methods/libs.R")
.jinit("resources/BrendaSOAP.jar")
source("methods/initials.R")
source("methods/correlation.R")
source("methods/utils.R")
source("methods/errors.R")
source("methods/clustering.R")
source("methods/distribution.R")
source("methods/database.R")
source("methods/login.R")
set_logging()

# Define server logic
shinyServer(function(input, output, session) {
  
  set_logging_session()
  
  # workingOnIt()
  
  hideStuffs <- function(){
    for(n in 1:12){shinyjs::hide(paste(at[n], "Filter", sep = ""))
      shinyjs::hide(paste(at[n], "2", sep = ""))}
    shinyjs::hide("getCorrelationMatrix")
    shinyjs::hide("helpCorrelationMatrix")
    shinyjs::hide("getCorrelationScatter")
    shinyjs::hide("helpCorrelationScatter")
    shinyjs::hide("copyAAClipboard")
    shinyjs::hide("eps")
    shinyjs::hide("minPts")
    shinyjs::hide("goDbscan")
  }
  
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
  
  ## Define folder
  folder <- reactiveVal("results/")
  
  ## Define user
  userRef <- reactiveVal(0L)
  
  ## Ec Numbers
  ecNumbers <- reactiveVal(NULL)
  
  ## Refs
  activeRef <- reactiveVal(NULL)
  
  # To tutorial section
  observeEvent(input$help, {
    updateTabItems(session, "inTabset", "help")
  })
  
  # Tutorial
  tutorial <- reactiveVal(includeHTML("www/tutorial_1.html"))
  activeTutorial <- reactiveVal(1)
  
  changeActiveTutorial <- function(active){
    tutorial(includeHTML( sprintf("www/tutorial_%d.html", active) ))
    prev <- activeTutorial()
    if (prev != active){
      shinyjs::hide( sprintf("tutorial%dlist", prev) )
      activeTutorial(active)
    }
    shinyjs::toggle( sprintf("tutorial%dlist", active) )
    if (active != 1) shinyjs::show("tutorialPrev")
    else shinyjs::hide("tutorialPrev")
    if (active != 9) shinyjs::show("tutorialNext")
    else shinyjs::hide("tutorialNext")
  }
  
  ## Page 1
  observeEvent(input$tutorial1, { changeActiveTutorial(1) })
  output$tutorial1list <- renderUI({ includeHTML("www/tutorial_sub_list_1.html") })
  
  ## Page 2
  observeEvent(input$tutorial2, { changeActiveTutorial(2) })
  output$tutorial2list <- renderUI({ includeHTML("www/tutorial_sub_list_2.html") })
  
  ## Page 3
  observeEvent(input$tutorial3, { changeActiveTutorial(3) })
  output$tutorial3list <- renderUI({ includeHTML("www/tutorial_sub_list_3.html") })
  
  ## Page 4
  observeEvent(input$tutorial4, { changeActiveTutorial(4) })
  output$tutorial4list <- renderUI({ includeHTML("www/tutorial_sub_list_4.html") })
  
  ## Page 5
  observeEvent(input$tutorial5, { changeActiveTutorial(5) })
  output$tutorial5list <- renderUI({ includeHTML("www/tutorial_sub_list_5.html") })
  
  ## Page 6
  observeEvent(input$tutorial6, { changeActiveTutorial(6) })
  output$tutorial6list <- renderUI({ includeHTML("www/tutorial_sub_list_6.html") })
  
  ## Page 7
  observeEvent(input$tutorial7, { changeActiveTutorial(7) })
  output$tutorial7list <- renderUI({ includeHTML("www/tutorial_sub_list_7.html") })
  
  ## Page 8
  observeEvent(input$tutorial8, { changeActiveTutorial(8) })
  output$tutorial8list <- renderUI({ includeHTML("www/tutorial_sub_list_8.html") })
  
  ## Page 9
  observeEvent(input$tutorial9, { changeActiveTutorial(9) })
  output$tutorial9list <- renderUI({ includeHTML("www/tutorial_sub_list_9.html") })
  
  ## Show active page
  output$tutorialMain <- renderUI({
    tutorial()
  })
  
  ## Previous section
  observeEvent(input$tutorialPrev, {
    shinyjs::click( sprintf("tutorial%d", activeTutorial() - 1) )
  })
  
  ## Next section
  observeEvent(input$tutorialNext, {
    shinyjs::click( sprintf("tutorial%d", activeTutorial() + 1) )
  })
  
  
  # Registration section
  ## To Registration section
  observeEvent(input$register, {
    updateTabItems(session, "inTabset", "registrationTab")
  })
  
  ## To Suggestion section
  observeEvent(input$suggestionsLink, {
    updateTabItems(session, "inTabset", "suggestions")
  })
  
  ## Registration
  observeEvent(input$suBack, {
    updateTabItems(session, "inTabset", "account")
  })
  
  observeEvent(input$suRegister, {
    log_event("Registration attempt")
    if (input$suMail == "") isRequired("Mail")
    else if (input$suFirst == "") isRequired("First name")
    else if (input$suLast == "") isRequired("Last name")
    else if (input$suPassword == "") isRequired("Password")
    else if(!checkMail(input$suMail, input$suMailConfirm)){
      log_warning("Mail doesn't match")
      mailDoesNotMatch()
    } else if (!checkPassword(input$suPassword, input$suPassword2)){
      log_warning("Password doesn't match")
      updateTextInput(session, "suPassword", value = "")
      updateTextInput(session, "suPassword2", value = "")
      passDoesNotMatch()
    } else {
      out <- save_user(input$suFirst, input$suLast, input$suMail, input$suPassword)
      if(out == 0L){
        updateTextInput(session, "suFirst", value = "")
        updateTextInput(session, "suLast", value = "")
        updateTextInput(session, "suMail", value = "")
        updateTextInput(session, "suMailConfirm", value = "")
        updateTextInput(session, "suPassword", value = "")
        updateTextInput(session, "suPassword2", value = "")
        updateTabItems(session, "inTabset", "account")
      } else log_warning("Unsuccessful registration")
    }
  })
  
  
  # Principal buttons
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
  
  # Disable user
  observeEvent(input$logIn, {
    shinyjs::disable("logIn")
    shinyjs::disable("mail")
    shinyjs::disable("pass")
    withProgress(value = 0, message = "Verify account", {
      ref <- check_password(input$mail, input$pass)
      incProgress(1)
    })
    if (ref == -1L) {
      wrongPassword()
      shinyjs::click("logOut")
    } else if (ref == -2L) {
      noAccount()
      shinyjs::click("logOut")
    } else {
      assign_reactive_val(flags, "isThereUser", TRUE)
      shinyjs::disable("anonymous")
      shinyjs::show("logOut")
      shinyjs::hide("pass")
      shinyjs::hide("logIn")
      shinyjs::hide("register")
      new_folder <- paste("results/", ref, "/", sep = "")
      folder(new_folder)
      userRef(ref)
      log_event("Folder for user created")
      log_value(folder())
      updateTabItems(session, "inTabset", "enzyme")
    }
    shinyjs::enable("logIn")
  })
  
  # Enable user
  observeEvent(input$logOut, {
    hideStuffs()
    shinyjs::show("logIn")
    shinyjs::show("register")
    shinyjs::enable("anonymous")
    shinyjs::enable("mail")
    shinyjs::enable("pass")
    shinyjs::show("pass")
    shinyjs::hide("logOut")
    shinyjs::hide("pick")
    shinyjs::hide("enzymeNameFinal")
    if(flags()$isThereUser){
      updateTextInput(session, "mail", value = "")
      assign_reactive_val(flags, "isThereUser", FALSE)
      userRef(0L)}
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
  
  # Anonymous user
  observeEvent(input$anonymous, {
    enterAnonymous()
  })
  observeEvent(input$anonymousBack, {
    enterAnonymous()
    updateTabItems(session, "inTabset", "account")
  })
  enterAnonymous <- function(){
    enterAnonymously(session)
  }
  observeEvent(input$enterAnonymous, {
    if(input$enterAnonymous){
      log_message("Licence accept in this session")
      shinyjs::disable("anonymous")
      shinyjs::disable("logIn")
      shinyjs::disable("mail")
      shinyjs::disable("pass")
      assign_reactive_val(flags, "isThereUser", TRUE)
      shinyjs::show("logOut")
      shinyjs::hide("pass")
      shinyjs::hide("logIn")
      shinyjs::hide("register")
      new_folder <- "results/anonymous/"
      folder(new_folder)
      userRef(0L)
      log_event("Folder for user created")
      log_value(folder())
      updateTabItems(session, "inTabset", "enzyme")
      shinyjs::enable("logIn")
      assign_reactive_val(flags, "anonymousUser", TRUE)
    }
  })
  
  # Does she/he/they have a saved table
  output$savedTable <- renderUI({
    if(!flags()$anonymousUser){
      cache <- checkCache(userRef())
      if(flags()$isThereUser & !flags()$proteinSearch
         & nrow(cache) == 1){
        log_message("User has saved data")
        ec_numbers <- c(cache$ec_numbers[1])
        ec_numbers <- gsub("\\}", "", gsub("\\{", "", ec_numbers))
        ec_numbers <- unlist(strsplit(ec_numbers, ",", fixed=T))
        log_value(ec_numbers)
        ecNumbers(ec_numbers)
        refs <- c(cache$refs[1])
        refs <- gsub("\\}", "", gsub("\\{", "", refs))
        refs <- as.numeric(unlist(strsplit(refs, ",", fixed=T)))
        log_value(refs)
        activeRef(refs)
        assign_reactive_val(saved, "proteinSaved", TRUE)
        assign_reactive_val(saved, "pdbSaved", cache$pdb[1])
        assign_reactive_val(saved, "fastaSaved", cache$fasta[1])
        fat <- flags()$attributeFound
        for(n in at){
          fat[n] <- cache[1, n]
        }
        assign_reactive_val(flags, "attributeFound", fat)
        log_value(saved())
        log_value(flags()$attributeFound)
        out <- div(style = "background-color: #a1fb9e; font-weight: bold;",
                   h4(icon("exclamation-triangle"),
                      "You have a saved table ",
                      actionLink("savedTableLink","more info")))
      } else{out <- NULL}
    } else{out <- NULL}
      out
  })
  
  # Protein Table Title
  output$proteinTableTitle <- renderUI({
    if(is.null(savedTables()$proteinTable)){
      out <- div(style = "font-weight: bold;", "Proteins")
    } else {
      available_ec <- paste(unique(savedTables()$proteinTable$EC_Number), collapse = "; ")
      out <- div(div(style = "display: inline-block; font-weight: bold;", "Working Proteins: "),
                 div(style = "display: inline-block;", available_ec))
    }
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
    fasta_file <- paste(folder(), "report_fasta.txt", sep = "")
    pdb_file <- paste(folder(), "pdb_table.txt", sep = "")
    out[1, "Available"] <- TRUE
    ecno <- ecNumbers()
    out[1, "Specific_Details"]  <- paste("EC Nuber:",
                                         paste(ecno, collapse = "; "),
                                         ". Total Organisms:",
                                         length(activeRef()),
                                         sep = " ")
    if(file.exists(fasta_file) & saved()$fastaSaved){
      out[2, "Available"] <- TRUE
      out[2, "Specific_Details"] <- paste("Sequence found: ",
                                          nrow(new_table(fasta_file)),
                                          sep = "")
    } else{out[2, "Available"] <- FALSE}
    if(file.exists(pdb_file) & saved()$pdbSaved){
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
      if(flags()$attributeFound[[at[n]]]){
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
  
  # Summary saved table
  output$summarySavedTable <- DT::renderDT({
    DT::datatable(saved()$summaryTable,
                  options = list(searching = FALSE))
  })
  
  # Load tables
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
    if(saved()$fastaSaved){shinyjs::click("toFasta")}
    if(saved()$pdbSaved){shinyjs::click("toPDB")}
    if(saved()$paramSaved){shinyjs::click("parameters")}
  })
  
  # Or don't
  observeEvent(input$toEnzyme2, {
    updateTabItems(session, "inTabset", "enzyme")
    assign_reactive_val(flags, "addEcNumber", FALSE)
  })
  
  
  # Enter EC Number functions
  
  
  # EC Number 2
  observeEvent(input$ec_number1, {
    shinyjs::disable("ecNumber")
    shinyjs::disable("ec_number2")
    shinyjs::disable("ec_number3")
    shinyjs::disable("ec_number4")
    choices <- updateEC2(input$ec_number1)
    updateSelectizeInput(session, "ec_number2", choices = choices)
    ec <- choices[1]
    choices <- updateEC3(input$ec_number1, choices[1])
    updateSelectizeInput(session, "ec_number3", choices = choices)
    choices <- updateEC4(input$ec_number1, ec, choices[1])
    updateSelectizeInput(session, "ec_number4", choices = choices)
    shinyjs::enable("ecNumber")
    shinyjs::enable("ec_number2")
    shinyjs::enable("ec_number3")
    shinyjs::enable("ec_number4")
  })
  # EC Number3
  observeEvent(input$ec_number2, {
    shinyjs::disable("ecNumber")
    shinyjs::disable("ec_number3")
    shinyjs::disable("ec_number4")
    choices <- updateEC3(input$ec_number1, input$ec_number2)
    updateSelectizeInput(session, "ec_number3", choices = choices)
    choices <- updateEC4(input$ec_number1, input$ec_number2, choices[1])
    updateSelectizeInput(session, "ec_number4", choices = choices)
    shinyjs::enable("ecNumber")
    shinyjs::enable("ec_number3")
    shinyjs::enable("ec_number4")
  })
  # EC Number4
  observeEvent(input$ec_number3, {
    shinyjs::disable("ecNumber")
    shinyjs::disable("ec_number4")
    choices <- updateEC4(input$ec_number1, input$ec_number2, input$ec_number3)
    updateSelectizeInput(session, "ec_number4", choices = choices)
    shinyjs::enable("ecNumber")
    shinyjs::enable("ec_number4")
  })
  
  # Update selector
  observeEvent(input$subclass, {
    subclass <- getReaction(input$subclass)
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
      table <- data.table(getSynonyms(input$subclass, input$subsubclass))
      table$value <- paste(table$ec_number, table$recommended_name, sep = ": ")
      if (nrow(table) != 0) {
        table <- aggregate(table[,c("value")], by=list(table$synonyms), paste, collapse="\t")
        choices <- as.list(table$value)
        choices <- setNames(choices, table$Group.1)
        updateSelectizeInput(session, "enzyme_name",
                             choices = choices)
      }
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
  
  # WordCloud on enzyme tab
  observeEvent(input$ec_number1, {
    assign_reactive_val(savedPlots, "imageWordCloud",
                        imageEnzymeTab(input$ec_number1))
  })
  
  observeEvent(input$subclass, {
    assign_reactive_val(savedPlots, "imageWordCloud",
                        imageEnzymeTab(input$subclass))
  })
  
  output$subclass <- renderImage({
    savedPlots()$imageWordCloud
  }, deleteFile = FALSE)
  
  # Generate Protein Table
  generateProteinTable <- function(){
    updateTabItems(session, "inTabset", "proteinTable")
    withProgress(message = "Searching for enzymes", value = 0, {
      if(!flags()$anonymousUser)
        eraseCache(userRef())
      assign_reactive_val(flags, "proteinSearch", TRUE)
      assign_reactive_val(flags, "fastaSearch", FALSE)
      assign_reactive_val(flags, "pdbSearch", FALSE)
      assign_reactive_val(flags, "paramSearch", FALSE)
      assign_reactive_val(flags, "attributeFound", nullFlags$attributeFound)
      incProgress(0, detail = "Entering data")
      error <- 0
      n <- 0
      ecno <- as.character(ecNumbers())
      n <- length(ecno)
      log_value(ecno)
      incProgress(0.2, detail = showTime(timeProtein(2000)*n))
      ecno <- paste("'", ecno, "'", sep = "")
      ecno <- paste(ecno, collapse = ", ")
      log_value(ecno)
      table <- getProteins(ecno)
      incProgress(0.7, detail = "Proteins found, showing...")
      log_value(nrow(table))
      table$link <- clickable(table$Literature.PubmedID.)
      log_event(sprintf("Proteins found: %d", nrow(table)))
      incProgress(0.1, detail = "Ready!")
    })
    assign_reactive_val(flags, "proteinSaved", FALSE)
    assign_reactive_val(savedTables, "proteinTable", table)
    ecNumbers(unique(table$EC_Number))
    log_event("EC Numbers")
    log_value(ecNumbers())
    activeRef(table$Ref)
    log_event("We have the following ref")
    log_value(activeRef())
    log_value(paste(activeRef(), collapse = ", "))
    if(!flags()$anonymousUser)
      saveCache(userRef(), paste(activeRef(), collapse = ", "), paste(ecNumbers(), collapse = "', '"))
    assign_reactive_val(flags, "merged", FALSE)
    hideStuffs()
  }
  
  # EC Number
  observeEvent(input$ecNumber, {
    log_event("Enzyme select using a ec number")
    if(!flags()$isThereUser){noSession()}
    else{
      shinyjs::disable("ecNumber")
      new_ec_number <- paste(input$ec_number1,
                             input$ec_number2,
                             input$ec_number3,
                             input$ec_number4,
                             sep = ".")
      log_value(new_ec_number)
      if(flags()$addEcNumber){ecno <- as.character(ecNumbers())
        ecno <- c(ecno, new_ec_number)
        ecNumbers(ecno)}
      else{ecNumbers(new_ec_number)}
      assign_reactive_val(flags, "addEcNumber", FALSE)
      generateProteinTable()
      assign_reactive_val(saved, "fastaSaved", FALSE)
      assign_reactive_val(saved, "pdbSaved", FALSE)
      assign_reactive_val(saved, "paramSaved", FALSE)
      shinyjs::enable("ecNumber")
    }
  })
  
  # Enzyme Name
  observeEvent(input$enzymeNameFinal, {
    log_event("Enzyme select using a synonym")
    log_value(input$pick)
    if(!flags()$isThereUser){noSession()}
    else{
      if(flags()$addEcNumber){ecno <- as.character(ecNumbers())
      ecno <- c(ecno, input$pick)
      ecNumbers(ecno)}
      else{ecNumbers(input$pick)}
      assign_reactive_val(flags, "addEcNumber", FALSE)
      shinyjs::disable("enzymeNameFinal")
      generateProteinTable()
      assign_reactive_val(saved, "fastaSaved", FALSE)
      assign_reactive_val(saved, "pdbSaved", FALSE)
      assign_reactive_val(saved, "paramSaved", FALSE)
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
    if(!flags()$proteinSearch){
      noProteins()
    } else{
      assign_reactive_val(flags, "addEcNumber", TRUE)
      updateTabItems(session, "inTabset", "enzyme")}
  })
  
  # Message to the enzyme searcher
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
  
  # Protein Table
  output$distProteinTable <- DT::renderDT({
    table <- savedTables()$proteinTable
    table$Ref <- NULL
    table$Literature.PubmedID. <- NULL
    attributes(table)$names <- lapply(attributes(table)$names, gsub, pattern ="link", replacement = "Literature.PubmedID.")
    if(!input$showComments1){table$Commentary <- NULL}
    if(!input$showLiterature1){table$Literature.PubmedID. <- NULL}
    if(!input$showTaxonomy){
      table <- erase_taxonomy(table)
    }
    selection <- list(target = "row")
    DT::datatable(table, escape = FALSE, filter = list(position = 'top', plain = TRUE),
                  selection = selection,
                  options = list(scrollX = TRUE,
                                 scrollY = 250,
                                 dom = "tip",
                                 pageLength = 10))
  })
  
  # Download Protein Table
  output$downloadProtein <- downloadHandler(
    filename <- 'protein_table.txt',
    content <- function(name){
      table <- savedTables()$proteinTable
      table$Ref <- NULL
      table$link <- NULL
      write.table(table, name, quote = FALSE, row.names = FALSE, sep = "\t")
    }
  )
  
  # Back to enzyme select
  observeEvent(input$toEnzyme, {
    updateTabItems(session, "inTabset", "enzyme")
    assign_reactive_val(flags, "addEcNumber", FALSE)
  })
  
  # Generate Fasta Table
  observeEvent(input$toFasta, {
    if(!flags()$proteinSearch){noSearch("Sequence")}
    else{
      searchFasta()
    }
  })
  
  searchFasta <- function(){
    withProgress(message = "Searching sequence", value = 0, {
      shinyjs::disable("toFasta")
      updateTabItems(session, "inTabset", "fasta")
      incProgress(0, detail = "Entering parameters")
      error <- 0
      if(!saved()$fastaSaved){
        ecno <- as.character(ecNumbers())
        main <- .jnew("cl.pesb2.best.BrendaSOAP", ecno[1], folder())
        n <- length(ecno)
        if(n > 1) lapply(ecno[2:n], function(e){
          main$addEnzyme(e)
        })
        incProgress(0.2, detail = showTime(timeFasta(length(ecNumbers()))))
        error <- main$getFastaSequence()
      }
      if(error == -1){handledJavaError(session, "Fasta")
        table <- NULL
      } else{
        if(!flags()$anonymousUser)
          alterCacheFasta(userRef())
        assign_reactive_val(flags, "fastaSearch", TRUE)
        incProgress(0.7, detail = "Showing")
        table <- new_table(paste(folder(), "report_fasta.txt", sep = ""))
        incProgress(0.1, detail = "Ready")}
    })
    shinyjs::enable("toFasta")
    assign_reactive_val(savedTables, "fastaTable", table)
    assign_reactive_val(saved, "fastaSaved", FALSE)
  }
  
  # Exception handler
  observeEvent(input$handlerJavaEFasta, {
    if(input$handlerJavaEFasta)
      searchFasta()
  })
  
  # Fasta Table
  output$fastaTable <- DT::renderDT({
    DT::datatable(savedTables()$fastaTable,
                  options = list(scrollX = TRUE,
                                 scrollY = 375,
                                 lengthMenu = c(5, 10, 50, 100),
                                 pageLength = 10))
  })
  
  # Download Fasta Sequence
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
  
  # Back to protein table
  observeEvent(input$toProtein1, {
    updateTabItems(session, "inTabset", "proteinTable")
  })
  
  # Generate PDB Table
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
          main <- .jnew("cl.pesb2.best.BrendaSOAP", ecno[1], folder())
          n <- length(ecno)
          if(n > 1) lapply(ecno[2:n], function(e){
            main$addEnzyme(e)
            })
          proteinTable <- savedTables()$proteinTable
          incProgress(0.2, detail = showTime(timePDB(nrow(proteinTable))*length(ecNumbers())))
          error <- main$getPDB()}
        if(error == 0){
          if(!flags()$anonymousUser)
            alterCachePDB(userRef())
          incProgress(0.7, detail = "Showing")
          table <- new_table(paste(folder(), "pdb_table.txt", sep = ""))
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
  
  # PDB Table
  output$pdbTable <- DT::renderDT({
    DT::datatable(savedTables()$pdbTable,
                  selection = 'none',
                  options = list(scrollX = TRUE,
                                 scrollY = 375,
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
  importParamTable <- function(n, ...){
    log_value(n)
    op <- list(...)
    only <- op$ref
    af <- flags()$attributeFound
    taf <- savedTables()$attrTable
    ftaf <- savedTables()$fattrTable
    table <- getParam(only, n)
    log_value(nrow(table))
    filterId <- paste(at[n], "Filter", sep = "")
    if(nrow(table) != 0){
      af[[n]] <- TRUE
      update_filter(session, table, filterId, nat[n], at[n])
      taf[[n]] <- table
      ftaf[[n]] <- table
    } else{
      update_filter_notFound(session, filterId, nat[n])
      af[[n]] <- FALSE
      taf[[n]] <- data.frame()
      ftaf[[n]] <- data.frame()
    }
    assign_reactive_val(savedTables, "attrTable", taf)
    assign_reactive_val(savedTables, "fattrTable", ftaf)
    assign_reactive_val(flags, "attributeFound", af)
  }
  eraseParam <- function(n, ...){
    af <- flags()$attributeFound
    taf <- savedTables()$attrTable
    ftaf <- savedTables()$fattrTable
    af[n] <- FALSE
    taf[[n]] <- data.frame()
    ftaf[[n]] <- data.frame()
    assign_reactive_val(flags, "attributeFound", af)
    assign_reactive_val(savedTables, "attrTable", taf)
    assign_reactive_val(savedTables, "fattrTable", ftaf)
  }
  
  # Add to output Table
  addTable <- function(n, ...){
    op <- list(...)
    table <- op$table
    with_mol <- op$with_mol
    out <- attr_collapse(n, savedTables()$fattrTable[[n]], with_mol)
    if (nrow(table) == 0) table <- out
    else table <- merge(table, out, by="Ref", all = TRUE)
    table
  }
  
  # Select all
  observeEvent(input$allParameters, {
    log_event("'All parameters' changed")
    if(input$allParameters) updateCheckboxGroupInput(session, "attributes", choices = at, selected = at)
    else updateCheckboxGroupInput(session, "attributes", choices = at, selected = c(NULL))
  })
  
  # Generate Parameter Table
  observeEvent(input$parameters, {
    log_event("Searching parameters")
    log_value(input$attributes)
    assign_reactive_val(flags, "paramSearch", TRUE)
    if(flags()$proteinSearch){
      shinyjs::disable("parameters")
      hideStuffs()
      updateTabItems(session, "inTabset", "parameterTable")
      withProgress(message = "Searching numerical Parameters", value = 0, {
        incProgress(0, detail = "Entering parameters...")
        ecno <- as.character(ecNumbers())
        log_value(ecno)
        n <- length(ecno)
        table <- savedTables()$proteinTable
        if(!saved()$paramSaved){
          incProgress(0.1, detail = "Selecting proteins for search")
          if (input$allProteins) {
            s <- input$distProteinTable_rows_all
          } else {
            s <- input$distProteinTable_rows_selected
          }
          log_value(length(s))
          log_value(nrow(table))
          table <- table[s, ]
          log_value(nrow(table))
          if (input$up){
            table <- table[!is.na(table$UniProt), ]
          }
          log_value(nrow(table))
          only <- paste(table$Ref, collapse = ", ")
          incProgress(0.2, detail = showTime(timeParameters(nrow(table))*length(ecNumbers())))
        }
        only <- paste(activeRef(), collapse = ", ")
        table <- erase_taxonomy(table)
        endGeneration(table, only, importParamTable)
        assign_reactive_val(savedTables, "parameterTable", table)
      })
    } else{
      noSearch("Parameters")
    }
  })
  
  # Parameter Table
  output$distParameterTable <- DT::renderDT({
    table <- savedTables()$fparameterTable
    table$Literature.PubmedID. <- NULL
    attributes(table)$names <- lapply(attributes(table)$names, gsub, pattern ="link", replacement = "Literature.PubmedID.")
    if(flags()$paramSearch){
      if(!input$showLiterature2){table$Literature.PubmedID. <- NULL}
      if(!input$showComments2){v <- grepl("Commentary", attributes(table)$names)
      table[,attributes(table)$names[v]] <- NULL}
      DT::datatable(table, escape = FALSE,
                    extensions = 'FixedColumns',
                    selection = 'none',
                    options = list(scrollX = TRUE,
                                   fixedColumns = list(leftColumns = 3),
                                   scrollY = 300,
                                   lengthMenu = c(2, 5, 10, 50, 100),
                                   pageLength = 5))
    } else{NULL}
  }, escape = FALSE)
  
  # End generation (used by filter and parameters searching)
  endGeneration <- function(table, only, principalFun){
    table1 <- data.frame()
    incProgress(0.5)
    listA <- list()
    assign_reactive_val(flags, "attributeFound", nullFlags$attributeFound)
    for(n in 1:12){
      do_function(n, eraseParam, principalFun, eraseParam, input$attributes, flags()$attributeFound, mol = molList[n], ref = only)
      log_value(flags()$attributeFound)
      table1 <- do_function(n, addTable, addNoTable, addNoTable, input$attributes, flags()$attributeFound, table = table1, with_mol = bool_mol[n])
      log_event("Rows on table")
      log_value(nat[n])
      log_value(nrow(table1))
      listA <- do_function(n, updateKmeans, addNoList, addNoList, input$attributes, flags()$attributeFound, listA = listA)
    }
    if (nrow(table1) != 0) table <- merge(table, table1, by="Ref", all = FALSE)
    updateCheckboxGroupInput(session, "kmeans", choices = listA)
    updateCheckboxGroupInput(session, "dbscanSelector", choices = listA)
    updateCheckboxGroupInput(session, "corScat", choices = listA)
    incProgress(0.05, detail = "Working on data")
    log_value(attributes(table)$names)
    refs <- table$Ref
    if(!flags()$anonymousUser)
      alterCacheParam(userRef(), paste(refs, collapse = ", "), input$attributes)
    table$Ref <- NULL
    incProgress(0.05, detail = "Ready")
    shinyjs::enable("parameters")
    assign_reactive_val(savedTables, "fparameterTable", table)
    assign_reactive_val(flags, "paramSaved", FALSE)
    assign_reactive_val(flags, "merged", FALSE)
  }
  
  # Download Parameter Table
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
  
  # Back to protein table
  observeEvent(input$toProtein3, {
    updateTabItems(session, "inTabset", "proteinTable")
  })

  # Extract amount information
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
  
  # Create filter parameter table
  filterParamTable <- function(n, ...){
    bf <- input[[sprintf("%s2", at[n])]]
    fv <- input[[sprintf("%sFilter", at[n])]]
    op <- list(...)
    only <- op$ref
    af <- flags()$attributeFound
    taf <- savedTables()$attrTable
    ftaf <- savedTables()$fattrTable
    log_value(bf)
    log_value(fv)
    if (bf){
      log_message("Get filtered")
      table <- getParam(only, n, min=as.double(fv[1]), max=as.double(fv[2]))
    } else {
      table <- taf[[n]]
    }
    log_value(nrow(table))
    if(nrow(table) != 0){
      af[[n]] <- TRUE
      ftaf[[n]] <- table
    } else{
      af[[n]] <- FALSE
      ftaf[[n]] <- data.frame()
    }
    assign_reactive_val(savedTables, "fattrTable", ftaf)
    assign_reactive_val(flags, "attributeFound", af)
  }
  
  # Filter
  observeEvent(input$filter, {
    if(!flags()$paramSearch){
      shinyalert("Cannot Filter",
                 "You need to look for parameters to filter them",
                 type = "error")
    } else{
      shinyjs::disable("filter")
      withProgress(message = "Filtering", value = 0, {
        table <- savedTables()$proteinTable
        table <- table[,c("EC_Number", "Organism", "Systematic_name", "Recommended_name", "UniProt", "Commentary", "Literature.PubmedID.", "link", "Ref")]
        only <- paste(table$Ref, collapse = ", ")
        endGeneration(table, only, filterParamTable)
      })
      shinyjs::enable("filter")
    }
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
  
  # Summary Table
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
  
  # Count Attributes
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
  
  # Generate Count plot
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
  
  # Count Plot
  output$histogram <- renderPlotly({
    savedPlots()$histPlot
  })
  
  
  # Back to parameter table
  observeEvent(input$backParameter1, {
    updateTabItems(session, "inTabset", "parameterTable")
  })
  
  # Generate Distribution
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
  
  # Distribution
  output$distributionOut <- renderPlotly({
    savedPlots()$distributionPlot
  })
  
  # Back to visualization
  observeEvent(input$backVisualization1, {
    updateTabItems(session, "inTabset", "histogram")
  })
  
  # Correlation
  correlationTable <- function(n, ...){
    tb <- simplify(savedTables()$fattrTable[[n]], nat[n])
  }
  
  # Generate Correlation
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
          tryCatch({
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
            }, error = function(cond){
              log_warning(cond)
              standardError("k-means clustering", conditionMessage(cond))
            }, warning = function(cond){
              log_warning(cond)
              standardError("k-means clustering", conditionMessage(cond))
            }
          )
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
      tryCatch({
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
      }, error = function(cond){
        log_warning(cond)
        standardError("DBSCAN clustering", conditionMessage(cond))
      }, warning = function(cond){
        log_warning(cond)
        standardError("DBSCAN clustering", conditionMessage(cond))
      })
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
  
})