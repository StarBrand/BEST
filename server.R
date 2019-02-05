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
.jinit('BrendaSOAP.jar')

# Define server logic
shinyServer(function(input, output, session) {
  
  # Create user
  user <- eventReactive(input$logIn, {
    .jnew("client.User",input$mail, input$pass)
  })
  
  
  # Disable user
  observeEvent(input$logIn, {
    show("logOut")
    disable("mail")
    hide("pass")
    hide("logIn")
  })
  
  # Enable user
  observeEvent(input$logOut, {
    show("logIn")
    enable("mail")
    show("pass")
    hide("logOut")
    updateTextInput(session, "mail", value = "")
    updateTextInput(session, "pass", value = "")
  })
  
  # Folder
  folder <- reactiveVal("_results\\")
  
  # Set folder
  observeEvent(input$logIn, {
    new_folder <- paste(input$mail, folder(), sep = "")
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
  
  # Change to Protein Table
  observeEvent(input$ecNumber, {
    updateTabItems(session, "inTabset", "proteinTable")
  })
  
  # Protein Table
  output$distProteinTable <- DT::renderDT({
    main <- .jnew("main.BrendaSOAP", input$ec_number, user(), as.integer(0), as.integer(0))
    main$getProtein()
    DT::datatable(table <- read.table(paste(folder(), "table.txt", sep = ""), sep = '\t', na.strings = 'null', header = TRUE),
                  options = list(scrollX = TRUE, lengthMenu = c(5, 10, 50, 100), pageLength = 5))
  })
  
  # Change to Parameter Table
  observeEvent(input$parameters, {
    updateTabItems(session, "inTabset", "parameterTable")
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
  
  # Parameter Table
  output$distParameterTable <- DT::renderDT({
    main <- .jnew("main.BrendaSOAP", input$ec_number, user(), as.integer(encoder_param()), as.integer(encoder_filter()))
    main$getProtein()
    int <- input$distProteinTable_rows_selected - 1
    int <- as.integer(int)
    int <- .jarray(int)
    main$getParameters(int, input$allProteins)
    attr_folder <- paste(folder(), "attributes\\", sep = "")
    table2 <- read.table(
      paste(folder(),"table.txt", sep = ""),
      sep = '\t',
      na.strings = 'null',
      header = TRUE)
    if(input$mw){
      table1 <- attr_collapse(
        "Molecular_Weight",
        read.table(
          paste(attr_folder, "Molecular Weight.txt", sep = ""),
          header = TRUE,
          na.strings = "null",
          sep = "\t"
        ))
      table2 <- merge(table2, table1, by="Ref", all = TRUE)
    }
    if(input$ic50){
      table1 <- attr_collapse_mol(
        "IC50",
        read.table(
          paste(attr_folder, "IC50.txt", sep = ""),
          header = TRUE,
          na.strings = "null",
          sep = "\t"
        ),
        "Inhibitor")
      table2 <- merge(table2, table1, by="Ref", all = TRUE)
    }
    if(input$kc){
      table1 <- attr_collapse_mol(
        "Kcat_Km",
        read.table(
          paste(attr_folder, "Kcat_Km.txt", sep = ""),
          header = TRUE,
          na.strings = "null",
          sep = "\t"
        ),
        "Substrate")
      table2 <- merge(table2, table1, by="Ref", all = TRUE)
    }
    if(input$ki){
      table1 <- attr_collapse_mol(
        "Ki",
        read.table(
          paste(attr_folder, "Ki.txt", sep = ""),
          header = TRUE,
          na.strings = "null",
          sep = "\t"
        ),
        "Inhibitor")
      table2 <- merge(table2, table1, by="Ref", all = TRUE)
    }
    if(input$km){
      table1 <- attr_collapse_mol(
        "Km",
        read.table(
          paste(attr_folder, "Km.txt", sep = ""),
          header = TRUE,
          na.strings = "null",
          sep = "\t"
        ),
        "Substrate")
      table2 <- merge(table2, table1, by="Ref", all = TRUE)
      table2$Km_Substrate <- gsub("more", "1,4-beta-D-xylan", table2$Km_Substrate)
    }
    if(input$pho){
      table1 <- attr_collapse(
        "pH_Optimum",
        read.table(
          paste(attr_folder, "pH Optimum.txt", sep = ""),
          header = TRUE,
          na.strings = "null",
          sep = "\t"
        ))
      table2 <- merge(table2, table1, by="Ref", all = TRUE)
    }
    if(input$phr){
      table1 <- attr_collapse(
        "pH Range",
        read.table(
          paste(attr_folder, "pH Range.txt", sep = ""),
          header = TRUE,
          na.strings = "null",
          sep = "\t"
        ))
      table2 <- merge(table2, table1, by="Ref", all = TRUE)
    }
    if(input$pi){
      table1 <- attr_collapse(
        "pI",
        read.table(
          paste(attr_folder, "pI.txt", sep = ""),
          header = TRUE,
          na.strings = "null",
          sep = "\t"
        ))
      table2 <- merge(table2, table1, by="Ref", all = TRUE)
    }
    if(input$sa){
      table1 <- attr_collapse(
        "Specific_Activity",
        read.table(
          paste(attr_folder, "Specific Activity.txt", sep = ""),
          header = TRUE,
          na.strings = "null",
          sep = "\t"
        ))
      table2 <- merge(table2, table1, by="Ref", all = TRUE)
    }
    if(input$to){
      table1 <- attr_collapse(
        "Temperature_Optimum",
        read.table(
          paste(attr_folder, "Temperature Optimum.txt", sep = ""),
          header = TRUE,
          na.strings = "null",
          sep = "\t"
        ))
      table2 <- merge(table2, table1, by="Ref", all = TRUE)
    }
    if(input$tr){
      table1 <- attr_collapse(
        "Temperature_Range",
        read.table(
          paste(attr_folder, "Temperature Range.txt", sep = ""),
          header = TRUE,
          na.strings = "null",
          sep = "\t"
        ))
      table2 <- merge(table2, table1, by="Ref", all = TRUE)
    }
    if(input$ton){
      table1 <- attr_collapse_mol(
        "Turnover_Number",
        read.table(
          paste(attr_folder, "Turnover Number.txt", sep = ""),
          header = TRUE,
          na.strings = "null",
          sep = "\t"
        ),
        "Substrate")
      table2 <- merge(table2, table1, by="Ref", all = TRUE)
    }
    table2 <- table2[sort(table2$Ref, decreasing = FALSE),]
    table2 <- data.frame(lapply(table2, function(i){
      gsub("-999", "Aditional Information", i)}))
    table2$Ref <- NULL
    DT::datatable(table2, options = list(scrollX = TRUE, lengthMenu = c(5, 10, 50, 100), pageLength = 5))
  })
  
  # Update slider to filter parameter table
  observe({
    attr_folder <- paste(folder(), "attributes\\", sep = "")
    if(input$mw){
      calc <- read.table(
        paste(attr_folder, "Molecular Weight.txt", sep = ""),
        sep = '\t', na.strings = 'null', header = TRUE)
      a <- min_max(calc$value)
      updateSliderInput(session, "mwFilter", value = a,
                        min = a[1], max = a[2])
    }
  })
  
  observe({
    attr_folder <- paste(folder(), "attributes\\", sep = "")
    if(input$ic50){
      calc <- read.table(
        paste(attr_folder, "IC50.txt", sep = ""),
        sep = '\t', na.strings = 'null', header = TRUE)
      a <- min_max(calc$value)
      updateSliderInput(session, "ic50Filter", value = a,
                        min = a[1], max = a[2])
    }
  })
  
  observe({
    attr_folder <- paste(folder(), "attributes\\", sep = "")
    if(input$kc){
      calc <- read.table(
        paste(attr_folder, "Kcat_Km.txt", sep = ""),
        sep = '\t', na.strings = 'null', header = TRUE)
      a <- min_max(calc$value)
      updateSliderInput(session, "kcFilter", value = a,
                        min = a[1], max = a[2])
    }
  })
  
  observe({
    attr_folder <- paste(folder(), "attributes\\", sep = "")
    if(input$ki){
      calc <- read.table(
        paste(attr_folder, "Ki.txt", sep = ""),
        sep = '\t', na.strings = 'null', header = TRUE)
      a <- min_max(calc$value)
      updateSliderInput(session, "kiFilter", value = a,
                        min = a[1], max = a[2])
    }
  })
  
  observe({
    attr_folder <- paste(folder(), "attributes\\", sep = "")
    if(input$km){
      calc <- read.table(
        paste(attr_folder, "Km.txt", sep = ""),
        sep = '\t', na.strings = 'null', header = TRUE)
      a <- min_max(calc$value)
      updateSliderInput(session, "kmFilter", value = a,
                        min = a[1], max = a[2])
    }
  })
  
  observe({
    attr_folder <- paste(folder(), "attributes\\", sep = "")
    if(input$pho){
      calc <- read.table(
        paste(attr_folder, "pH Optimum.txt", sep = ""),
        sep = '\t', na.strings = 'null', header = TRUE)
      a <- min_max(calc$value)
      updateSliderInput(session, "phoFilter", value = a,
                        min = a[1], max = a[2])
    }
  })
  
  observe({
    attr_folder <- paste(folder(), "attributes\\", sep = "")
    if(input$phr){
      calc <- read.table(
        paste(attr_folder, "pH Range.txt", sep = ""),
        sep = '\t', na.strings = 'null', header = TRUE)
      a <- min_max(calc$value)
      updateSliderInput(session, "phrFilter", value = a,
                        min = a[1], max = a[2])
    }
  })
  
  observe({
    attr_folder <- paste(folder(), "attributes\\", sep = "")
    if(input$pi){
      calc <- read.table(
        paste(attr_folder, "pI.txt", sep = ""),
        sep = '\t', na.strings = 'null', header = TRUE)
      a <- min_max(calc$value)
      updateSliderInput(session, "piFilter", value = a,
                        min = a[1], max = a[2])
    }
  })
  
  observe({
    attr_folder <- paste(folder(), "attributes\\", sep = "")
    if(input$sa){
      calc <- read.table(
        paste(attr_folder, "Specific Activity.txt", sep = ""),
        sep = '\t', na.strings = 'null', header = TRUE)
      a <- min_max(calc$value)
      updateSliderInput(session, "saFilter", value = a,
                        min = a[1], max = a[2])
    }
  })
  
  observe({
    attr_folder <- paste(folder(), "attributes\\", sep = "")
    if(input$to){
      calc <- read.table(
        paste(attr_folder, "Temperature Optimum", sep = ""),
        sep = '\t', na.strings = 'null', header = TRUE)
      a <- min_max(calc$value)
      updateSliderInput(session, "toFilter", value = a,
                        min = a[1], max = a[2])
    }
  })
  
  observe({
    attr_folder <- paste(folder(), "attributes\\", sep = "")
    if(input$tr){
      calc <- read.table(
        paste(attr_folder, "Temperature Range.txt", sep = ""),
        sep = '\t', na.strings = 'null', header = TRUE)
      a <- min_max(calc$value)
      updateSliderInput(session, "trFilter", value = a,
                        min = a[1], max = a[2])
    }
  })
  
  observe({
    attr_folder <- paste(folder(), "attributes\\", sep = "")
    if(input$ton){
      calc <- read.table(
        paste(attr_folder, "Turnover Number.txt", sep = ""),
        sep = '\t', na.strings = 'null', header = TRUE)
      a <- min_max(calc$value)
      updateSliderInput(session, "tonFilter", value = a,
                        min = a[1], max = a[2])
    }
  })
  
  
})
