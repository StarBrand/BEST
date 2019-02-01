#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)
library(rJava)
library(stringr)
library(ggplot2)
.jinit('BrendaSOAP.jar')

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  user <- eventReactive(input$logIn, {
    #.jnew("client.User",input$mail, input$pass)
  })
  
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
  
  encoder_filter<-function(){
    cond <- 0
    if(input$up){
      cond <- cond + 1
    }
    cond
  }
  
  observeEvent(input$logIn, {
    updateTabsetPanel(session, "inTabset",
                      selected = "proteinSearch")
  })
  
  observeEvent(input$ecNumber, {
    updateTabsetPanel(session, "protein",
                      selected = "proteinTable")
  })
  
  observeEvent(input$parameters, {
    updateTabsetPanel(session, "protein",
                      selected = "parameterTable")
  })
  
  observeEvent(input$sequence, {
    updateTabsetPanel(session, "protein",
                      selected = "uniprot")
  })
  
  output$distProteinTable <- DT::renderDT({
    # main <- .jnew('main.BrendaSOAP', input$ec_number, user(), as.integer(0), as.integer(0))
    # main$getProtein()
    DT::datatable(table <- read.table("table.txt", sep = '\t', na.strings = 'null', header = TRUE),
    options = list(lengthMenu = c(6, 10, 50, 100), pageLength = 6))
  })
  
  output$proteinTable <- downloadHandler(
    filename <- 'protein_table.csv',
    content <- function(name){
      write.csv(table, name)
      }
  )
  
  time <- function(){
    param <- 1
    if(input$mw){
      param <- param + 1
    }
    if(input$ic50){
      param <- param + 1
    }
    if(input$kc){
      param <- param + 1
    }
    if(input$ki){
      param <- param + 1
    }
    if(input$km){
      param <- param + 1
    }
    if(input$pho){
      param <- param + 1
    }
    if(input$phr){
      param <- param + 1
    }
    if(input$pi){
      param <- param + 1
    }
    if(input$sa){
      param <- param + 1
    }
    if(input$to){
      param <- param + 1
    }
    if(input$tr){
      param <- param + 1
    }
    if(input$ton){
      param <- param + 1
    }
    n_protein <- nrow(read.table("table.txt", sep = '\t', na.strings = 'null', header = TRUE))
    if(input$up){
      n_protein <- (n_protein %/% 10) + 1
    }
    queries <- param * n_protein
    time <- ( (queries * 2) %/% 60 ) + 1
  }
  
  output$timeSearch <- renderText({
    if( time() == 60 ){
      out <- paste("1 hour.", sep="")
    }
    else if( time() > 60 ){
      out <- paste( (time()%/%60) + 1, " hours.", sep="" )
    }
    else{
      out <- paste( time(), " minutes.", sep = "" )
    }
    out <- paste("The query is goint to take: ", out, sep ="")
  })
  
  output$longTimeSearch <- renderText({
    if( time() >= 60 ){
      out <- "Warning: This search is going to take more than an hour."
    }
    else{
      out <- ""
    }
    out
  })
  
  fasta <- function(){
    # main <- .jnew('main.BrendaSOAP', input$ec_number, user(), as.integer(0), as.integer(0))
    # main$getProtein()
    # main$getFastaSequence()
    read.table("fasta_output.txt", sep = '\t', header = FALSE);
  }
  
  output$distProteinSequence <- DT::renderDT({
    table <- data.frame(Enzyme = c(), Organism = c(), UniProt = c(), Found_with_Brenda = c())
    for( line in 1:nrow( fasta() ) ){
      if(startsWith(as.vector(fasta()[line, 1]), prefix = '>')){
        toAdd<-strsplit(as.vector(fasta()[line, 1]), ' ')[[1]]
        UniProt <- c(toAdd[1])
        Enzyme <- c(toAdd[2])
        organism <- ""
        for(i in 3:length(toAdd)){
          organism <- paste(organism, toAdd[i], sep = '')
        }
        Organism <- c(organism)
        if(startsWith(as.vector(fasta()[line+1,1]), prefix = '>')){
          Found_with_Brenda <- c(FALSE)
        }
        else{
          Found_with_Brenda <- c(TRUE)
        }
        table <- rbind(table, data.frame(Enzyme, Organism, UniProt, Found_with_Brenda))
      }
    }
    table
  })
  
  output$fasta <- downloadHandler(
    filename <- 'fasta.txt',
    content <- function(name){
      if(input$no_filter){
        write.table(fasta(), name, quote = FALSE, row.names = FALSE)
      }
      else{
        table <- data.frame(c=c())
        s <- input$distProteinSequence_rows_selected
        if(length(s)){
          table <- rbind(table, data.frame(c=c(s)))
        }
        write.table(table, name, quote = FALSE, row.names = FALSE)
      }
    }
  )
  
  output$distParameterTable <- renderDT({
    #main <- .jnew('main.BrendaSOAP', input$ec_number, user(),
                  #as.integer(encoder_param()),
                  #as.integer(encoder_filter()) )
    #main$getProtein()
    #main$getParameters()
    table2 <- read.table("table2.txt", sep = '\t', na.strings = 'null', header = TRUE)
    DT::datatable(table2 <- read.table("table2.txt", sep = '\t', na.strings = 'null', header = TRUE),
                  options = list(lengthMenu = c(6, 10, 50, 100), pageLength = 6))
  })
  
  
  output$parameterTable <- downloadHandler(
    filename <- 'parameter_table.csv',
    content <- function(name){
      write.csv(table2, name)
    }
  )
  
  min_max <- function(vectorIn){
    use <- str_split(vectorIn, ";")
    use <- unlist(use, recursive = TRUE)
    use <- str_split(use, "-")
    use <- unlist(use, recursive = TRUE)
    use <- sapply(use, 'as.double')
    a <- c(min(use, na.rm = TRUE), max(use, na.rm = TRUE))
  }
  
  observe({
    calc <- read.table("table2.txt", sep = '\t', na.strings = 'null', header = TRUE)
    if(input$mw){
      a <- min_max(calc$Molecular.Weight.value)
      updateSliderInput(session, "mwFilter", value = a,
                        min = a[1], max = a[2])
    }
  })
  
  observe({
    calc <- read.table("table2.txt", sep = '\t', na.strings = 'null', header = TRUE)
    if(input$ic50){
      a <- min_max(calc$IC50.value)
      updateSliderInput(session, "ic50Filter", value = a,
                        min = a[1], max = a[2])
    }
  })
  
  observe({
    calc <- read.table("table2.txt", sep = '\t', na.strings = 'null', header = TRUE)
    if(input$kc){
      a <- min_max(calc$Kcat.Km.value)
      updateSliderInput(session, "kcFilter", value = a,
                        min = a[1], max = a[2])
    }
  })
  
  observe({
    calc <- read.table("table2.txt", sep = '\t', na.strings = 'null', header = TRUE)
    if(input$ki){
      a <- min_max(calc$Ki.value)
      updateSliderInput(session, "kiFilter", value = a,
                        min = a[1], max = a[2])
    }
  })
  
  observe({
    calc <- read.table("table2.txt", sep = '\t', na.strings = 'null', header = TRUE)
    if(input$km){
      a <- min_max(calc$Km.value)
      updateSliderInput(session, "kmFilter", value = a,
                      min = a[1], max = a[2])
    }
  })
  
  observe({
    calc <- read.table("table2.txt", sep = '\t', na.strings = 'null', header = TRUE)
    if(input$pho){
      a <- min_max(calc$pH.Optimum.value)
      updateSliderInput(session, "phoFilter", value = a,
                        min = a[1], max = a[2])
    }
  })
  
  observe({
    calc <- read.table("table2.txt", sep = '\t', na.strings = 'null', header = TRUE)
    if(input$phr){
      a <- min_max(calc$pH.Range.value)
      updateSliderInput(session, "phrFilter", value = a,
                        min = a[1], max = a[2])
    }
  })
  
  observe({
    calc <- read.table("table2.txt", sep = '\t', na.strings = 'null', header = TRUE)
    if(input$pi){
      a <- min_max(calc$pI.value)
      updateSliderInput(session, "piFilter", value = a,
                        min = a[1], max = a[2])
    }
  })
  
  observe({
    calc <- read.table("table2.txt", sep = '\t', na.strings = 'null', header = TRUE)
    if(input$sa){
      a <- min_max(calc$Specific.Activity.value)
      updateSliderInput(session, "saFilter", value = a,
                        min = a[1], max = a[2])
    }
  })
  
  observe({
    calc <- read.table("table2.txt", sep = '\t', na.strings = 'null', header = TRUE)
    if(input$to){
      a <- min_max(calc$Temperature.Optimum.value)
      updateSliderInput(session, "toFilter", value = a,
                        min = a[1], max = a[2])
    }
  })
  
  observe({
    calc <- read.table("table2.txt", sep = '\t', na.strings = 'null', header = TRUE)
    if(input$tr){
      a <- min_max(calc$Temperature.Range.value)
      updateSliderInput(session, "trFilter", value = a,
                        min = a[1], max = a[2])
    }
  })
  
  observe({
    calc <- read.table("table2.txt", sep = '\t', na.strings = 'null', header = TRUE)    if(input$ton){
      a <- min_max(calc$Turnover.Number.value)
      updateSliderInput(session, "tonFilter", value = a,
                        min = a[1], max = a[2])
    }
  })
  
  
  output$histogram <- renderPlot({
    a <- table2[grepl('value' , apply(data.frame(attributes(table2)[1]), 2, identity))]
    data <- data.frame(attribute = c(), mark = c(), ndata = c())
    j <- 1
    for(i in attributes(a)$names){
      data <- rbind(
        data,
        data.frame(attribute = i,
                   mark = j,
                   ndata = sum(str_count(a[, i], ";") + 1, na.rm = TRUE))
      )
      j <- j + 1
    }
    ggplot(data, aes(x=mark,y =ndata, fill=attribute )) +
      geom_bar(stat = 'identity') +
      xlab("Parameter") + 
      ylab("Count") +
      scale_fill_brewer(palette = "Set3")
      
  })
  
})
