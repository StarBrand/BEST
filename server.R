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
.jinit('BrendaSOAP.jar')

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  user <- eventReactive(input$logIn, {
    .jnew("client.User",input$mail, input$pass)
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
  
  output$distProteinTable <- DT::renderDT({
    main <- .jnew('main.BrendaSOAP', input$ec_number, user(), as.integer(0), as.integer(0))
    main$getProtein()
    table <- read.table("table.txt", sep = '\t', na.strings = 'null', header = TRUE)
  })
  
  output$proteinTable <- downloadHandler(
    filename <- 'protein_table.csv',
    content <- function(name){
      write.csv(table, name)
      }
  )
  
  fasta <- function(){
    main <- .jnew('main.BrendaSOAP', input$ec_number, user(), as.integer(0), as.integer(0))
    main$getProtein()
    main$getFastaSequence()
    read.table("fasta_output.txt", sep = '\t');
  }
  
  output$fasta <- downloadHandler(
    filename <- 'fasta.txt',
    content <- function(name){
      write.table(fasta(), name)
    }
  )
  
  output$distParameterTable <- renderTable({
    main <- .jnew('main.BrendaSOAP', input$ec_number, user(),
                  as.integer(encoder_param()),
                  as.integer(encoder_filter()) )
    main$getProtein()
    main$getParameters()
    table <- read.table("table.txt", sep = '\t', na.strings = 'null', header = TRUE)
  })
  
  output$parameterTable <- downloadHandler(
    filename <- 'parameter_table.csv',
    content <- function(name){
      write.csv(table, name)
    }
  )
  
})
