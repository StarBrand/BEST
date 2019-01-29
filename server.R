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
user <- .jnew("client.User","juan.saez.hidalgo@gmail.com", "BrendaUser32")

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
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
  
  observeEvent(input$ecNumber, {
    updateTabsetPanel(session, "tabs",
                      selected = "proteinPanel")
  })
  
  observeEvent(input$parameters, {
    updateTabsetPanel(session, "tabs",
                      selected = "parameterPanel")
  })
  
  output$distProteinTable <- DT::renderDT({
    input$ecNumber
    main <- .jnew('main.BrendaSOAP', input$ec_number, user, as.integer(0), as.integer(0))
    main$getProtein()
    table <- read.table("table.txt", sep = '\t', na.strings = 'null', header = TRUE)
    })
  
  output$proteinTable <- downloadHandler(
    filename <- 'protein_table.csv',
    content <- function(name){
      write.csv(table, name)
      }
  )
  
  output$distParameterTable <- renderTable({
    input$parameters
    main <- .jnew('main.BrendaSOAP', input$ec_number, user,
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
