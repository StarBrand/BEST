#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(DT)
library(shiny)
library(shinyjs)

# Define UI for application that draws a histogram
shinyUI(fluidPage(useShinyjs(),
                  navbarPage("Title", id = "inTabset",
                             tabPanel(title = "Home",
                                      value = "home",
                             sidebarLayout(
                               sidebarPanel(
                                 h2("Brenda User"),
                                 textInput(inputId = "mail",
                                           label = "Mail"),
                                 textInput(inputId = "pass",
                                           label = "password"),
                                 actionButton(inputId = "logIn",
                                              label = "Log in",
                                              class = "btn-primary")
                                 ),
                               mainPanel(h1("Welcome"),
                                         p(textOutput("user"))
                                         )
                               )
                          
                             ),
                             tabPanel(title = "Protein Search",
                                      value = "proteinSearch",
                                      sidebarLayout(
                                        sidebarPanel(
                                          
                                          conditionalPanel(condition = "input.protein == 'enzyme'",
                                                           p("Chooce an input:"),
                                                           textInput("ec_number", "Enter EC Number"),
                                                           actionButton("ecNumber", "Enter", class = "btn-primary"),
                                                           textInput("enzyme_name", "Enzyme name"),
                                                           fileInput("uniprot", "Uniprot File")
                                                           ),
                                          
                                          conditionalPanel(condition = "input.protein == 'proteinTable'",
                                                           p('Parameters to be included:'),
                                                           checkboxInput(inputId = 'mw',
                                                                         label = 'Molecular Weight'
                                                           ),
                                                           checkboxInput(inputId = 'ic50',
                                                                         label = 'IC50 Value'
                                                           ),
                                                           checkboxInput(inputId = 'kc',
                                                                         label = 'Kcat/Km'
                                                           ),
                                                           checkboxInput(inputId = 'ki',
                                                                         label = 'Ki Value'
                                                           ),
                                                           checkboxInput(inputId = 'km',
                                                                         label = 'Km Value'
                                                           ),
                                                           checkboxInput(inputId = 'pho',
                                                                         label = 'pH Optimum'
                                                           ),
                                                           checkboxInput(inputId = 'phr',
                                                                         label = 'pH Range'
                                                           ),
                                                           checkboxInput(inputId = 'pi',
                                                                         label = 'pI Value'
                                                           ),
                                                           checkboxInput(inputId = 'sa',
                                                                         label = 'Specific Activity'
                                                           ),
                                                           checkboxInput(inputId = 'to',
                                                                         label = 'Temperature Optimum'
                                                           ),
                                                           checkboxInput(inputId = 'tr',
                                                                         label = 'Temperature Range'
                                                           ),
                                                           checkboxInput(inputId = 'ton',
                                                                         label = 'TurnOver Number'
                                                           ),
                                                           p('Filters:'),
                                                           checkboxInput(inputId = 'up',
                                                                         label = 'Proteins with UniProt code'
                                                           ),
                                                           actionButton("parameters", "Enter", class = "btn-primary")),
                                          
                                          conditionalPanel(condition = "input.protein == 'parameterTable'",
                                                           h3("Parameter Table")),
                                          
                                          conditionalPanel(condition = "input.protein == 'plot'",
                                                           h3("Plot"))
                                          ),
                                        mainPanel(
                                          tabsetPanel(id = "protein",
                                                      tabPanel(title = "Enzyme",
                                                               value = "enzyme"),
                                                      tabPanel(title = "Protein Table",
                                                               value = "proteinTable",
                                                               downloadButton("proteinTable", "Download Table", class = "btn-primary"),
                                                               downloadButton("fasta", "Download FASTA sequence", class = "btn-secondary"),
                                                               DTOutput("distProteinTable")),
                                                      tabPanel(title = "Parameter Table",
                                                               value = "parameterTable",
                                                               downloadButton("parameterTable", "Download Table", class = "btn-primary"),
                                                               DTOutput("distParameterTable")),
                                                      tabPanel(title = "Plots",
                                                               value = "plot",
                                                               h1("¯\\_(ツ)_/¯"))
                                                      )
                                        )
                                        )
                             ),
                             tabPanel(title = "Help",
                                      value = "help"
                             ),
                             tabPanel(title = "Acknowledgments",
                                      value = "acknowledgments"
                             ),
                             tabPanel(title = "FAQ",
                                      value = "faq"
                             )
                             )
                  )
        )