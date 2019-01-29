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

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  tabsetPanel(type = "tabs",
              
              tabPanel("Home", value = "home", fluid = TRUE,
                       sidebarLayout(
                         sidebarPanel(
                           'Brenda User',
                           textInput('mail', label = 'Mail'),
                           textInput('pass', label = 'password'),
                           actionButton("user", "Enter")
                           ),
                         mainPanel("...")
                         )
                       ),
              
              tabPanel('Protein Search', value = 'proteinSearcf', fluid = TRUE,
                       mainPanel(
                         tabsetPanel(
                           
                           tabPanel("Enzyme", value = "enzyme", fluid = TRUE,
                                    sidebarLayout(
                                      sidebarPanel(
                                        "Choose input",
                                        textInput('ec_number', label = 'Enter EC Number', value = "6.6.1.2"),
                                        actionButton("ecNumber", "Enter"),
                                        textInput('enzyme_name', label = 'Enter the name of the enzyme'),
                                        actionButton("enzymeName", "Enter"),
                                        fileInput('uniprot_file', label = 'Select your UniProt file', buttonLabel = "Browse...", placeholder = "No file selected"),
                                        actionButton("uniprotFile", "Enter")
                                        ),
                                      mainPanel("...")
                                      )
                                    ),
                           
                           tabPanel("Protein Table", value = "proteinPanel" ,fluid = TRUE,
                                    sidebarLayout(
                                      sidebarPanel(
                                        'Parameters to be included:',
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
                                        'Filters:',
                                        checkboxInput(inputId = 'up',
                                                      label = 'Proteins with UniProt code'
                                                      ),
                                        actionButton("parameters", "Enter")
                                        ),
                                      
                                      mainPanel(downloadButton(
                                        'proteinTable',
                                        label = "Download Table"
                                        ),
                                        DTOutput("distProteinTable"))
                                      )
                                    ),
                           tabPanel("Parameters Table", value = "parameterPanel", fluid = TRUE,
                                    mainPanel(downloadButton(
                                      'parameterTable',
                                      label = "Dowload Table"
                                      ),
                                      tableOutput("distParameterTable")
                                      )
                                    ),
                           tabPanel("Plots", value = "plotPanel", fluid = TRUE
                                    )
                           )
                         )
                       ),
              
              tabPanel("Help", fluid = TRUE
                       
                       ),
              tabPanel("Acknowledgements", fluid = TRUE),
              tabPanel("FAQ", fluid =TRUE)
              )
  )
)