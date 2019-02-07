#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(DT)
library(shinyjs)
library(shinyWidgets)
library(shinyalert)
library(plotly)

dashboardPage(skin = "green",
  dashboardHeader(title = "Title"),
  dashboardSidebar(
    sidebarMenu(
      id = "inTabset",
      useShinyjs(),
      menuItem("Home", tabName = "home", icon = icon("home"), startExpanded = FALSE,
               menuSubItem("Account", tabName = "account", icon = icon("user")),
               menuSubItem("Tutorial", tabName = "help", icon = icon("graduation-cap"))
               ),
      menuItem("Protein Search", tabName = "protein", icon = icon("bug"), startExpanded = FALSE,
               menuSubItem("Select Enzyme", tabName = "enzyme"),
               menuSubItem("Protein Table", tabName = "proteinTable"),
               menuSubItem("FASTA", tabName = "fasta"),
               menuSubItem("PDB", tabName = "pdb"),
               menuSubItem("Parameter Table", tabName = "parameterTable"),
               menuSubItem("Information Available", tabName = "info")
               ),
      menuItem("Visualization", tabName = "visualization", icon = icon("chart-area"), startExpanded = TRUE,
               menuSubItem("Parameters Found", tabName = "histogram"),
               menuSubItem("Distribution", tabName = "distribution"),
               menuSubItem("Correlation", tabName = "correlation")
               ),
      menuItem("Acknowledgments", tabName = "acknowledgments", icon = icon("award")),
      menuItem("FAQ", tabName = "faq", icon = icon("question")),
      tags$hr(),
      div(style="text-align:center",
      h3("First time?"),
      helpText("You can always read"),
      helpText("the turorial section "),
      actionLink("help", "here"))
    )
  ),
  dashboardBody(
    useShinyjs(),
    useShinyalert(),
    tabItems(
      # Log in
      tabItem(tabName = "account",
              box( title = "Brenda User", width = 3,
                   tags$br(),
                   textInput(inputId = "mail",
                             label = "Mail"),
                   passwordInput(inputId = "pass",
                             label = "password"),
                   div(
                   actionButton(inputId = "logIn",
                                label = "log in",
                                class = "btn-success"),
                   style = "float:right"),
                   hidden(
                   actionButton(inputId = "logOut",
                                label = "log out",
                                class = "btn-danger"))
              ),
              box(width = 9, height = '70vh',
                img(src = 'Referential.jpg',
                    height = '350vh',
                    style = "display: block; margin-left: auto; margin-right: auto;")
              )
      ),
      
      
      # Help
      tabItem(tabName = "help",
              h1("Tutorial")
      ),
      
      # Enzyme
      tabItem(tabName = "enzyme",
              box( title = "Select Enzyme", width = 4,
                   div(style='height:70vh; overflow-y: scroll',
                   h3("Chooce an input"),
                   helpText("The ... app allows multiple type of",
                            "inputs. Such as enter the ec number, ",
                            "write the name of the enzyme or ",
                            "upload a .txt file with UniProt code",
                            "of your enzymes of interest"),
                   tags$br(),
                   h4("Enter EC Number"),
                   div(style="display: inline-block;vertical-align:top;width: 50px;",
                   selectizeInput("ec_number1", "",
                                  choices = read.table("ecNumber\\select.txt",
                                                       sep = "\t", header = FALSE,
                                                       col.names = "1")
                                  )),
                   div(style="display: inline-block;vertical-align:bottom;width: 2px;",
                       p(".")),
                   div(style="display: inline-block;vertical-align:top;width: 50px;",
                   selectizeInput("ec_number2", "", choices = NULL)),
                   div(style="display: inline-block;vertical-align:bottom;width: 2px;",
                       p(".")),
                   div(style="display: inline-block;vertical-align:top;width: 50px;",
                   selectizeInput("ec_number3", "", choices = NULL)),
                   div(style="display: inline-block;vertical-align:bottom;width: 2px;",
                       p(".")),
                   div(style="display: inline-block;vertical-align:top;width: 50px;",
                   selectizeInput("ec_number4", "", choices = NULL)),
                   tags$br(),
                   div(style = "float:right",
                   actionButton("ecNumber", "Enter", class = "btn-primary")),
                   tags$hr(),
                   selectInput("subclass", "Now the subclass of the enzyme type?",
                                  choices = list("Oxidoreductases" = "1",
                                                 "Transferases" = "2",
                                                 "Hydrolases" = "3",
                                                 "Lyases" = "4",
                                                 "Isomerases" = "5",
                                                 "Ligases" = "6")),
                   selectizeInput("enzyme_name", "Select a Name of an Enzyme", choices = NULL),
                   div(style = "float:right",
                   actionButton("enzymeName", "Search", class = "btn-primary")),
                   shinyjs::hidden(
                     radioButtons("pick", "Which one of these?",
                                  choices = list("..."))),
                   div(style = "float:right",
                   shinyjs::hidden(
                     actionButton("enzymeNameFinal", "Go", class = "btn-success")
                   )),
                   tags$br(),
                   tags$hr(),
                   fileInput("uniprot_file", "Upload your uniprot file"))
              )
      ),
      
      # Protein Table
      tabItem(tabName = "proteinTable",
                box(title = "Select Parameters", width = 3, height = '80vh',
                    div(style='height:70vh; overflow-y: scroll',
                    h4('Parameters to be look up'),
                    div(style="display: inline-block;vertical-align:top;width: 15vh;", checkboxInput(inputId = 'mw', label = 'Molecular Weight')),
                    div(style="display: inline-block;vertical-align:top;width: 2vh;", HTML("<br>")),
                    div(style="display: inline-block;vertical-align:top;width: 15vh;", checkboxInput(inputId = 'ic50', label = 'IC50 Value')),
                    div(style="display: inline-block;vertical-align:top;width: 15vh;", checkboxInput(inputId = 'kc', label = 'Kcat/Km')),
                    div(style="display: inline-block;vertical-align:top;width: 2vh;", HTML("<br>")),
                    div(style="display: inline-block;vertical-align:top;width: 15vh;", checkboxInput(inputId = 'ki', label = 'Ki Value')),
                    div(style="display: inline-block;vertical-align:top;width: 15vh;", checkboxInput(inputId = 'km', label = 'Km Value')),
                    div(style="display: inline-block;vertical-align:top;width: 2vh;", HTML("<br>")),
                    div(style="display: inline-block;vertical-align:top;width: 15vh;", checkboxInput(inputId = 'pho', label = 'pH Optimum')),
                    div(style="display: inline-block;vertical-align:top;width: 15vh;", checkboxInput(inputId = 'phr', label = 'pH Range')),
                    div(style="display: inline-block;vertical-align:top;width: 2vh;", HTML("<br>")),
                    div(style="display: inline-block;vertical-align:top;width: 15vh;", checkboxInput(inputId = 'pi', label = 'pI Value')),
                    div(style="display: inline-block;vertical-align:top;width: 15vh;", checkboxInput(inputId = 'sa', label = 'Specific Activity')),
                    div(style="display: inline-block;vertical-align:top;width: 2vh;", HTML("<br>")),
                    div(style="display: inline-block;vertical-align:top;width: 15vh;", checkboxInput(inputId = 'to', label = 'Temperature Optimum')),
                    div(style="display: inline-block;vertical-align:top;width: 15vh;", checkboxInput(inputId = 'tr', label = 'Temperature Range')),
                    div(style="display: inline-block;vertical-align:top;width: 2vh;", HTML("<br>")),
                    div(style="display: inline-block;vertical-align:top;width: 15vh;", checkboxInput(inputId = 'ton', label = 'TurnOver Number')),
                    tags$hr(),
                    h4("Additional Options:"),
                    checkboxInput(inputId = 'up',
                                  label = 'Look up just for the proteins that has UniProt code'
                                  ),
                    actionButton("parameters", "Search for parameters", class = "btn-success"),
                    tags$br(),
                    helpText("Here you can obtain numerical parameters",
                             "of the enzymes you want to."))
                ),
                box(
                  title = "Protein Table", width = 9,
                  div(style="display: inline-block;", materialSwitch("allProteins", "Serch for all the list", right = TRUE, value = TRUE, status = "primary")),
                  div(style="display: inline-block;vertical-align:top;width: 5vh;", HTML("<br>")),
                  div(style="display: inline-block;",actionButton("toFasta", "Get sequence", class = "btn-success")),
                  div(style="display: inline-block;",actionButton("toPDB", "Get PDB", class = "btn-success")),
                  tags$br(),
                  tags$br(),
                  div(style='height:60vh; overflow-y: scroll',
                  DTOutput("distProteinTable", height = '60vh'))
                )
      ),
      
      # FASTA sequence
      tabItem(tabName = "fasta",
              box(title = "Options", width = 3,
                  div(style='height:70vh; overflow-y: scroll',
                      p("Select the proteins you want to download"),
                      p("or"),
                      materialSwitch("no_filter", "Download all the list", right = TRUE, value = TRUE),
                      tags$hr(),
                      p("There are some sequence that couldn't ",
                        "find using Brenda",
                        "What do you want to do about it?"),
                      tags$br(),
                      radioButtons("searchFasta", "Search for other database?",
                                   choices = list("Uniprot from Rcpi" = 1,
                                                  "Leave it blank" = 0)))
              ),
              
              box(title = "FASTA Sequence", width = 9,
                  tags$br(),
                  downloadButton("downloadFASTA", "Download sequence in FASTA format", class = "btn-success"),
                  tags$br(),
                  div(style='height:70vh; overflow-y: scroll',
                  DTOutput("fastaTable"))
              )
      
      ),
      
      # PDB
      tabItem(tabName = "pdb",
              box(title = "PDB Table", width = 12,
                  tags$br(),
                  div(style='height:70vh; overflow-y: scroll',
                  DTOutput("pdbTable"))
              )
      ),
      
      # Parameter Table
      tabItem(tabName = "parameterTable",
              box(
                title = "Filters", width = 4, height = '80vh',
                div(style='height:70vh; overflow-y: scroll',
                conditionalPanel(condition = "input.mw",
                                 sliderInput("mwFilter", h5("Filter Molecular Weight"),
                                             min = 0,
                                             max = 100,
                                             value = c(
                                               0,
                                               100
                                             )
                                 )
                ),
                
                conditionalPanel(condition = "input.ic50",
                                 sliderInput("ic50Filter", h5("Filter IC50 Value"),
                                             min = 0,
                                             max = 100,
                                             value = c(
                                               0,
                                               100
                                             )
                                 )
                ),
                
                conditionalPanel(condition = "input.kc",
                                 sliderInput("kcFilter", h5("Filter Kcat/Km value"),
                                             min = 0,
                                             max = 100,
                                             value = c(
                                               0,
                                               100
                                             )
                                 )
                ),
                
                conditionalPanel(condition = "input.ki",
                                 sliderInput("kiFilter", h5("Filter Ki value"),
                                             min = 0,
                                             max = 100,
                                             value = c(
                                               0,
                                               100
                                             )
                                 )
                ),
                
                conditionalPanel(condition = "input.km",
                                 sliderInput("kmFilter", h5("Filter Km value"),
                                             min = 0,
                                             max = 100,
                                             value = c(
                                               0,
                                               100
                                             )
                                 )
                ),
                
                conditionalPanel(condition = "input.pho",
                                 sliderInput("phoFilter", h5("Filter pH Optimum value"),
                                             min = 0,
                                             max = 100,
                                             value = c(
                                               0,
                                               100
                                             )
                                 )
                ),
                
                conditionalPanel(condition = "input.phr",
                                 sliderInput("phrFilter", h5("Filter pH Range value"),
                                             min = 0,
                                             max = 100,
                                             value = c(
                                               0,
                                               100
                                             )
                                 )
                ),
                
                conditionalPanel(condition = "input.pi",
                                 sliderInput("piFilter", h5("Filter pI value"),
                                             min = 0,
                                             max = 100,
                                             value = c(
                                               0,
                                               100
                                             )
                                 )
                ),
                
                conditionalPanel(condition = "input.sa",
                                 sliderInput("saFilter", h5("Filter Specific Activity"),
                                             min = 0,
                                             max = 100,
                                             value = c(
                                               0,
                                               100
                                             )
                                 )
                ),
                
                conditionalPanel(condition = "input.to",
                                 sliderInput("toFilter", h5("Filter Temperature Optimum value"),
                                             min = 0,
                                             max = 100,
                                             value = c(
                                               0,
                                               100
                                             )
                                 )
                ),
                
                conditionalPanel(condition = "input.tr",
                                 sliderInput("trFilter", h5("Filter Temperature Range value"),
                                             min = 0,
                                             max = 100,
                                             value = c(
                                               0,
                                               100
                                             )
                                 )
                ),
                
                conditionalPanel(condition = "input.ton",
                                 sliderInput("tonFilter", h5("Filter TurnOver number"),
                                             min = 0,
                                             max = 100,
                                             value = c(
                                               0,
                                               100
                                             )
                                 )
                ))
              ),
              box(
                title = "Parameter Table", width = 8, height = '80vh',
                div(style='height:60vh; overflow-y: scroll',
                DTOutput("distParameterTable"), height = '60vh')
              )
      ),
      
      # Information Avaible
      tabItem(tabName = "info",
              h1("Information Available")
      ),
      
      # Parameters Found show with an Histogram
      tabItem(tabName = "histogram",
              box(title = "Parameters Found", width = '100vh',
                  height = '70vh',
                  plotlyOutput("histogram")
              )
      ),
      
      # Distribution
      tabItem(tabName = "distribution",
              h1("Distribution")
      ),
      
      # Correlation
      tabItem(tabName = "correlation",
              h1("Correlation")
      ),
      
      # Acknowledgments
      tabItem(tabName = "acknowledgments",
              h1("Acknowledgments")
      ),
      
      # Frequently asked question
      tabItem(tabName = "faq",
              h1("Frequently asked question")
      )
    )
  )
)