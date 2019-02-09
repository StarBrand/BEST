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
      menuItem("Suggestion", tabName = "suggestion", icon = icon("eye")),
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
                                                       sep = "\t", header = TRUE,
                                                       col.names = "dig1")$dig1
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
                   tags$br(),
                   tags$hr(),
                   selectInput("subclass", "Know the subclass of the enzyme type?",
                                  choices = list("Oxidoreductases" = "1",
                                                 "Transferases" = "2",
                                                 "Hydrolases" = "3",
                                                 "Lyases" = "4",
                                                 "Isomerases" = "5",
                                                 "Ligases" = "6")),
                   selectInput("subsubclass", "What specific reaction it does?",
                               choices = NULL),
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
                    materialSwitch("allParameters", "Select all", right = TRUE, value = FALSE, status = "primary"),
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
                  div(style="display: inline-block;vertical-align:top;width: 0.5vh;", HTML("<br>")),
                  div(style="display: inline-block;",downloadButton("downloadProtein", "Download table(.csv)", class = "btn-primary")),
                  div(style="display: inline-block;",actionButton("toFasta", "Get sequence", class = "btn-success")),
                  div(style="display: inline-block;",actionButton("toPDB", "Get PDB", class = "btn-success")),
                  div(style="display: inline-block;float:right;",actionButton("toEnzyme", "Go back and search another Enzyme", class = "btn-warning")),
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
                      materialSwitch("no_filter", "Download all the list", right = TRUE, value = TRUE, status = "primary"),
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
                  div(style="display: inline-block;",downloadButton("downloadFASTA", "Download sequence in FASTA format", class = "btn-success")),
                  div(style="display: inline-block;float:right;",actionButton("toProtein1", "Back to the protein table", class = "btn-warning")),
                  tags$br(),
                  tags$br(),
                  div(style='height:70vh; overflow-y: scroll',
                  DTOutput("fastaTable"))
              )
      
      ),
      
      # PDB
      tabItem(tabName = "pdb",
              box(title = "PDB Table", width = 12,
                  tags$br(),
                  div(style="display: inline-block;",downloadButton("downloadPDB", "Download PDB Table", class = "btn-success")),
                  div(style="display: inline-block;float:right;",actionButton("toProtein2", "Back to the protein table", class = "btn-warning")),
                  tags$br(),
                  tags$br(),
                  div(style='height:70vh; overflow-y: scroll',
                  DTOutput("pdbTable"))
              )
      ),
      
      # Parameter Table
      tabItem(tabName = "parameterTable",
              box(
                width = 4, height = '80vh',
                actionButton("filter", "Filter", class = "btn-primary"),
                div(style='height:70vh; overflow-y: scroll',
                conditionalPanel(condition = "input.mw",
                                 div(style="display: inline-block;",checkboxInput(inputId = 'mw2', label = "")),
                                 div(style="display: inline-block;width:40vh;",
                                 sliderInput("mwFilter", h5("Filter Molecular Weight"),
                                             min = 0,
                                             max = 100,
                                             value = c(
                                               0,
                                               100
                                             )
                                 ))
                ),
                
                conditionalPanel(condition = "input.ic50",
                                 div(style="display: inline-block;",checkboxInput(inputId = 'ic502', label = "")),
                                 div(style="display: inline-block;width:40vh;",
                                 sliderInput("ic50Filter", h5("Filter IC50 Value"),
                                             min = 0,
                                             max = 100,
                                             value = c(
                                               0,
                                               100
                                             )
                                 ))
                ),
                
                conditionalPanel(condition = "input.kc",
                                 div(style="display: inline-block;",checkboxInput(inputId = 'kc2', label = "")),
                                 div(style="display: inline-block;width:40vh;",
                                 sliderInput("kcFilter", h5("Filter Kcat/Km value"),
                                             min = 0,
                                             max = 100,
                                             value = c(
                                               0,
                                               100
                                             )
                                 ))
                ),
                
                conditionalPanel(condition = "input.ki",
                                 div(style="display: inline-block;",checkboxInput(inputId = 'ki2', label = "")),
                                 div(style="display: inline-block;width:40vh;",
                                 sliderInput("kiFilter", h5("Filter Ki value"),
                                             min = 0,
                                             max = 100,
                                             value = c(
                                               0,
                                               100
                                             )
                                 ))
                ),
                
                conditionalPanel(condition = "input.km",
                                 div(style="display: inline-block;",checkboxInput(inputId = 'km2', label = "")),
                                 div(style="display: inline-block;width:40vh;",
                                 sliderInput("kmFilter", h5("Filter Km value"),
                                             min = 0,
                                             max = 100,
                                             value = c(
                                               0,
                                               100
                                             )
                                 ))
                ),
                
                conditionalPanel(condition = "input.pho",
                                 div(style="display: inline-block;",checkboxInput(inputId = 'pho2', label = "")),
                                 div(style="display: inline-block;width:40vh;",
                                 sliderInput("phoFilter", h5("Filter pH Optimum value"),
                                             min = 0,
                                             max = 100,
                                             value = c(
                                               0,
                                               100
                                             )
                                 ))
                ),
                
                conditionalPanel(condition = "input.phr",
                                 div(style="display: inline-block;",checkboxInput(inputId = 'phr2', label = "")),
                                 div(style="display: inline-block;width:40vh;",
                                 sliderInput("phrFilter", h5("Filter pH Range value"),
                                             min = 0,
                                             max = 100,
                                             value = c(
                                               0,
                                               100
                                             )
                                 ))
                ),
                
                conditionalPanel(condition = "input.pi",
                                 div(style="display: inline-block;",checkboxInput(inputId = 'pi2', label = "")),
                                 div(style="display: inline-block;width:40vh;",
                                 sliderInput("piFilter", h5("Filter pI value"),
                                             min = 0,
                                             max = 100,
                                             value = c(
                                               0,
                                               100
                                             )
                                 ))
                ),
                
                conditionalPanel(condition = "input.sa",
                                 div(style="display: inline-block;",checkboxInput(inputId = 'sa2', label = "")),
                                 div(style="display: inline-block;width:40vh;",
                                 sliderInput("saFilter", h5("Filter Specific Activity"),
                                             min = 0,
                                             max = 100,
                                             value = c(
                                               0,
                                               100
                                             )
                                 ))
                ),
                
                conditionalPanel(condition = "input.to",
                                 div(style="display: inline-block;",checkboxInput(inputId = 'to2', label = "")),
                                 div(style="display: inline-block;width:40vh;",
                                 sliderInput("toFilter", h5("Filter Temperature Optimum value"),
                                             min = 0,
                                             max = 100,
                                             value = c(
                                               0,
                                               100
                                             )
                                 ))
                ),
                
                conditionalPanel(condition = "input.tr",
                                 div(style="display: inline-block;",checkboxInput(inputId = 'tr2', label = "")),
                                 div(style="display: inline-block;width:40vh;",
                                 sliderInput("trFilter", h5("Filter Temperature Range value"),
                                             min = 0,
                                             max = 100,
                                             value = c(
                                               0,
                                               100
                                             )
                                 ))
                ),
                
                conditionalPanel(condition = "input.ton",
                                 div(style="display: inline-block;",checkboxInput(inputId = 'ton2', label = "")),
                                 div(style="display: inline-block;width:40vh;",
                                 sliderInput("tonFilter", h5("Filter TurnOver number"),
                                             min = 0,
                                             max = 100,
                                             value = c(
                                               0,
                                               100
                                             )
                                 ))
                ))
              ),
              box(
                div(style="display: inline-block;",actionButton("visualize", "Visualize", class = "btn-success")),
                div(style="display: inline-block;float:right;",actionButton("toProtein3", "Back to the protein table", class = "btn-warning")),
                tags$br(),
                tags$br(),
                title = "Parameter Table", width = 8, height = '80vh',
                div(style='height:60vh; overflow-y: scroll',
                DTOutput("distParameterTable"), height = '60vh')
              )
      ),
      
      # Information Avaible
      tabItem(tabName = "info",
              box(
                title = "Information", width = 4, height = '80vh',
                div(style='height:60vh; overflow-y: scroll',
                h4("Want more data?"),
                div(style="display: inline-block;",
                p("To look for protein: ")),
                div(style="display: inline-block;",
                actionLink("enzymeInput", "click here")),
                tags$br(),
                div(style="display: inline-block;",
                p("To look for numeric parameters: ")),
                div(style="display: inline-block;",
                actionLink("paramInput", "click here")),
                tags$br(),
                div(style="display: inline-block;",
                p("To look for sequence: ")),
                div(style="display: inline-block;",
                actionLink("fastaInput", "click here")),
                tags$br(),
                div(style="display: inline-block;",
                p("To look for PDB: ")),
                div(style="display: inline-block;",
                actionLink("pdbInput", "click here")),
                tags$br(),
                div(style="display: inline-block;",
                p("To filter the list by parameters: ")),
                div(style="display: inline-block;",
                actionLink("filterInput", "click here"))
                )
              ),
              box(
                title = "Available Information", width = 8, height = '80vh',
                div(style="display: inline-block;",actionButton("generateSummary", "Refresh", icon = icon("refresh"), class = "btn-primary")),
                div(style='height:60vh; overflow-y: scroll',
                tags$br(),
                tags$br(),
                DTOutput("distSummaryTable"), height = '70vh')
              )
      ),
      
      # Parameters Found show with an Histogram
      tabItem(tabName = "histogram",
              box(title = "Sidebar", width = 3,
                  actionButton("getDistribution", "Watch distribution")),
              box(title = "Parameters Found", width = 9,
                  height = '85vh',
                  div(plotlyOutput("histogram"), height = '70vh', width = '70vh')
              )
      ),
      
      # Distribution
      tabItem(tabName = "distribution",
              box(title = "Sidebar", width = 3),
              box(tile = "Distribution", width = 9,
                  height = '70vh',
                  div(
                  plotlyOutput("distributionOut"),
                  height = '70vh')
              )
      ),
      
      # Correlation
      tabItem(tabName = "correlation",
              h1("Correlation")
      ),
      
      # Suggestions
      tabItem(tabName = "suggestion",
              h1("Suggestion")
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