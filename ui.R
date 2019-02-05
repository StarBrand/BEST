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

dashboardPage(skin = "green",
  dashboardHeader(title = "Title"),
  dashboardSidebar(
    sidebarMenu(
      id = "inTabset",
      useShinyjs(),
      menuItem("Home", tabName = "home", icon = icon("home"),
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
      menuItem("Visualization", tabName = "visualization", icon = icon("chart-area"), startExpanded = FALSE,
               menuSubItem("Parameters Found", tabName = "histogram"),
               menuSubItem("Distribution", tabName = "distribution"),
               menuSubItem("Correlation", tabName = "correlation")
               ),
      menuItem("Acknowledgments", tabName = "acknowledgments", icon = icon("award")),
      menuItem("FAQ", tabName = "faq", icon = icon("question"))
      
    )
  ),
  dashboardBody(
    useShinyjs(),
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
              )
      ),
      
      
      # Help
      tabItem(tabName = "help",
              h1("Tutorial")
      ),
      
      # Enzyme
      tabItem(tabName = "enzyme",
              box( title = "Select Enzyme", width = 4,
                   h3("Chooce an input"),
                   helpText("The ... app allows multiple type of",
                            "inputs. Such as enter the ec number, ",
                            "write the name of the enzyme or ",
                            "upload a .txt file with UniProt code",
                            "of your enzymes of interest"),
                   textInput("ec_number", "Enter EC Number"),
                   actionButton("ecNumber", "Enter", class = "btn-primary")
              )
      ),
      
      # Protein Table
      tabItem(tabName = "proteinTable",
                box(title = "Select Parameters", width = 3, height = '80vh',
                    div(style='height:70vh; overflow-y: scroll',
                    h4('Parameters to be looked up:'),
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
                    actionButton("parameters", "Search for parameters", class = "btn-primary"),
                    tags$br(),
                    helpText("Here you can obtain numerical parameters",
                             "of the enzymes you want to."))
                ),
                box(
                  title = "Protein Table", width = 9,
                  tags$br(),
                  materialSwitch("allProteins", "Serch for all the list", right = TRUE, value = TRUE),
                  tags$br(),
                  div(style='height:60vh; overflow-y: scroll',
                  DTOutput("distProteinTable", height = '60vh'))
                )
      ),
      
      # FASTA sequence
      tabItem(tabName = "fasta",
              h1("FASTA")
      ),
      
      # PDB
      tabItem(tabName = "pdb",
              h1("PDB")
      ),
      
      # Parameter Table
      tabItem(tabName = "parameterTable",
              box(
                title = "Filters", width = 3, height = '80vh',
                div(style='height:70vh; overflow-y: scroll',
                conditionalPanel(condition = "input.mw",
                                 sliderInput("mwFilter", h4("Filter Molecular Weight"),
                                             min = 0,
                                             max = 100,
                                             value = c(
                                               0,
                                               100
                                             )
                                 )
                ),
                
                conditionalPanel(condition = "input.ic50",
                                 sliderInput("ic50Filter", h4("Filter IC50 Value"),
                                             min = 0,
                                             max = 100,
                                             value = c(
                                               0,
                                               100
                                             )
                                 )
                ),
                
                conditionalPanel(condition = "input.kc",
                                 sliderInput("kcFilter", h4("Filter Kcat/Km value"),
                                             min = 0,
                                             max = 100,
                                             value = c(
                                               0,
                                               100
                                             )
                                 )
                ),
                
                conditionalPanel(condition = "input.ki",
                                 sliderInput("kiFilter", h4("Filter Ki value"),
                                             min = 0,
                                             max = 100,
                                             value = c(
                                               0,
                                               100
                                             )
                                 )
                ),
                
                conditionalPanel(condition = "input.km",
                                 sliderInput("kmFilter", h4("Filter Km value"),
                                             min = 0,
                                             max = 100,
                                             value = c(
                                               0,
                                               100
                                             )
                                 )
                ),
                
                conditionalPanel(condition = "input.pho",
                                 sliderInput("phoFilter", h4("Filter pH Optimum value"),
                                             min = 0,
                                             max = 100,
                                             value = c(
                                               0,
                                               100
                                             )
                                 )
                ),
                
                conditionalPanel(condition = "input.phr",
                                 sliderInput("phrFilter", h4("Filter pH Range value"),
                                             min = 0,
                                             max = 100,
                                             value = c(
                                               0,
                                               100
                                             )
                                 )
                ),
                
                conditionalPanel(condition = "input.pi",
                                 sliderInput("piFilter", h4("Filter pI value"),
                                             min = 0,
                                             max = 100,
                                             value = c(
                                               0,
                                               100
                                             )
                                 )
                ),
                
                conditionalPanel(condition = "input.sa",
                                 sliderInput("saFilter", h4("Filter Specific Activity"),
                                             min = 0,
                                             max = 100,
                                             value = c(
                                               0,
                                               100
                                             )
                                 )
                ),
                
                conditionalPanel(condition = "input.to",
                                 sliderInput("toFilter", h4("Filter Temperature Optimum value"),
                                             min = 0,
                                             max = 100,
                                             value = c(
                                               0,
                                               100
                                             )
                                 )
                ),
                
                conditionalPanel(condition = "input.tr",
                                 sliderInput("trFilter", h4("Filter Temperature Range value"),
                                             min = 0,
                                             max = 100,
                                             value = c(
                                               0,
                                               100
                                             )
                                 )
                ),
                
                conditionalPanel(condition = "input.ton",
                                 sliderInput("tonFilter", h4("Filter TurnOver number"),
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
                title = "Parameter Table", width = 9, height = '80vh',
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
              h1("Parameters Found")
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