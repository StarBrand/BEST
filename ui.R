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
                                 tags$br(),
                                 textInput(inputId = "mail",
                                           label = "Mail"),
                                 textInput(inputId = "pass",
                                           label = "password"),
                                 actionButton(inputId = "logIn",
                                              label = "log in",
                                              class = "btn-primary")
                                 ),
                               mainPanel(
                                 fluidRow(
                                   column(12,
                                          h1("Welcome")
                                          )
                                   ),
                                   
                                 fluidRow(
                                   column(4,
                                          
                                          h5("Brenda User"),
                                          helpText("To start using the ... app",
                                                   "you need a valid Brenda User,",
                                                   "if you don't, please, visit:"),
                                          tags$a("Brenda sign up", href="https://www.brenda-enzymes.org/login.php")
                                   ),
                                   
                                   column(4,
                                          
                                          h5("Protein Search"),
                                          helpText("In this app you can",
                                                   "access all the Brenda",
                                                   "potencial in a more",
                                                   "intuitive way.")
                                   ),
                                   
                                   column(4,
                                          h5("Analysis and visualization of data"),
                                          helpText("...")
                                          
                                   )
                                   
                                 )
                                 )
                               )
                          
                             ),
                             tabPanel(title = "Protein Search",
                                      value = "proteinSearch",
                                      sidebarLayout(
                                        sidebarPanel(
                                          
                                          conditionalPanel(condition = "input.protein == 'enzyme'",
                                                           tags$head(tags$style(
                                                             type = 'text/css',
                                                             'form.well { max-height: 80vh; overflow-y: auto; }'
                                                           )),
                                                           h3("Chooce an input"),
                                                           helpText("The ... app allows multiple type of",
                                                                    "inputs. Such as enter the ec number, ",
                                                                    "write the name of the enzyme or ",
                                                                    "upload a .txt file with UniProt code",
                                                                    "of your enzymes of interest"),
                                                           textInput("ec_number", "Enter EC Number"),
                                                           actionButton("ecNumber", "Enter", class = "btn-primary"),
                                                           tags$br(),
                                                           helpText("Select this input if you",
                                                                    "know the EC number of your enzyme",
                                                                    "of interest, you can check it in:"),
                                                           tags$a("EC Number Info", href="https://qmul.ac.uk/sbcs/iubmb"),
                                                           tags$hr(),
                                                           textInput("enzyme_name", "Enzyme name"),
                                                           actionButton("enzymeName", "Enter", class = "btn-primary"),
                                                           tags$br(),
                                                           helpText("Select this input if you",
                                                                    "just know the name of your",
                                                                    "enzyme"),
                                                           tags$br(),
                                                           fileInput("uniprot", "UniProt File"),
                                                           helpText("If you have the UniProt code(s)",
                                                                    "of your enzyme(s) of interest,",
                                                                    "upload a text file with them")
                                                           ),
                                          
                                          conditionalPanel(condition = "input.protein == 'proteinTable'",
                                                           tags$head(tags$style(
                                                             type = 'text/css',
                                                             'form.well { max-height: 80vh; overflow-y: auto; }'
                                                           )),
                                                           h4('Parameters to be looked up:'),
                                                           div(style="display: inline-block;vertical-align:top;width: 100px;", checkboxInput(inputId = 'mw', label = 'Molecular Weight')),
                                                           div(style="display: inline-block;vertical-align:top;width: 10px;", HTML("<br>")),
                                                           div(style="display: inline-block;vertical-align:top;width: 100px;", checkboxInput(inputId = 'ic50', label = 'IC50 Value')),
                                                           div(style="display: inline-block;vertical-align:top;width: 100px;", checkboxInput(inputId = 'kc', label = 'Kcat/Km')),
                                                           div(style="display: inline-block;vertical-align:top;width: 10px;", HTML("<br>")),
                                                           div(style="display: inline-block;vertical-align:top;width: 100px;", checkboxInput(inputId = 'ki', label = 'Ki Value')),
                                                           div(style="display: inline-block;vertical-align:top;width: 100px;", checkboxInput(inputId = 'km', label = 'Km Value')),
                                                           div(style="display: inline-block;vertical-align:top;width: 10px;", HTML("<br>")),
                                                           div(style="display: inline-block;vertical-align:top;width: 100px;", checkboxInput(inputId = 'pho', label = 'pH Optimum')),
                                                           div(style="display: inline-block;vertical-align:top;width: 100px;", checkboxInput(inputId = 'phr', label = 'pH Range')),
                                                           div(style="display: inline-block;vertical-align:top;width: 10px;", HTML("<br>")),
                                                           div(style="display: inline-block;vertical-align:top;width: 100px;", checkboxInput(inputId = 'pi', label = 'pI Value')),
                                                           div(style="display: inline-block;vertical-align:top;width: 100px;", checkboxInput(inputId = 'sa', label = 'Specific Activity')),
                                                           div(style="display: inline-block;vertical-align:top;width: 10px;", HTML("<br>")),
                                                           div(style="display: inline-block;vertical-align:top;width: 100px;", checkboxInput(inputId = 'to', label = 'Temperature Optimum')),
                                                           div(style="display: inline-block;vertical-align:top;width: 100px;", checkboxInput(inputId = 'tr', label = 'Temperature Range')),
                                                           div(style="display: inline-block;vertical-align:top;width: 10px;", HTML("<br>")),
                                                           div(style="display: inline-block;vertical-align:top;width: 100px;", checkboxInput(inputId = 'ton', label = 'TurnOver Number')),
                                                           tags$hr(),
                                                           h4("Additional Options:"),
                                                           checkboxInput(inputId = 'up',
                                                                         label = 'Look up just for the proteins that has UniProt code'
                                                           ),
                                                           actionButton("parameters", "Search for parameters", class = "btn-primary"),
                                                           tags$br(),
                                                           helpText("Here you can obtain numerical parameters",
                                                                    "of the enzymes you want to.")),
                                          
                                          conditionalPanel(condition = "input.protein == 'parameterTable'",
                                                           tags$head(tags$style(
                                                             type = 'text/css',
                                                             'form.well { max-height: 80vh; overflow-y: auto; }'
                                                           )),
                                                           h3("What's next?"),
                                                           h4("Filters"),
                                                           helpText("Move the sliders to filter ",
                                                                    "the table prior further analysis ",
                                                                    "if you want to maintain all the ",
                                                                    "obtained data, leave the sliders ",
                                                                    "as they are.")
                                                           ),
                                          conditionalPanel(condition = "input.protein == 'parameterTable' & input.mw",
                                                           sliderInput("mwFilter", h4("Filter Molecular Weight"),
                                                                       min = 0,
                                                                       max = 100,
                                                                       value = c(
                                                                         0,
                                                                         100
                                                                       )
                                                           )
                                          ),
                                          
                                          conditionalPanel(condition = "input.protein == 'parameterTable' & input.ic50",
                                                           sliderInput("ic50Filter", h4("Filter IC50 Value"),
                                                                       min = 0,
                                                                       max = 100,
                                                                       value = c(
                                                                         0,
                                                                         100
                                                                       )
                                                           )
                                          ),
                                          
                                          conditionalPanel(condition = "input.protein == 'parameterTable' & input.kc",
                                                           sliderInput("kcFilter", h4("Filter Kcat/Km value"),
                                                                       min = 0,
                                                                       max = 100,
                                                                       value = c(
                                                                         0,
                                                                         100
                                                                       )
                                                           )
                                          ),
                                          
                                          conditionalPanel(condition = "input.protein == 'parameterTable' & input.ki",
                                                           sliderInput("kiFilter", h4("Filter Ki value"),
                                                                       min = 0,
                                                                       max = 100,
                                                                       value = c(
                                                                         0,
                                                                         100
                                                                       )
                                                           )
                                          ),
                                          
                                          conditionalPanel(condition = "input.protein == 'parameterTable' & input.km",
                                                           sliderInput("kmFilter", h4("Filter Km value"),
                                                                       min = 0,
                                                                       max = 100,
                                                                       value = c(
                                                                         0,
                                                                         100
                                                                         )
                                                                       )
                                                           ),
                                          
                                          conditionalPanel(condition = "input.protein == 'parameterTable' & input.pho",
                                                           sliderInput("phoFilter", h4("Filter pH Optimum value"),
                                                                       min = 0,
                                                                       max = 100,
                                                                       value = c(
                                                                         0,
                                                                         100
                                                                       )
                                                           )
                                          ),
                                          
                                          conditionalPanel(condition = "input.protein == 'parameterTable' & input.phr",
                                                           sliderInput("phrFilter", h4("Filter pH Range value"),
                                                                       min = 0,
                                                                       max = 100,
                                                                       value = c(
                                                                         0,
                                                                         100
                                                                       )
                                                           )
                                          ),
                                          
                                          conditionalPanel(condition = "input.protein == 'parameterTable' & input.pi",
                                                           sliderInput("piFilter", h4("Filter pI value"),
                                                                       min = 0,
                                                                       max = 100,
                                                                       value = c(
                                                                         0,
                                                                         100
                                                                       )
                                                           )
                                          ),
                                          
                                          conditionalPanel(condition = "input.protein == 'parameterTable' & input.sa",
                                                           sliderInput("saFilter", h4("Filter Specific Activity"),
                                                                       min = 0,
                                                                       max = 100,
                                                                       value = c(
                                                                         0,
                                                                         100
                                                                       )
                                                           )
                                          ),
                                          
                                          conditionalPanel(condition = "input.protein == 'parameterTable' & input.to",
                                                           sliderInput("toFilter", h4("Filter Temperature Optimum value"),
                                                                       min = 0,
                                                                       max = 100,
                                                                       value = c(
                                                                         0,
                                                                         100
                                                                       )
                                                           )
                                          ),
                                          
                                          conditionalPanel(condition = "input.protein == 'parameterTable' & input.tr",
                                                           sliderInput("trFilter", h4("Filter Temperature Range value"),
                                                                       min = 0,
                                                                       max = 100,
                                                                       value = c(
                                                                         0,
                                                                         100
                                                                       )
                                                           )
                                          ),
                                          
                                          conditionalPanel(condition = "input.protein == 'parameterTable' & input.ton",
                                                           sliderInput("tonFilter", h4("Filter TurnOver number"),
                                                                       min = 0,
                                                                       max = 100,
                                                                       value = c(
                                                                         0,
                                                                         100
                                                                       )
                                                           )
                                          ),
                                          
                                          conditionalPanel(condition = "input.protein == 'uniprot'",
                                                           h5("Options:"),
                                                           p("Select the proteins you want to download"),
                                                           p("or"),
                                                           checkboxInput("no_filter", "Download all the list"),
                                                           tags$hr(),
                                                           textOutput("noSequenceProtein"),
                                                           p("What do you want to do about it?"),
                                                           tags$br(),
                                                           p("Search for other database:"),
                                                           checkboxInput("search", "Some source"),
                                                           checkboxInput("no_search", "Leave it blank"))
                                          ),
                                        mainPanel(
                                          tabsetPanel(id = "protein",
                                                      tabPanel(title = "Enzyme",
                                                               value = "enzyme"),
                                                      tabPanel(title = "Protein Table",
                                                               value = "proteinTable",
                                                               tags$br(),
                                                               tags$br(),
                                                               downloadButton("proteinTable", "Download Table", class = "btn-primary"),
                                                               actionButton("sequence", "Get Sequences", class = "btn-secondary"),
                                                               tags$br(),
                                                               textOutput("timeSearch"),
                                                               textOutput("longTimeSearch"),
                                                               tags$br(),
                                                               tags$br(),
                                                               DTOutput("distProteinTable")
                                                               ),
                                                      tabPanel(title = "UniProt",
                                                               value = "uniprot",
                                                               tags$br(),
                                                               downloadButton("fasta", "Download FASTA sequence", class = "btn-secondary"),
                                                               tags$br(),
                                                               tags$br(),
                                                               DTOutput("distProteinSequence")),
                                                      tabPanel(title = "Parameter Table",
                                                               value = "parameterTable",
                                                               tags$br(),
                                                               downloadButton("parameterTable", "Download Table", class = "btn-primary"),
                                                               tags$br(),
                                                               tags$br(),
                                                               DTOutput("distParameterTable"))
                                                      )
                                        )
                                        )
                             ),
                             
                             tabPanel(title = "Visualization",
                                      value = "visualization",
                                      sidebarLayout(
                                        sidebarPanel(
                                          
                                          conditionalPanel(condition = "input.visual == 'histogram'",
                                                           checkboxInput("new_table", "Work with a new table")
                                                           ),
                                          
                                          conditionalPanel(condition = "input.visual == 'histogram' && input.new_table",
                                                           fileInput("uploadTable", "Upload a new table"),
                                                           helpText("If you want to work with a new table, ",
                                                                    "or, if you already download a table with ",
                                                                    "parameters of enzymes in this site ",
                                                                    "you can upload the table in .csv format ",
                                                                    "here. Make sure to check the format of your table",
                                                                    "to match the format of the tables of this side, ",
                                                                    "to avoid posibles error.")
                                                           ),
                                          
                                          conditionalPanel(condition = "input.visual == 'histogram'",
                                                           h4("What do you want to do now?")
                                                           ),
                                          
                                          conditionalPanel(condition = "input.visual == 'correlational'",
                                                           h3("3")
                                                           ),
                                          
                                          conditionalPanel(condition = "input.visual == 'plot'",
                                                           h3("4"))
                                        ),
                                        mainPanel(
                                          tabsetPanel(id = "visual",
                                                      tabPanel(title = "Histogram",
                                                               value = "histogram",
                                                               plotOutput("histogram")),
                                                      tabPanel(title = "Correlational",
                                                               value = "correlational"),
                                                      tabPanel(title = "Plot",
                                                               value = "plot")
                                          )
                                          )
                                        )
                                      ),

                             tabPanel(title = "Tutorial",
                                      value = "help",
                                      h1("How to use ... app"),
                                      h2("Get started:"),
                                      p("First you need a Brenda user",
                                        "valid account, if you don't, please",
                                        "enter this site",
                                        uiOutput("userLink")),
                                      h2("Searching Proteins"),
                                      p("In the first panel:",
                                        ", you have to enter your Brenda account.",
                                        "Then, you are going to be in ",
                                        "the Enzyme panel. Here",
                                        "you have to choose the way you want to",
                                        "enter the enzyme (or enzymes) you are looking ",
                                        "for. We give you 3 different options:"),
                                      h3("Enter the EC Number"),
                                      p("The EC Number is standarized code that defines",
                                          " an enzyme according which reaction catalize")
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