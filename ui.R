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
      helpText("You can always read",
               "the turorial section ",
               actionLink("help", "here")))
    )
  ),
  dashboardBody(
    useShinyjs(),
    useShinyalert(),
    tabItems(
      # Log in
      tabItem(tabName = "account",
              
              box( title = "Brenda User", width = 3, height = '80vh',
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
              box(width = 9, height = '60vh',
                  img(src = 'Referential.jpg',
                      height = '320vh',
                      style = "display: block; margin-left: auto; margin-right: auto;")
              ),
              box(width = 9, heigth = '30vh',
                  column(4, h5("Brenda User"),
                         helpText("To start using the ... app",
                                  "you need a valid Brenda User,",
                                  "if you don't, please, visit:",
                                  tags$a("Brenda: Registration", href="https://www.brenda-enzymes.org/register.php"))),
                  column(4, h5("Protein Search"),
                         helpText("In this app you can",
                                  "access all the Brenda",
                                  "potencial in a more",
                                  "intuitive way.")),
                  column(4, h5("Analysis and visualization of data"),
                         helpText("..."))
              )
      ),
      
      
      # Help
      tabItem(tabName = "help",
              box(width = 3, height = '90vh', 
                       tags$li(tags$a(href = "#1", "Get started"),
                               tags$ul(tags$li(tags$a(href = "#1_1", "Enzyme query"),
                                               tags$ul(tags$li(tags$a(href = "#1_1_1", "EC Number")),
                                                       tags$li(tags$a(href = "#1_1_2", "Enzyme name"))
                                                       ))
                                       )
                               )
                  ),
              box(width = 9, height = '90vh',
                  div(style='height:80vh; overflow-y: scroll',
                       div(id = "1", class = "section level1",
                           h1("Get started"),
                           p("To start looking in Brenda, we need your Brenda account (why?). ",
                             "If you do not have one, you can visit: ",
                             tags$a("Brenda Registration", href="https://www.brenda-enzymes.org/register.php"),
                             "Once you have your account, we can look for the enzymes you need ",
                             "and the numerical parameters you request.")),
                       div(id = "1_1", class = "section level2",
                           h2("Enzyme query"),
                           p("To get the enzymes you need, we offer three different kind of options. This inputs are in ",
                             "the ",
                             actionLink("enzymeHelp", "select enzyme"),
                             "section.")),
                       div(id = "1_1_1", class = "section level3",
                           h3("EC Number"),
                           p("Enzymes are classified by the ",
                             tags$a("IUBMB", href = "https://iubmb.org/"),
                             " according to the reaction they catalize. This corresponds to the",
                             "EC Number, which is a four number code, with the first one indicating the subclass ",
                             "such as oxidoreductases, transferases, hydrolases, lyases, isomerases, ligases and translocases. ",
                             "The second number indicates the functional group of reaction, the third indicates an specific ",
                             "type of molecule and the fourth the last",
                             "level and the reaction itself. More info: ",
                             tags$a("EC number", href = "https://www.qmul.ac.uk/sbcs/iubmb/"),
                             br(),
                             "If you know the EC number of your enzyme of interest, indicate it in the input",
                             "shown in the next image: "),
                           img(src = 'Tutorial(1).jpg',
                               width = '250px',
                               style = "display: block; margin-left: auto; margin-right: auto;")
                           ),
                      div(id = "1_1_2", class = "section level3",
                          h3("Enzyme name"),
                          p("Enzymes have a lot of names that differ depending on the gen codifiding it, the reaction, ",
                            "historical reasons and so on. Brenda groups them according to its ec number, however, they are associated",
                            "to the multiple names in which they are called in literature. This category is showed in the ",
                            tags$a("Brenda web page"),
                            "with the name 'Synonyms'."),
                          p("Due to the length of the list, that contains every synonym of every type of enzyme on the database, this names ",
                            "are separeted in subclasses. Then, if you know just a particular name of your enzyme, first you have to",
                            " know in which of this categories it is: "),
                          img(src = 'Tutorial(2).jpg',
                              width = '250px',
                              style = "display: block; margin-left: auto; margin-right: auto;"),
                          p("For the next step you have to select the functional group that is involved in the catalyzed ",
                            "reaction, such as: "),
                          img(src = 'Tutorial(3).jpg',
                              width = '250px',
                              style = "display: block; margin-left: auto; margin-right: auto;"),
                          p("In the follow space, you can type the name of the enzyme you know. You can search for the list ",
                            "below, remembering that it is a long list. This selector autocompletes your input and shows the posible ",
                            "names that are registered in the Brenda database, select the one that fits most with the name you know."),
                          img(src = 'Tutorial(4).jpg',
                              width = '250px',
                              style = "display: block; margin-left: auto; margin-right: auto;"),
                          p("After you press the search button, a short list of posible type of enzyme is going to be shown (it can be ",
                            "just one). The selector shown the ec number and the Brenda recommended name. Select the one you are looking ",
                            "for, press 'go', and go on with the numerical parameter selector."),
                          img(src = 'Tutorial(5).jpg',
                              width = '250px',
                              style = "display: block; margin-left: auto; margin-right: auto;")
                          )
                      
                      ))
      ),
      
      # Enzyme
      tabItem(tabName = "enzyme",
              box( title = "Select Enzyme", width = 4,
                   div(style='height:80vh; overflow-y: scroll',
                   h3("Chooce an input"),
                   helpText("The ... app allows multiple type of",
                            "inputs. Such as enter the ec number, ",
                            "write the name of the enzyme or ",
                            "upload a .txt file with UniProt code",
                            "of your enzymes of interest"),
                   tags$br(),
                   h4("Enter EC Number"),
                   helpText("Select this input if you ",
                            "know the EC number of your enzyme ",
                            "of interest, you can check it in: ",
                            tags$a("EC Number Info", href="https://qmul.ac.uk/sbcs/iubmb")),
                   tags$br(),
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
                   h4("Enzyme name"), 
                   helpText("Select this input if you ",
                            "just know the name of your ",
                            "enzyme"),
                   helpText("But first, you have to know in which",
                            "of this subclasses your enzyme is"),
                   selectInput("subclass", "Know the subclass of the enzyme type?",
                                  choices = list("Oxidoreductases" = "1",
                                                 "Transferases" = "2",
                                                 "Hydrolases" = "3",
                                                 "Lyases" = "4",
                                                 "Isomerases" = "5",
                                                 "Ligases" = "6")),
                   helpText("Select one type of reaction, considering the functional group ",
                            "involved in the catalyzed reaction"),
                   selectInput("subsubclass", "What specific reaction it does?",
                               choices = NULL),
                   helpText("Here, just type the name of your enzyme of interest ",
                            "and select the one that fits most"),
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
                   helpText("If you have the UniProt code(s) ",
                            "of your enzyme(s) of interest, ",
                            "upload a text file with them"),
                   fileInput("uniprot_file", "Upload your uniprot file"))
              )
      ),
      
      # Protein Table
      tabItem(tabName = "proteinTable",
                box(title = "Select Parameters", width = 3, height = '90vh',
                    div(style='height:80vh; overflow-y: scroll',
                    h4('Parameters to be look up'),
                    materialSwitch("allParameters", "Select all", right = TRUE, value = FALSE, status = "primary"),
                    div(style="display: inline-block;vertical-align:top;width: 40vh;",
                    checkboxGroupInput("attributes", "",
                                       choices = list("Molecular Weight" = "mw",
                                                      "IC50" = "ic50",
                                                      "Kcat/Km" = "kc",
                                                      "Ki" = "ki",
                                                      "Km" = "km",
                                                      "pH Optimum" = "pho",
                                                      "pH Range" = "phr",
                                                      "pI" = "pi",
                                                      "Specific Activity" = "sa",
                                                      "Temperature Optimum" = "to",
                                                      "Temperature Range" = "tr",
                                                      "Turnover Number" = "ton")
                                                      )),
                    tags$hr(),
                    h4("Additional Options"),
                    checkboxInput(inputId = 'up',
                                  label = 'Look up just for the proteins that has UniProt code'
                                  ),
                    actionButton("parameters", "Search for parameters", class = "btn-success"),
                    tags$br(),
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
                  div(style="display: inline-block;", checkboxInput("showComments1", "Show the commentary column", value = FALSE)),
                  div(style="display: inline-block;vertical-align:top;width: 25px;", HTML("<br>")),
                  div(style="display: inline-block;", checkboxInput("showLiterature1", "Show the Literature column (Pubmed ID)", value = FALSE)),
                  tags$br(),
                  div(style='height:60vh; overflow-y: scroll',
                  DTOutput("distProteinTable", height = '60vh'))
                )
      ),
      
      # FASTA sequence
      tabItem(tabName = "fasta",
              box(title = "Options", width = 3,
                  div(style='height:80vh; overflow-y: scroll',
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
                  div(style="display: inline-block;",downloadButton("downloadFASTA", "Download sequence in FASTA format", class = "btn-primary")),
                  div(style="display: inline-block;float:right;",actionButton("toProtein1", "Back to the protein table", class = "btn-warning")),
                  tags$br(),
                  tags$br(),
                  div(style='height:60vh; overflow-y: scroll',
                  DTOutput("fastaTable"))
              )
      
      ),
      
      # PDB
      tabItem(tabName = "pdb",
              box(title = "PDB Table", width = 12,
                  tags$br(),
                  div(style="display: inline-block;",downloadButton("downloadPDB", "Download PDB Table", class = "btn-primary")),
                  div(style="display: inline-block;float:right;",actionButton("toProtein2", "Back to the protein table", class = "btn-warning")),
                  tags$br(),
                  tags$br(),
                  div(style='height:60vh; overflow-y: scroll',
                  DTOutput("pdbTable"))
              )
      ),
      
      # Parameter Table
      tabItem(tabName = "parameterTable",
              box(
                width = 4, height = '90vh',
                actionButton("filter", "Filter", class = "btn-primary"),
                helpText("Move the sliders to filter ",
                         "the table prior further analysis ",
                         "if you want to maintain all the ",
                         "obtained data, leave the sliders ",
                         "as they are."),
                div(style='height:65vh; overflow-y: scroll',
                    conditionalPanel(condition = "input.attributes.includes('mw')",
                                     div(style="display: inline-block;",checkboxInput(inputId = 'mw2', label = "")),
                                     div(style="display: inline-block;width:40vh;",
                                         sliderInput("mwFilter", h5("Filter Molecular Weight"),
                                                     min = 0, max = 100, value = c(0,100))
                                         )),
                    conditionalPanel(condition = "input.attributes.includes('ic50')",
                                     div(style="display: inline-block;",checkboxInput(inputId = 'ic502', label = "")),
                                     div(style="display: inline-block;width:40vh;",
                                         sliderInput("ic50Filter", h5("Filter IC50 Value"),
                                                     min = 0, max = 100, value = c(0,100))
                                         )),
                    conditionalPanel(condition = "input.attributes.includes('kc')",
                                     div(style="display: inline-block;",checkboxInput(inputId = 'kc2', label = "")),
                                     div(style="display: inline-block;width:40vh;",
                                         sliderInput("kcFilter", h5("Filter Kcat/Km value"),
                                                     min = 0, max = 100, value = c(0,100))
                                         )),
                    conditionalPanel(condition = "input.attributes.includes('ki')",
                                     div(style="display: inline-block;",checkboxInput(inputId = 'ki2', label = "")),
                                     div(style="display: inline-block;width:40vh;",
                                         sliderInput("kiFilter", h5("Filter Ki value"),
                                                     min = 0, max = 100, value = c(0,100))
                                         )),
                    conditionalPanel(condition = "input.attributes.includes('km')",
                                     div(style="display: inline-block;",checkboxInput(inputId = 'km2', label = "")),
                                     div(style="display: inline-block;width:40vh;",
                                         sliderInput("kmFilter", h5("Filter Km value"),
                                                     min = 0, max = 100, value = c(0,100))
                                         )),
                    conditionalPanel(condition = "input.attributes.includes('pho')",
                                     div(style="display: inline-block;",checkboxInput(inputId = 'pho2', label = "")),
                                     div(style="display: inline-block;width:40vh;",
                                         sliderInput("phoFilter", h5("Filter pH Optimum value"),
                                                     min = 0, max = 100, value = c(0,100))
                                         )),
                    conditionalPanel(condition = "input.attributes.includes('phr')",
                                     div(style="display: inline-block;",checkboxInput(inputId = 'phr2', label = "")),
                                     div(style="display: inline-block;width:40vh;",
                                         sliderInput("phrFilter", h5("Filter pH Range value"),
                                                     min = 0, max = 100, value = c(0,100))
                                         )),
                    conditionalPanel(condition = "input.attributes.includes('pi')",
                                     div(style="display: inline-block;",checkboxInput(inputId = 'pi2', label = "")),
                                     div(style="display: inline-block;width:40vh;",
                                         sliderInput("piFilter", h5("Filter pI value"),
                                                     min = 0, max = 100, value = c(0,100))
                                         )),
                    conditionalPanel(condition = "input.attributes.includes('sa')",
                                     div(style="display: inline-block;",checkboxInput(inputId = 'sa2', label = "")),
                                     div(style="display: inline-block;width:40vh;",
                                         sliderInput("saFilter", h5("Filter Specific Activity"),
                                                     min = 0, max = 100, value = c(0,100))
                                         )),
                    conditionalPanel(condition = "input.attributes.includes('to')",
                                     div(style="display: inline-block;",checkboxInput(inputId = 'to2', label = "")),
                                     div(style="display: inline-block;width:40vh;",
                                         sliderInput("toFilter", h5("Filter Temperature Optimum value"),
                                                     min = 0, max = 100, value = c(0,100))
                                         )),
                    conditionalPanel(condition = "input.attributes.includes('tr')",
                                     div(style="display: inline-block;",checkboxInput(inputId = 'tr2', label = "")),
                                     div(style="display: inline-block;width:40vh;",
                                         sliderInput("trFilter", h5("Filter Temperature Range value"),
                                                     min = 0, max = 100, value = c(0,100))
                                         )),
                    conditionalPanel(condition = "input.attributes.includes('ton')",
                                     div(style="display: inline-block;",checkboxInput(inputId = 'ton2', label = "")),
                                     div(style="display: inline-block;width:40vh;",
                                         sliderInput("tonFilter", h5("Filter TurnOver number"),
                                                     min = 0, max = 100, value = c(0,100))
                                         ))
                    )),
              box(
                div(style="display: inline-block;",actionButton("visualize", "Visualize", class = "btn-success")),
                div(style="display: inline-block;",downloadButton("downloadTable", "Download table (.csv)", class = "btn-primary")),
                div(style="display: inline-block;float:right;",actionButton("toProtein3", "Back to the protein table", class = "btn-warning")),
                tags$br(),
                div(style="display: inline-block;", checkboxInput("showComments2", "Show the commentary column", value = FALSE)),
                div(style="display: inline-block;vertical-align:top;width: 25px;", HTML("<br>")),
                div(style="display: inline-block;", checkboxInput("showLiterature2", "Show the Literature column (Pubmed ID)", value = FALSE)),
                tags$br(),
                title = "Parameter Table", width = 8, height = '80vh',
                div(style='height:60vh; overflow-y: scroll',
                DTOutput("distParameterTable"), height = '60vh')
              )
      ),
      
      # Information Avaible
      tabItem(tabName = "info",
              box(
                title = "Information", width = 4, height = '90vh',
                div(style='height:80vh; overflow-y: scroll',
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
              box(title = "Sidebar", width = 3, height = '90vh',
                  checkboxInput("new_table", "Work with a new table"),
                  conditionalPanel(condition = "input.new_table",
                                   fileInput("uploadTable", "Upload a new table")
                  ),
                  helpText("If you want to, you can work with a new table, ",
                           "or, if you already downloaded a table with ",
                           "parameters of enzymes in this site ",
                           "you can upload the table in .csv format ",
                           "here. Make sure to check the format of your table",
                           "to match the format of the tables of this side, ",
                           "to avoid posibles error."),
                  div(style = "text-align:center;",
                    actionButton("getDistribution", "Watch distribution", class = "btn-primary"))
              ),
              box(title = "Parameters Found", width = 9,
                  height = '85vh',
                  div(plotlyOutput("histogram"), height = '70vh', width = '70vh')
              )
      ),
      
      # Distribution
      tabItem(tabName = "distribution",
              box(title = "Sidebar", width = 3, height = '90vh'),
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
    ),
    div(
      style = "text-align:center;",
      hr(),
      p("~~~~~~~~~~Info~~~~~~~~~~")
    )
  )
)