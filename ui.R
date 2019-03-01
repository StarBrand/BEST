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
library(shinyBS)
library(rclipboard)
source("helpers.R")
source("variables.R")

dashboardPage(skin = "green",
  dashboardHeader(title = "Title",
                  tags$li(class = "dropdown",
                          div(style = "display: inline-block;",
                          uiOutput("savedTable")),
                          div(style = "display: inline-block;width: 400px;",
                          tags$br()),
                          div(style = "display: inline-block;margin-top: 5px;",
                          circleButton("goHelp", icon("question"), status = "success", size = "sm"),
                          circleButton("goProtein", icon("asterisk"), status = "success", size = "sm"),
                          circleButton("goSummary", icon("table"), status = "success", size = "sm"),
                          circleButton("goAccount", icon("user"), status = "success", size = "sm")),
                          bsTooltip("goHelp", "Tutorial", "bottom", "hover"),
                          bsTooltip("goProtein", "Protein Table", "bottom", "hover"),
                          bsTooltip("goSummary", "Avaiable Information", "bottom", "hover"),
                          bsTooltip("goAccount", "Account", "bottom", "hover"))
  ),
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
      menuItem("Visualization", tabName = "visualization", icon = icon("chart-area"), startExpanded = FALSE,
               menuSubItem("Parameters Found", tabName = "histogram"),
               menuSubItem("Distribution", tabName = "distribution"),
               menuSubItem("Correlation", tabName = "correlation")
               ),
      menuItem("Analysis", tabName = "analysis", icon = icon("project-diagram"),
               menuSubItem("Clustering", tabName = "cluster"),
               menuSubItem("External tools", tabName = "external")
               ),
      menuItem("Suggestion", tabName = "suggestion", icon = icon("eye")),
      menuItem("Acknowledgments", tabName = "acknowledgments", icon = icon("award")),
      menuItem("FAQ", tabName = "faq", icon = icon("question")),
      ### Hidden ones ###
      shinyjs::hidden(
        menuItem("phylogenyTree", tabName = "phylogenyTree"),
        menuItem("savedTableTab", tabName = "savedTableTab")),
      tags$hr(),
      div(style="text-align:center;",
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
                   helpText("Welcome to ..."),
                   helpText("To start using this app, we need to have access to the ",
                            tags$a("Brenda site", href="https://www.brenda-enzymes.org/"),
                            "."),
                   helpText("Please, enter a mail and password of a valid Brenda account."),
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
                   shinyjs::hidden(
                   actionButton(inputId = "logOut",
                                label = "log out",
                                class = "btn-danger")),
                   tags$br(),
                   tags$br(),
                   tags$hr(),
                   helpText("If you do not have a Brenda account register on ",
                            tags$a("Brenda/Registration", href="https://www.brenda-enzymes.org/register.php"))
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
                                  tags$a("Brenda/Registration", href="https://www.brenda-enzymes.org/register.php"))),
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
              includeHTML("www/tutorial.html")
      ),
      
      # Enzyme
      tabItem(tabName = "enzyme",
              box( title = "Select Enzyme", width = 4,
                   div(style='height:75vh; overflow-y: scroll',
                   h3("Chooce an input to look for the enzymes you need"),
                   helpText("The ... app allows multiple type of",
                            "inputs. Such as enter the ,",
                            tags$a("EC Number", href="https://qmul.ac.uk/sbcs/iubmb"),
                            ", write the name of the enzyme or ",
                            "upload a .txt file with UniProt code ",
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
                   withBusyIndicatorUI(
                       actionButton("ecNumber", "Enter", class = "btn-primary"))),
                   tags$br(),
                   tags$hr(),
                   h4("Enzyme name"), 
                   helpText("Select this input if you just know the name of your enzyme"),
                   helpText("But first, you have to know in which of this subclasses your enzyme is"),
                   selectInput("subclass", "Know the subclass of the type of enzyme?",
                               choices = list("Oxidoreductases" = "1",
                                              "Transferases" = "2",
                                              "Hydrolases" = "3",
                                              "Lyases" = "4",
                                              "Isomerases" = "5",
                                              "Ligases" = "6")),
                   div(style = "float:right;",
                       actionButton("enterSubclass", "Ok", class = "btn-primary")),
                   bsTooltip("enterSubsubclass", "Select one type of reaction, considering the functional group involved in the catalyzed reaction",
                             "rigth", "hover"),
                   bsTooltip("enzymeName", "Here, just type the name of your enzyme of interest and select the one that fits most",
                             "rigth", "hover"),
                   shinyjs::hidden(
                   selectInput("subsubclass", "In what functional group interact?",
                               choices = NULL),
                   actionButton("enterSubsubclass", "Ok", class = "btn-primary"),
                   selectizeInput("enzyme_name", "Select a Name of an Enzyme", choices = NULL),
                   actionButton("enzymeName", "Search", class = "btn-primary"),
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
              ),
              box(width = 8, heigth = '10vh', background = "green",
                  uiOutput("enzymeWillBe")
              ),
              box(width = 8, heigth = '80vh',
                div(style = "text-align: center;",
                imageOutput("subclass"))
              )
      ),
      
      # Protein Table
      tabItem(tabName = "proteinTable",
                box(title = "Select Parameters", width = 3, height = '85vh',
                    div(style='height:75vh; overflow-y: scroll',
                    h4('Parameters to be look up'),
                    materialSwitch("allParameters", "Select all functional parameters", right = TRUE, value = FALSE, status = "primary"),
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
                                                      ),
                    tags$hr(),
                    h4("Additional Options"),
                    materialSwitch("up", "Look up just for the proteins that has UniProt code",
                                  right = TRUE, value = FALSE, status = "warning"),
                    tags$br(),
                    div(style="text-align:center",
                    actionButton("addEnzyme", "Add proteins", class = "btn-warning"),
                    div(style = "display: inline-block;vertical-align:top;",
                        circleButton("helpAddEnzyme", icon("question"), status = "warning", size = "xs")),
                    bsTooltip("helpAddEnzyme", "You can add another type of enzyme (ec number) to your search. Note: every search and visualization is going to take more time",
                              "right", trigger = "hover", options = list(container = "body")),
                    tags$br(),
                    tags$br(),
                    actionButton("phylogeny", "Select by phylogeny", class = "btn-warning"),
                    div(style = "display: inline-block;vertical-align:top;",
                        circleButton("helpPhylogeny", icon("question"), status = "warning", size = "xs")),
                    bsTooltip("helpPhylogeny", "You can select the proteins using a phylogenetic tree. Note: This can take several minutes",
                              "right", trigger = "hover", options = list(container = "body"))),
                    tags$hr(),
                    div(style = "float: right;",
                        circleButton("helpParameters", icon("question"), status = "success", size = "xs"),
                        actionButton("parameters", "Search for parameters", class = "btn-success")),
                    bsTooltip("helpParameters", "Here you can obtain numerical parameters of the enzymes you want to", "top")
                )),
                box(
                  title = "Protein Table", width = 9, height = '85vh',
                  div(style="display: inline-block;",downloadButton("downloadProtein", "Download table(.csv)", class = "btn-primary")),
                  bsTooltip("downloadProtein", "Download this table in .csv format (comment column will be included)", "bottom", "hover"),
                  div(style="display: inline-block;",actionButton("toFasta", "Get sequence", class = "btn-success")),
                  bsTooltip("toFasta", "Search for the available sequence of all the proteins in this list. Note: this could take several minutes", "bottom", "hover"),
                  div(style="display: inline-block;",actionButton("toPDB", "Get PDB", class = "btn-success")),
                  bsTooltip("toPDB", "Search for the available PDB codes of this list, with the link provided", "bottom", "hover"),
                  div(style="display: inline-block;float:right;",actionButton("toEnzyme", "Go back and search another Enzyme", class = "btn-warning")),
                  bsTooltip("toEnzyme", "Go back and enter another group of enzymes, replacing the current ones", "bottom", "hover"),
                  tags$br(),
                  tags$br(),
                  div(style="display: inline-block;", materialSwitch("allProteins", "Serch for all the list", right = TRUE, value = TRUE, status = "primary")),
                  div(style="display: inline-block;vertical-align: top;", circleButton("helpAllProteins", icon("question"), status = "primary", size = "xs")),
                  bsTooltip("helpAllProteins", "Deactive this option to use just the selected proteins in the table below", "right"),
                  tags$br(),
                  div(style="display: inline-block;", checkboxInput("showComments1", "Show the commentary column", value = FALSE)),
                  div(style="display: inline-block;vertical-align:top;width: 25px;", HTML("<br>")),
                  div(style="display: inline-block;", checkboxInput("showLiterature1", "Show the Literature column (Pubmed ID)", value = FALSE)),
                  tags$br(),
                  div(style='height:60vh; overflow-y: scroll',
                  DTOutput("distProteinTable", height = '20vh'))
                )
      ),
      
      # Phylogeny tree
      tabItem(tabName = "phylogenyTree",
              box(title = "Taxa selected", width = 3, height = '90vh'),
              box(
                title = "Phylogenetic Tree", width = 9, heigth = '70vh',
                plotOutput("treeOut")
              )
      ),
      
      # FASTA sequence
      tabItem(tabName = "fasta",
              box(title = "Options", width = 3,
                  div(style='height:80vh; overflow-y: scroll',
                      p("Select the proteins you want to download"),
                      style="text-align:center", p("or"),
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
                  bsTooltip("downloadFASTA", "Download a .txt file with the selected (or all) sequence in FASTA format", "bottom"),
                  bsTooltip("toProtein1", "Go back to the protein table. Note: no data is going to be lost", "bottom"),
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
                  bsTooltip("downloadPDB", "Download this table in a .txt file", "buttom"),
                  bsTooltip("toProtein2", "Go back to the protein table. Note: no data is going to be lost", "bottom"),
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
                helpText("Move the sliders to filter the table prior further analysis. "),
                helpText("If you want to maintain all the obtained data, leave the sliders ",
                         "as they are and select no checkbox."),
                div(style='height:65vh; overflow-y: scroll',
                    div(style="display: inline-block;",checkboxInput(inputId = 'mw2', label = "")),
                    div(style="display: inline-block;width:40vh;",
                        sliderInput("mwFilter", h5("Filter Molecular Weight"),
                                    min = 0, max = 100, value = c(0,100))
                        ),
                    tags$br(),
                    div(style="display: inline-block;",checkboxInput(inputId = 'ic502', label = "")),
                    div(style="display: inline-block;width:40vh;",
                        sliderInput("ic50Filter", h5("Filter IC50 Value"),
                                    min = 0, max = 100, value = c(0,100))
                        ),
                    tags$br(),
                    div(style="display: inline-block;",checkboxInput(inputId = 'kc2', label = "")),
                    div(style="display: inline-block;width:40vh;",
                        sliderInput("kcFilter", h5("Filter Kcat/Km value"),
                                    min = 0, max = 100, value = c(0,100))
                        ),
                    tags$br(),
                    div(style="display: inline-block;",checkboxInput(inputId = 'ki2', label = "")),
                    div(style="display: inline-block;width:40vh;",
                        sliderInput("kiFilter", h5("Filter Ki value"),
                                    min = 0, max = 100, value = c(0,100))
                        ),
                    tags$br(),
                    div(style="display: inline-block;",checkboxInput(inputId = 'km2', label = "")),
                    div(style="display: inline-block;width:40vh;",
                        sliderInput("kmFilter", h5("Filter Km value"),
                                    min = 0, max = 100, value = c(0,100))
                        ),
                    tags$br(),
                    div(style="display: inline-block;",checkboxInput(inputId = 'pho2', label = "")),
                    div(style="display: inline-block;width:40vh;",
                        sliderInput("phoFilter", h5("Filter pH Optimum value"),
                                    min = 0, max = 100, value = c(0,100))
                        ),
                    tags$br(),
                    div(style="display: inline-block;",checkboxInput(inputId = 'phr2', label = "")),
                    div(style="display: inline-block;width:40vh;",
                        sliderInput("phrFilter", h5("Filter pH Range value"),
                                    min = 0, max = 100, value = c(0,100))
                        ),
                    tags$br(),
                    div(style="display: inline-block;",checkboxInput(inputId = 'pi2', label = "")),
                    div(style="display: inline-block;width:40vh;",
                        sliderInput("piFilter", h5("Filter pI value"),
                                    min = 0, max = 100, value = c(0,100))
                        ),
                    tags$br(),
                    div(style="display: inline-block;",checkboxInput(inputId = 'sa2', label = "")),
                    div(style="display: inline-block;width:40vh;",
                        sliderInput("saFilter", h5("Filter Specific Activity"),
                                    min = 0, max = 100, value = c(0,100))
                        ),
                    tags$br(),
                    div(style="display: inline-block;",checkboxInput(inputId = 'to2', label = "")),
                    div(style="display: inline-block;width:40vh;",
                        sliderInput("toFilter", h5("Filter Temperature Optimum value"),
                                    min = 0, max = 100, value = c(0,100))
                        ),
                    tags$br(),
                    div(style="display: inline-block;",checkboxInput(inputId = 'tr2', label = "")),
                    div(style="display: inline-block;width:40vh;",
                        sliderInput("trFilter", h5("Filter Temperature Range value"),
                                    min = 0, max = 100, value = c(0,100))
                        ),
                    tags$br(),
                    div(style="display: inline-block;",checkboxInput(inputId = 'ton2', label = "")),
                    div(style="display: inline-block;width:40vh;",
                        sliderInput("tonFilter", h5("Filter TurnOver number"),
                                    min = 0, max = 100, value = c(0,100))
                        )
                    )),
              box(
                div(style="display: inline-block;",actionButton("visualize", "Visualize", class = "btn-success")),
                div(style="display: inline-block;",actionButton("analyze", "Analyze", class = "btn-success")),
                div(style="display: inline-block;",downloadButton("downloadTable", "Download table (.csv)", class = "btn-primary")),
                div(style="display: inline-block;float:right;",actionButton("toProtein3", "Back to the protein table", class = "btn-warning")),
                tags$br(),
                div(style="display: inline-block;", checkboxInput("showComments2", "Show the commentary column", value = FALSE)),
                div(style="display: inline-block;vertical-align:top;width: 25px;", HTML("<br>")),
                div(style="display: inline-block;", checkboxInput("showLiterature2", "Show the Literature column (Pubmed ID)", value = FALSE)),
                bsTooltip("visualize", "Go to the visualization of this data. Note: The data will be filtered", "bottom"),
                bsTooltip("downloadTable", "Download this table in .csv format with the filters applied. The commentary and the literature column will be included", "bottom"),
                bsTooltip("toProtein3", "Go back to the protein table. Note: no data is going to be lost", "bottom"),
                tags$br(),
                title = "Parameter Table", width = 8, height = '80vh',
                div(style='height:60vh; overflow-y: scroll',
                DTOutput("distParameterTable"), height = '55vh')
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
                actionLink("filterInput", "click here")),
                tags$hr(),
                h5("Summary"),
                tableOutput("informationTable")
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
              box(width = 3, height = '80vh',
                  checkboxInput("new_table", "Work with a new table"),
                  shinyjs::hidden(
                    fileInput("uploadTable", "Upload a new table")),
                  helpText("If you want to, you can work with a new table, ",
                           "or, if you already downloaded a table with ",
                           "parameters of enzymes in this site ",
                           "you can upload the table in .csv format ",
                           "here. Make sure to check the format of your table",
                           "to match the format of the tables of this side, ",
                           "to avoid posible errors."),
                  div(style = "text-align:center;",
                    actionButton("getDistribution", "Watch distribution", class = "btn-primary"),
                    div(style = "display: inline-block;", circleButton("helpDistribution", icon("question"), status = "primary", size = "xs")),
                    tags$hr(),
                    actionButton("getCorrelation", "Watch correlation", class = "btn-primary", width = "200px"),
                    tags$br(),
                    tags$br(),
                    actionButton("getCorrelationMatrix", "As heatmap matrix", class = "btn-success"),
                    div(style = "display: inline-block;", circleButton("helpCorrelationMatrix", icon("question"), status = "success", size = "xs")),
                    tags$br(),
                    tags$br(),
                    actionButton("getCorrelationScatter", "As a scatterplot matrix", class = "btn-success"),
                    div(style = "display: inline-block;", circleButton("helpCorrelationScatter", icon("question"), status = "success", size = "xs"))
                  ),
                  bsTooltip("helpDistribution", "Generate a boxplot showing the distribution of the functional parameters found", "right"),
                  bsTooltip("getCorrelation", "Generate the correlation of the data found. Note: This can take several minutes", "right"),
                  bsTooltip("helpCorrelationMatrix", "Generate and show a heatmap matrix representing the correlation of each pairs of functional parameters", "right"),
                  bsTooltip("helpCorrelationScatter", "Generate and show a scatterplot matrix, in which, its cells show a two dimensional plot with a pair of functional parameter as axis", "right")
              ),
              box(title = "Parameters Found", width = 9,
                  height = '80vh',
                  div(plotlyOutput("histogram"), height = '70vh', width = '70vh')
              )
      ),
      
      # Distribution
      tabItem(tabName = "distribution",
              box(title = "Information", width = 3, height = '85vh',
                  helpText("This show the distribution of every functional parameters filtered and found. ",
                           "The box plot contains the measures of position, such as the quartils, median and ",
                           "average. The outliers are shown as point indicating value, organism and recommended ",
                           "name of the enzyme"),
                  helpText("Due to the different scale we recommend to show the functional parameters separately ",
                           "using the selector. Double click in one parameter is going to hide all the other parameters, ",
                           "one click and that parameter is going to hide."),
                  helpText("The parameters of the same scale are:"),
                  helpText("- Molecular Weight"),
                  helpText("- IC50, Kcat/Km, Ki, Km, Specific Activity, Turmover Number"),
                  helpText("- pH Optimum, pH Range, pI"),
                  helpText("- Temperature Optimum, Temperature Range")),
              box(title = "Distribution", width = 9,
                  height = '70vh',
                  div(style = "float: right;", actionButton("backVisualization1", "Go back to visualization", class = "btn-warning")),
                  tags$br(),
                  tags$br(),
                  tags$br(),
                  div(
                  plotlyOutput("distributionOut"),
                  height = '70vh')
              )
      ),
      
      # Correlation
      tabItem(tabName = "correlation",
              box(width = 3, height = '85vh',
                  conditionalPanel(condition = "input.correlationPlot == 'matrix'",
                                   h5("Options"), 
                                   radioButtons("correlationEquation", "Change the type of correlation calculated",
                                                choices = list("Pearson" = "pearson",
                                                               "Kendall" = "kendall",
                                                               "Spearman" = "spearman")),
                                   radioButtons("correlationColor", "Change the palette of the correlation",
                                                choices = list("Default",
                                                               "Spectral",
                                                               "Red/Yellow/Green" = "RdYlGn",
                                                               "Red/Yellow/Blue" = "RdYlBu"))
                  ),
                  conditionalPanel(condition = "input.correlationPlot == 'scatter'",
                                   div(style='height:75vh; overflow-y: scroll',
                                       checkboxGroupInput("corScat", "Select functional parameters to get correlation as a scatter plots, 2 to 6 parameters available"),
                                       tags$br(),
                                       actionButton("getCorrelationScatter2", "Get correlation", class = "btn-primary"),
                                       tags$br(),
                                       tags$hr(),
                                       helpText("To generate a good correlation pairing we recommend generate the heatmap matrix first ",
                                                "in order to make a better decisions of what parameters to choose."))
                  )
              ),
              tabBox(title = "Correlation", id = "correlationPlot", selected = "matrix", width = 9, height = '70vh',
                     tabPanel(value = "matrix", h5("Heatmap matrix"),
                         div(style = "float: right;", actionButton("backVisualization2", "Go back to visualization", class = "btn-warning")),
                         tags$br(),
                         tags$br(),
                         tags$br(),
                         plotlyOutput("correlationOut")
                         ),
                     tabPanel(value = "scatter", h5("Scatterplot matrix"),
                         div(style = "float: right;", actionButton("backVisualization3", "Go back to visualization", class = "btn-warning")),
                         tags$br(),
                         tags$br(),
                         tags$br(),
                         plotlyOutput("correlationOut2")
                         )
                
              )
      ),
      
      # Clustering
      tabItem(tabName = "cluster",
              box(width = 3, height = '85vh',
                  conditionalPanel(condition = "input.clusterPlot == 'home'",
                                   h5("Information"),
                                   helpText("Clustering is an analysis technic that divide data into meaningful or useful ",
                                            "groups. This section allows you to use three different cluster algorithms using ",
                                            "the functional parameters recovered from Brenda."),
                                   tags$br(),
                                   helpText("The amount of functional parameters you can use on the clustering depends on ",
                                            "the ocuppied technic."),
                                   tags$br(),
                                   div(style = "text-align:center;",
                                       actionButton("toKMeans", "K-means Clustering", class = "btn-success")),
                                   tags$hr(),
                                   helpText("For more information about clustering visit ...")
                                   ),
                  conditionalPanel(condition = "input.clusterPlot == 'k-means'",
                                   div(style='height:75vh; overflow-y: scroll',
                                   checkboxGroupInput("kmeans", "Select two or three parameters to do a K-means clustering"),
                                   div(style = "float: right;",
                                       actionButton("goKmeans", "Clusterize", class = "btn-primary"),
                                       circleButton("elbowInfo", icon("question"), status = "primary", size = "xs")),
                                   tags$br(),
                                   tags$hr(),
                                   bsTooltip("elbowInfo", 'K (number of clusters) is determined by calculating the "elbow", which is the K with the biggest decrease of WSS (within-cluster sum of square), showing below', "top"),
                                   tags$br(),
                                   div(style = "text-align: center;",
                                       h5(div(style = "text_weigth: bold;", "Elbow Method to determine the k, number of clusters")),
                                       plotOutput("elbowKMeans", width = "260px", height = "260px")),
                                   uiOutput("kmeansDimInfo"))
                                   )
                  ),
              tabBox(title = "Clustering", id = "clusterPlot", selected = "home", width = 9, height = '90vh',
                     tabPanel(value = "home", h5("Clustering")),
                     tabPanel(value = "k-means", h5("K-means Clusteing"),
                              downloadButton("downloadKmeans", "Download clusterization", class = "btn-primary"),
                              div(plotlyOutput("kmeansPlot"), height = '80vh'))
              )
      ),
      
      # External link
      tabItem(tabName = "external",
              box(width = 3, height = '90vh'),
              box(width = 9, height = '80vh')
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
      ),
      
      # Saved Table
      tabItem(tabName = "savedTableTab",
              box(title = "Information", width = 3, height = '80vh',
                  helpText("... saves your last query for 72 hours ",
                           "in order to do your analysis easier. ",
                           "If you want to load them, select the available ",
                           "tables in the right panel. To load any table, selecting the ",
                           "saved protein table, which contains the organisms ",
                           "and type of enzyme according to the ec number, is mandatory."),
                  tags$hr(),
                  helpText("If you do not want to load the tables, you can click the button ",
                           "below. Consider this will erase the saved tables, ",
                           div(style = "display: inline-block;font-weight: bold;",
                               "this cannot be undone.")),
                  tags$br(),
                  div(style = "text-align: center;",
                  actionButton("toEnzyme2", "Go back and search another enzyme", class = "btn-danger"))),
              box(width = 9, height = '80vh',
                  DTOutput("summarySavedTable"),
                  tags$br(),
                  tags$br(),
                  div(style = "float: right;",
                      actionButton("loadTable", "Load Table Selected", class = "btn-primary")))
      )
    ),
    div(
      style = "text-align:center;",
      hr(),
      p("~~~~~~~~~~Info~~~~~~~~~~")
    )
  )
)