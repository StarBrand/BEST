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
  dashboardHeader(title = div(style = "font-weight: bold;", "FBS"),
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
               menuSubItem("Available Information", tabName = "info")
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
      menuItem("Suggestions", tabName = "suggestions", icon = icon("eye")),
      menuItem("Acknowledgments", tabName = "acknowledgments", icon = icon("award")),
      menuItem("FAQ", tabName = "faq", icon = icon("question")),
      ### Hidden ones ###
      shinyjs::hidden(
        menuItem("phylogenyTree", tabName = "phylogenyTree"),
        menuItem("savedTableTab", tabName = "savedTableTab")),
      tags$hr(),
      div(style="text-align:center;",
      h3("First time?"),
      helpText("You can always read"),
      helpText("the turorial section ",
               actionLink("help", "here")))
    )
  ),
  dashboardBody(
    useShinyjs(),
    useShinyalert(),
    tabItems(
      # Log in
      tabItem(tabName = "account",
              box( title = div(style = "font-weight: bold;", "Brenda User"), width = 3, height = '80vh',
                   helpText("Welcome to Fast Brenda Searcher"),
                   helpText("To start using this app, we need to have access to the ",
                            tags$a("Brenda site", href="https://www.brenda-enzymes.org/", target = "_blank"),
                            "."),
                   helpText("Please, enter a mail and password of a valid Brenda account."),
                   tags$br(),
                   textInput(inputId = "mail",
                             label = "Mail"),
                   passwordInput(inputId = "pass",
                             label = "password"),
                   div(
                   actionButton(inputId = "logIn",
                                label = div(style = "font-weight: bold", "log in"),
                                class = "btn-success",
                                style = "color: #ffffff"),
                   style = "float:right"),
                   shinyjs::hidden(
                   actionButton(inputId = "logOut",
                                label = div(style = "font-weight: bold", "log out"),
                                class = "btn-danger",
                                style = "color: #ffffff")),
                   tags$br(),
                   tags$br(),
                   tags$hr(),
                   helpText("If you do not have a Brenda account register on ",
                            tags$a("Brenda/Registration", href="https://www.brenda-enzymes.org/register.php", target = "_blank"))
              ),
              box(width = 9, height = '60vh',
                  img(src = 'Cover.jpg',
                      height = '390vh',
                      style = "display: block; margin-left: auto; margin-right: auto;")
              ),
              box(width = 9, heigth = '30vh',
                  column(3, h5("Brenda User"),
                         helpText("To start using the FBS app, you need a valid Brenda User, ",
                                  "if you don't, please, visit:",
                                  tags$a("Brenda/Registration", href="https://www.brenda-enzymes.org/register.php", target = "_blank"))),
                  column(5, h5("Protein Search"),
                         helpText("In this app you can access all the Brenda potencial in a more intuitive way. ",
                                  "Generating a single table that contains the different parameters of an enzyme separated ",
                                  "by the organism in which it is")),
                  column(4, h5("Analysis and visualization of data"),
                         helpText("You can also do visualization of the found data or analyse using cluster. We also ",
                                  "provides links to other online tools"))
              )
      ),
      
      
      # Help
      tabItem(tabName = "help",
              includeHTML("www/tutorial.html")
      ),
      
      # Enzyme
      tabItem(tabName = "enzyme",
              box(title = div(style = "font-weight: bold;", "Select Enzyme"), width = 4,
                  div(style='height:75vh; overflow-y: scroll',
                  h3("Chooce an input to look for the enzymes you need"),
                  helpText("The FBS app allows multiple type of",
                           "inputs. Such as enter the ,",
                           tags$a("EC Number", href="https://qmul.ac.uk/sbcs/iubmb", target = "_blank"),
                           ", or write the name of the enzyme"),
                  tags$br(),
                  h4("1. Enter EC Number"),
                  helpText("Select this input if you ",
                           "know the EC number of your enzyme ",
                           "of interest, you can check it in: ",
                           tags$a("EC Number Info", href="https://qmul.ac.uk/sbcs/iubmb", target = "_blank")),
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
                        actionButton("ecNumber", div(style = "font-weight: bold", "Enter"), class = "btn-primary", style = "color: #ffffff"))),
                  tags$br(),
                  tags$hr(),
                  h4("2. Enzyme name"),
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
                      actionButton("enterSubclass", div(style = "font-weight: bold", "Ok"), class = "btn-primary", style = "color: #ffffff")),
                  bsTooltip("enterSubsubclass", "Select one type of reaction, considering the functional group involved in the catalyzed reaction",
                            "rigth", "hover"),
                  bsTooltip("enzymeName", "Here, just type the name of your enzyme of interest and select the one that fits most",
                            "rigth", "hover"),
                  shinyjs::hidden(
                    selectInput("subsubclass", "In what functional group interact?",
                                choices = NULL),
                    actionButton("enterSubsubclass", div(style = "font-weight: bold", "Ok"), class = "btn-primary", style = "color: #ffffff"),
                    selectizeInput("enzyme_name", "Select a Name of an Enzyme", choices = NULL),
                    actionButton("enzymeName", div(style = "font-weight: bold", "Search"), class = "btn-primary", style = "color: #ffffff"),
                    radioButtons("pick", "Which one of these?",
                                 choices = list("..."))),
                  div(style = "float:right",
                      shinyjs::hidden(
                        actionButton("enzymeNameFinal", div(style = "font-weight: bold", "Go"), class = "btn-success", style = "color: #ffffff")
                        ))
                  )
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
                box(title = div(style = "font-weight: bold;", "Select Parameters"), width = 3, height = '85vh',
                    div(style='height:75vh; overflow-y: scroll',
                    helpText("You can select functional parameters to be work with. Click ",
                             "the checkbox below to select the wanted parameters. Or, activate ",
                             "the \"Select all functional parameters\" switch to select all ",
                             "twelve numerical parameters at once."),
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
                    h4(div(style = "font-weight: bold;", "Additional Options")),
                    materialSwitch("up", "Look up just for the proteins that has UniProt code",
                                  right = TRUE, value = FALSE, status = "warning"),
                    tags$br(),
                    actionButton("addEnzyme", div(style = "font-weight: bold", "Add proteins"), class = "btn-warning", style = "color: #ffffff"),
                    div(style = "display: inline-block;vertical-align:top;",
                        circleButton("helpAddEnzyme", icon("question"), status = "warning", size = "xs")),
                    bsTooltip("helpAddEnzyme", "You can add another type of enzyme (ec number) to your search. Note: every search and visualization is going to take more time",
                              "right", trigger = "hover", options = list(container = "body")),
                    tags$br(),
                    tags$br(),
                    actionButton("phylogeny", div(style = "font-weight: bold", "Select by phylogeny"), class = "btn-warning", style = "color: #ffffff"),
                    div(style = "display: inline-block;vertical-align:top;",
                        circleButton("helpPhylogeny", icon("question"), status = "warning", size = "xs")),
                    bsTooltip("helpPhylogeny", "You can select the proteins using a phylogenetic tree. Note: This can take several minutes",
                              "right", trigger = "hover", options = list(container = "body")),
                    tags$hr(),
                    div(style = "float: right;",
                        actionButton("parameters", div(style = "font-weight: bold", "Search"), class = "btn-primary", style = "color: #ffffff;"),
                        circleButton("helpParameters", icon("question"), status = "primary", size = "xs")),
                    bsTooltip("helpParameters", "Here you can obtain numerical parameters of the enzymes you want to", "top")
                )),
                box(
                  title = uiOutput("proteinTableTitle"), width = 9, height = '85vh',
                  div(style="display: inline-block;", downloadButton("downloadProtein", "Download table(.txt)", class = "btn-primary", style = "color: #ffffff; font-weight: bold;")),
                  bsTooltip("downloadProtein", "Download this table in .txt format (comment column will be included)", "bottom", "hover"),
                  div(style="display: inline-block;",actionButton("toFasta", div(style = "font-weight: bold", "Get sequence"), class = "btn-success", style = "color: #ffffff")),
                  bsTooltip("toFasta", "Search for the available sequence of all the proteins in this list. Note: this could take several minutes", "bottom", "hover"),
                  div(style="display: inline-block;",actionButton("toPDB", div(style = "font-weight: bold", "Get PDB"), class = "btn-success", style = "color: #ffffff")),
                  bsTooltip("toPDB", "Search for the available PDB codes of this list, with the link provided", "bottom", "hover"),
                  div(style="display: inline-block;float:right;",actionButton("toEnzyme", div(style = "font-weight: bold", "Go back and search another Enzyme"), class = "btn-warning", style = "color: #ffffff")),
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
              box(title = div(style = "font-weight: bold;", "Taxa selected"), width = 3, height = '90vh'),
              box(
                title = div(style = "font-weight: bold;", "Phylogenetic Tree"), width = 9, heigth = '70vh',
                plotOutput("treeOut")
              )
      ),
      
      # FASTA sequence
      tabItem(tabName = "fasta",
              box(title = div(style = "font-weight: bold;", "Options"), width = 3,
                  div(style='height:80vh; overflow-y: scroll',
                      p("Select the proteins you want to download"),
                      style="text-align:center", p("or"),
                      materialSwitch("no_filter", "Download all the list", right = TRUE, value = TRUE, status = "primary"),
                      tags$hr(),
                      p("There are some sequence that couldn't ",
                        "find using Brenda. We are working to do ",
                        "something about it. Meanwhile, those sequence ",
                        " are going to leave blank."))
              ),
              
              box(title = div(style = "font-weight: bold;", "FASTA Sequence"), width = 9,
                  div(style="display: inline-block;", downloadButton("downloadFASTA", "Download sequence in FASTA format", class = "btn-primary", style = "color: #ffffff; font-weight: bold;")),
                  div(style="display: inline-block; width: 10px", tags$br()),
                  div(style="display: inline-block;",rclipButton("copyAAClipboard", "Copy a Sequence to Clipboard", "AAA", icon = icon("clipboard"))),
                  div(style="display: inline-block;float:right;",actionButton("toProtein1", div(style = "font-weight: bold", "Back to the protein table"), class = "btn-warning", style = "color: #ffffff")),
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
              box(title = div(style = "font-weight: bold;", "Information"), width = 3,
                  div(style='height:80vh; overflow-y: scroll',
                  helpText("Protein Data Bank (PDB) is an open access digital data resource. ",
                           "Through PDB code, showed on this table, you can access to 3D structure data ",
                           "of the enzyme."),
                  helpText("For more info you can access ", tags$a(href = "https://www.rcsb.org/pages/about-us/index", "RCSB PDB page", target = "_blank")),
                  helpText("They can also have a tutorial pages such as ", tags$a(href = "http://pdb101.rcsb.org/more/about-pdb-101", "PDB 101", target = "_blank"), 
                           " and ", tags$a(href = "http://www.wwpdb.org/deposition/tutorial", "wwPDB tutorial", target = "_blank"), ".")
                  )
              ),
              box(title = div(style = "font-weight: bold;", "PDB Table"), width = 9,
                  tags$br(),
                  div(style="display: inline-block;", downloadButton("downloadPDB", "Download PDB Table", class = "btn-primary", style = "color: #ffffff; font-weight: bold;")),
                  div(style="display: inline-block;float:right;",actionButton("toProtein2", div(style = "font-weight: bold", "Back to the protein table"), class = "btn-warning", style = "color: #ffffff")),
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
              box(title = div(style = "font-weight: bold;", "Numerical Filter"),
                width = 4, height = '90vh',
                helpText("Move the sliders to filter the table prior further analysis. "),
                helpText("If you want to maintain all the obtained data, leave the sliders ",
                         "as they are and select no checkbox."),
                div(style = "float: right;", actionButton("filter", div(style = "font-weight: bold", "Filter"), class = "btn-primary", style = "color: #ffffff")),
                tags$br(),
                tags$br(),
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
                div(style="display: inline-block;",actionButton("visualize", div(style = "font-weight: bold", "Visualize"), class = "btn-success", style = "color: #ffffff")),
                div(style="display: inline-block;",actionButton("analyze", div(style = "font-weight: bold", "Analyze"), class = "btn-success", style = "color: #ffffff")),
                div(style="display: inline-block;",downloadButton("downloadTable", "Download table (.txt)", class = "btn-primary", style = "color: #ffffff; font-weight: bold;")),
                div(style="display: inline-block;float:right;",actionButton("toProtein3", div(style = "font-weight: bold", "Back to the protein table"), class = "btn-warning", style = "color: #ffffff")),
                tags$br(),
                div(style="display: inline-block;", checkboxInput("showComments2", "Show the commentary column", value = FALSE)),
                div(style="display: inline-block;vertical-align:top;width: 25px;", HTML("<br>")),
                div(style="display: inline-block;", checkboxInput("showLiterature2", "Show the Literature column (Pubmed ID)", value = FALSE)),
                bsTooltip("visualize", "Go to the visualization of this data. Note: The data will be filtered", "bottom"),
                bsTooltip("downloadTable", "Download this table in .txt format with the filters applied. The commentary and the literature column will be included", "bottom"),
                bsTooltip("toProtein3", "Go back to the protein table. Note: no data is going to be lost", "bottom"),
                tags$br(),
                title = div(style = "font-weight: bold;", "Parameter Table"), width = 8, height = '80vh',
                div(style='height:60vh; overflow-y: scroll',
                DTOutput("distParameterTable"), height = '55vh')
              )
      ),
      
      # Information Avaible
      tabItem(tabName = "info",
              box(
                title = div(style = "font-weight: bold;", "Information"), width = 4, height = '90vh',
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
                h5(div(style = "font-weight: bold;", "Summary")),
                tableOutput("informationTable")
                )
              ),
              box(
                title = div(style = "font-weight: bold;", "Available Information"), width = 8, height = '80vh',
                div(style="display: inline-block;",actionButton("generateSummary", label = div(style = "font-weight: bold", "Generate"), class = "btn-primary", style = "color: #ffffff")),
                div(style='height:60vh; overflow-y: scroll',
                tags$br(),
                tags$br(),
                DTOutput("distSummaryTable"), height = '70vh')
              )
      ),
      
      # Parameters Found show with an Histogram
      tabItem(tabName = "histogram",
              box(width = 3, height = '80vh',
                  helpText("Here you can visualize your data in different format. ",
                           "On the right, you can see a barplot showing the found data using ",
                           "Brenda, separated by parameter."),
                  div(style = "text-align:center;",
                    actionButton("getDistribution", div(style = "font-weight: bold", "Watch distribution"), class = "btn-primary", style = "color: #ffffff"),
                    div(style = "display: inline-block;", circleButton("helpDistribution", icon("question"), status = "primary", size = "xs")),
                    tags$hr(),
                    actionButton("getCorrelation", div(style = "font-weight: bold", "Watch correlation"), class = "btn-primary", width = "200px", style = "color: #ffffff"),
                    tags$br(),
                    tags$br(),
                    actionButton("getCorrelationMatrix", div(style = "font-weight: bold", "As heatmap matrix"), class = "btn-success", style = "color: #ffffff"),
                    div(style = "display: inline-block;", circleButton("helpCorrelationMatrix", icon("question"), status = "success", size = "xs")),
                    tags$br(),
                    tags$br(),
                    actionButton("getCorrelationScatter", div(style = "font-weight: bold", "As a scatterplot matrix"), class = "btn-success", style = "color: #ffffff"),
                    div(style = "display: inline-block;", circleButton("helpCorrelationScatter", icon("question"), status = "success", size = "xs"))
                  ),
                  bsTooltip("helpDistribution", "Generate a boxplot showing the distribution of the functional parameters found", "right"),
                  bsTooltip("getCorrelation", "Generate the correlation of the data found. Note: This can take several minutes", "right"),
                  bsTooltip("helpCorrelationMatrix", "Generate and show a heatmap matrix representing the correlation of each pairs of functional parameters", "right"),
                  bsTooltip("helpCorrelationScatter", "Generate and show a scatterplot matrix, in which, its cells show a two dimensional plot with a pair of functional parameter as axis", "right")
              ),
              box(title = div(style = "font-weight: bold;", "Parameters Found"), width = 9,
                  height = '80vh',
                  div(style = "float: right; ", actionButton("backParameter1", div(style = "font-weight: bold", "Go back to Parameter Table"), class = "btn-warning", style = "color: #ffffff")),
                  tags$br(),
                  tags$br(),
                  tags$br(),
                  div(plotlyOutput("histogram"), height = '70vh', width = '70vh')
              )
      ),
      
      # Distribution
      tabItem(tabName = "distribution",
              box(title = div(style = "font-weight: bold;", "Information"), width = 3, height = '85vh',
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
              box(title = div(style = "font-weight: bold;", "Distribution"), width = 9,
                  height = '70vh',
                  div(style = "float: right;", actionButton("backVisualization1", div(style = "font-weight: bold", "Go back to visualization"), class = "btn-warning", style = "color: #ffffff")),
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
                                   tags$br(),
                                   helpText("The correlation calculated was done using the ",
                                            tags$a("Pearson method", href = "https://libguides.library.kent.edu/SPSS/PearsonCorr", target = "_blank"),
                                            ", which calculated a linear correlation factor. A correlation (r) equals 1 means a positive ",
                                            "linal correlation, and r = -1 indicates a negative one. r = 0 indicate a no correlation between ",
                                            "those two variables. Pearson correlation asumes a normal distribution of the data."),
                                   radioButtons("correlationColor", "Change the palette of the correlation",
                                                choices = list("Default",
                                                               "Spectral",
                                                               "Red/Yellow/Green" = "RdYlGn",
                                                               "Red/Yellow/Blue" = "RdYlBu",
                                                               "Red/Gray" = "RdGy",
                                                               "Red/Blue" = "RdBu",
                                                               "Orange/Purple" = "PuOr",
                                                               "Purple/Green" = "PRGn",
                                                               "Pink/Green" = "PiYG",
                                                               "Brown/Blue-Green" = "BrBG"))
                  ),
                  conditionalPanel(condition = "input.correlationPlot == 'scatter'",
                                   div(style='height:75vh; overflow-y: scroll',
                                       checkboxGroupInput("corScat", "Select functional parameters to get correlation as a scatter plots, 2 to 6 parameters available"),
                                       tags$br(),
                                       actionButton("getCorrelationScatter2", div(style = "font-weight: bold", "Get correlation"), class = "btn-primary", style = "color: #ffffff"),
                                       tags$br(),
                                       tags$hr(),
                                       helpText("To generate a good correlation pairing we recommend generate the heatmap matrix first ",
                                                "in order to make a better decisions of what parameters to choose."))
                  )
              ),
              tabBox(title = div(style = "font-weight: bold;", "Correlation"), id = "correlationPlot", selected = "matrix", width = 9, height = '70vh',
                     tabPanel(value = "matrix", h5("Heatmap matrix"),
                              div(style = "float: right;", actionButton("backVisualization2", div(style = "font-weight: bold", "Go back to visualization"), class = "btn-warning", style = "color: #ffffff")),
                              tags$br(),
                              tags$br(),
                              tags$br(),
                              plotlyOutput("correlationOut")
                              ),
                     tabPanel(value = "scatter", h5("Scatterplot matrix"),
                              div(style = "display: inline-block; color: #202453", icon("circle")),
                              div(style = "display: inline-block; width: 10px;", tags$br()),
                              div(style = "display: inline-block; ", "Wild Type"),
                              div(style = "display: inline-block; width: 40px;", tags$br()),
                              div(style = "display: inline-block; color: #4c9e4a", icon("circle")),
                              div(style = "display: inline-block; width: 10px;", tags$br()),
                              div(style = "display: inline-block; ", "Mutant"),
                              div(style = "display: inline-block; float: right;", actionButton("backVisualization3", div(style = "font-weight: bold", "Go back to visualization"), class = "btn-warning", style = "color: #ffffff")),
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
                                       actionButton("toKMeans", div(style = "font-weight: bold", "K-means Clustering"), class = "btn-success", style = "color: #ffffff")),
                                   tags$br(),
                                   div(style = "text-align:center;",
                                       actionButton("toDBSCAN", div(style = "font-weight: bold", "DBSCAN Clustering"), class = "btn-success", style = "color: #ffffff")),
                                   tags$hr(),
                                   helpText("For more information about clustering visit ", tags$a("this scientific review", href = "https://dl.acm.org/citation.cfm?doid=568574.568575", target = "_blank"))
                                   ),
                  conditionalPanel(condition = "input.clusterPlot == 'k-means'",
                                   div(style='height:75vh; overflow-y: scroll',
                                   checkboxGroupInput("kmeans", "Select two or three parameters to do a K-means clustering"),
                                   div(style = "float: right;",
                                       actionButton("goKmeans", div(style = "font-weight: bold", "Clusterize"), class = "btn-primary", style = "color: #ffffff"),
                                       circleButton("elbowInfo", icon("question"), status = "primary", size = "xs")),
                                   tags$br(),
                                   tags$hr(),
                                   bsTooltip("elbowInfo", 'K (number of clusters) is determined by calculating the "elbow", which is the K with the biggest decrease of WSS (within-cluster sum of square), showing below', "top"),
                                   tags$br(),
                                   div(style = "text-align: center;",
                                       h5(div(style = "text_weigth: bold;", "Elbow Method to determine the k, number of clusters")),
                                       plotOutput("elbowKMeans", width = "260px", height = "260px")),
                                   uiOutput("kmeansDimInfo"))
                                   ),
                  conditionalPanel(condition = "input.clusterPlot == 'dbscan'",
                                   div(style='height:75vh; overflow-y: scroll',
                                   checkboxGroupInput("dbscanSelector", "Select two or three parameters to do a DBSCAN clustering"),
                                   tags$br(),
                                   actionButton("dbscanMerge", div(style = "font-weight: bold", "Merge data to clusterize"), class = "btn-primary", style = "color: #ffffff"),
                                   tags$br(),
                                   tags$hr(),
                                   p("Select the hyperparameters"),
                                   plotOutput("histDBSCAN", width = "260px", height = "260px"),
                                   sliderInput("eps", "Enter epsilon", value = 1, min = 0, max = 1),
                                   sliderInput("minPts", "Enter minimum of points per cluster", value = 1, min = 0, max = 1),
                                   tags$br(),
                                   div(style = "float: right;",
                                       actionButton("goDbscan", div(style = "font-weight: bold", "Clusterize"), class = "btn-success", style = "color: #ffffff")),
                                   tags$br(),
                                   uiOutput("dbscanDimInfo"))
                                   )
                  ),
              tabBox(title = div(style = "font-weight: bold;", "Clustering"), id = "clusterPlot", selected = "home", width = 9, height = '90vh',
                     tabPanel(value = "home", h5("Clustering"),
                              div(style = "float: right;", actionButton("backParameter2", div(style = "font-weight: bold", "Go back to Parameter Table"), class = "btn-warning", style = "color: #ffffff"))
                              ),
                     tabPanel(value = "k-means", h5("K-means Clustering"),
                              div(style = "width: 200px;", downloadButton("downloadKmeans", "Download clusterization", class = "btn-primary", style = "color: #ffffff; font-weight: bold;")),
                              div(plotlyOutput("kmeansPlot"), height = '80vh')
                              ),
                     tabPanel(value = "dbscan", h5("DBSCAN Clustering"),
                              div(style = "width: 200px;", downloadButton("downloadDBSCAN", "Download clusterization", class = "btn-primary", style = "color: #ffffff; font-weight: bold;")),
                              div(plotlyOutput("dbscanPlot"), height = '80vh')
                              )
              )
      ),
      
      # External link
      tabItem(tabName = "external",
              box(width = 6, title = div(style = "font-weight: bold;", "Protein Prediction"),
                  img(src = 'scratch_header.jpg',
                      height = "62px",
                      style = "display: block; margin-left: auto; margin-right: auto;"),
                  tags$br(),
                  helpText("Univeristy of California, Irvine has developed a Protein Predictor. Using RNN ",
                           "(Recursive Neural Network), this tool can predict properties such as domains, secondary structure, tertiary structure ",
                           "and more (", tags$a(href = "http://scratch.proteomics.ics.uci.edu/explanation.html", "more info", target = "_blank"), "). The ",
                           "link to this online version tool is below."),
                  tags$br(),
                  div(style = "text-align: center;", actionButton("proteinPredictor", div(style = "font-weight: bold", "SCRATCH Protein Prediction"), class = "btn-success",
                                                                  onclick ="window.open('http://scratch.proteomics.ics.uci.edu/', '_blank')", style = "color: #ffffff")),
                  tags$br(),
                  tags$br(),
                  helpText("We provide you the possibility of use one of the sequence you got using FBS as input of this predictor. ",
                           "Click the button below and you will be redirected to the AA sequence section.",
                           "In there, you can select one of the obtained sequence and click the \"Copy to clipboard\" button above the table. ",
                           "Then, go to the Protein Predictor and paste the sequence you selected, it will be already copy."),
                  tags$br(),
                  div(style = "text-align: center;", actionButton("copySequence", div(style = "font-weight: bold", "Copy a sequence from the obtained ones"), class = "btn-primary", style = "color: #ffffff")),
                  tags$br(),
                  tags$br()
                  ),
              box(width = 6, height = '80vh')
      ),
      
      # Suggestions
      tabItem(tabName = "suggestions",
              column(width = 1),
              box(title = h1(icon("eye"), "Suggestions"), width = 10, height = '90vh', solidHeader = TRUE, status = "success", 
                  div(style='height:70vh; overflow-y: scroll;',
                      div(style = "margin-right: 50px; margin-left: 50px;",
                      p(icon("check"), "This is a project in development. Hence, in order to make this application more functional ",
                        "and useful for the generation of knowledge and to contribute to the scientific community, ",
                        "we are open to any suggestion to improve this app."),
                      tags$br(),
                      p("So, first, choose the kind of suggestion you want us to know:"),
                      div(style = "width: 800px;",
                      pickerInput("suggestionType", "", choices = list("functions", "tutorial", "graphics", "bug",
                                                                       "wrong_data", "question", "others", "unclasified"),
                                  choicesOpt = list(content = c(I(paste(icon("check"), "A new useful functionalty")),
                                                                I(paste(icon("graduation-cap"), "Improve the tutorial")),
                                                                I(paste(icon("chart-line"), "User graphical interface can be better")),
                                                                I(paste(icon("bug"), "Report a bug or an unexpected system failure")),
                                                                I(paste(icon("exclamation-triangle"), "There is a datum or information wrong")),
                                                                I(paste(icon("question"), "A particular question")),
                                                                I(paste(icon("plus-square"), "None of the list")),
                                                                I(paste(icon("times-circle"), "Don't know the most accurate type (unclasified suggestion)"))
                                                                ))
                                  )),
                      tags$hr(),
                      p("Now, be more accurate about the type"),
                      div(style = "width: 800px", selectInput("suggestionSubtype", "", choices = list(""))),
                      shinyjs::hidden(textInputAddon("newType", "What do you think the type of your suggestion is?", addon = "Suggestion type")),
                      div(style = "display: inline-block;", checkboxInput("allowMail", "Want to receive a mail with the answer")),
                      div(style = "display: inline-block; width: 20px;", tags$br()),
                      div(style = "display: inline-block;", checkboxInput("showMail", "Want us to know your mail")),
                      tags$hr(),
                      p("If you need it, you can upload an image. If you are reporting a bug, please upload an image."),
                      div(style = "width: 800px;",fileInput("imageSuggestion", "Upload an image", accept = c('image/png', 'image/jpeg'))),
                      p("Write your suggestion, be as specific as you want. Any idea, complain or gratitude are welcome."),
                      div(style = "text-align: center;", textAreaInput("suggestionText", label = "", placeholder = "Suggestion", height = "250px", resize = "vertical")),
                      div(style = "float: right;", actionButton("enterSuggestion", div(style = "font-weight: bold", "Submit"), class = "btn-primary", style = "color: #ffffff"))
                  ))
              ),
              column(width = 1)
      ),
      
      # Acknowledgments
      tabItem(tabName = "acknowledgments",
              includeHTML("www/acknowledgments.html")
      ),
      
      # Frequently asked question
      tabItem(tabName = "faq",
              includeHTML("www/faq.html")
      ),
      
      # Saved Table
      tabItem(tabName = "savedTableTab",
              box(title = div(style = "font-weight: bold;", "Information"), width = 3, height = '80vh',
                  helpText("Fast Brenda Searcher saves your last query for 72 hours ",
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
                  actionButton("toEnzyme2", div(style = "font-weight: bold", "Search another enzyme"), class = "btn-danger", style = "color: #ffffff"))),
              box(width = 9, height = '80vh',
                  DTOutput("summarySavedTable"),
                  tags$br(),
                  tags$br(),
                  div(style = "float: right;",
                      actionButton("loadTable", div(style = "font-weight: bold", "Load Selected Table"), class = "btn-primary", style = "color: #ffffff")))
      )
    ),
    div(
      style = "text-align:center;",
      hr(),
      p("~~~~~~~~~~Info~~~~~~~~~~")
    )
  )
)