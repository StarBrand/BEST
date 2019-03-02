#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
  dashboardPage(skin = "green",
                dashboardHeader(title = "Title"),
                dashboardSidebar(
                  sidebarMenu(
                    id = "inTabset",
                    menuItem("Tutorial", tabName = "help"),
                    menuItem("Acknowledgments", tabName = "acknowledgments"),
                    menuItem("FAQ", tabName = "faq")
                  )
                ),
                dashboardBody(
                  tabItems(
                    tabItem(tabName = "help",
                            box(width = 3, height = '90vh',
                                div(style='height:80vh; overflow-y: scroll',
                                tags$li(tags$a(href = "#1", "Get started"),
                                        tags$ul(tags$li(tags$a(href = "#1_1", "Enzyme query"),
                                                        tags$ul(tags$li(tags$a(href = "#1_1_1", "EC Number")),
                                                                tags$li(tags$a(href = "#1_1_2", "Enzyme name"))
                                                                ))
                                                ),
                                        tags$ul(tags$li(tags$a(href = "#1_2", "Before the query"),
                                                        tags$ul(tags$li(tags$a(href = "#1_2_1", "Add enzymes to the query")),
                                                                tags$li(tags$a(href = "#1_2_2", "Erase the query")),
                                                                tags$li(tags$a(href = "#1_2_3", "Unavoidable errors"))
                                                                ))
                                                )
                                        ),
                                tags$li(tags$a(href = "#2", "Queries from the organism table"),
                                        tags$ul(tags$li(tags$a(href = "#2_1", "Get PDB")),
                                                tags$li(tags$a(href = "#2_2", "Get amino acids sequence")),
                                                tags$li(tags$a(href = "#2_3", "Get functional Parameters"))
                                                )
                                        ),
                                tags$li(tags$a(href = "#3", "Functional Parameters Query"),
                                        tags$ul(tags$li(tags$a(href = "#3_1", "Selecting parameters")),
                                                tags$li(tags$a(href = "#3_2", "Selecting by phylogeny")),
                                                tags$li(tags$a(href = "#3_3", "Other options of query")),
                                                tags$li(tags$a(href = "#3_4", "Filters"))
                                                )
                                        ),
                                tags$li(tags$a(href = "#4", "Summary table and quick access"),
                                        tags$ul(tags$li(tags$a(href = "#4_1", "Summary table and where to find it")),
                                                tags$li(tags$a(href = "#4_2", "Quick access toolbar"))
                                                )
                                        ),
                                tags$li(tags$a(href = "#5", "Visualize"),
                                        tags$ul(tags$li(tags$a(href = "#5_1", "Distribution as boxplot")),
                                                tags$li(tags$a(href = "#5_2", "Correlation"),
                                                        tags$ul(tags$li(tags$a(href = "#5_2_1", "Correlation as a heatmap matrix")),
                                                                tags$li(tags$a(href = "#5_2_1", "Correlation as a paired scatter plot"))
                                                                ))
                                                )
                                        ),
                                tags$li(tags$a(href = "#6", "Cluster"),
                                        tags$ul(tags$li(tags$a(href = "6_1", "K-means")))
                                        ),
                                tags$li(tags$a(href = "#7", "External tools")
                                        ),
                                tags$li(tags$a(href = "#8", "Saved tables")
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
                                          tags$a("Brenda web page", href = "www.brenda-enzymes.org/"),
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
                                        ),
                                    div(id = "1_2", class = "seccion level2",
                                        h2("Before the query"),
                                        p("At this point, a table showing all the organism that has a protein with the same catalytic function define in the ",
                                          "ec number, available in Brenda is generated. This table contains the next columns that depend on the enzyme type: ",
                                          "ec number, systematic name (this could be empty) and recommended name; and the next columns that depend on the organism: ",
                                          "Uniprot, organism (scientific name) and commentary. This last one is hidden by default, but can be shown selecting the ",
                                          "\"show the comentary column\" checkbox."),
                                        img(src = 'Tutorial(6).jpg',
                                            width = '500px',
                                            style = "display: block; margin-left: auto; margin-right: auto;"),
                                        p("The way to proceed after this point is described in ", tags$a("\"Queries from the organsm table\" section", href = "#2"),
                                          ". In this section, the optional proceeds to have a complex query are described.")
                                        ),
                                    div(id = "1_2_1", class = "seccion level3",
                                        h3("Add Enzymes to the query"),
                                        p("If more than just one enzyme type is needed, you can add another ecnumber to the query. This proccess can be done virtually ",
                                          "undefinely. To add ec numbers to your organism table (or protein table) look for the \"Add proteins\" button in the side bar ",
                                          "panel."),
                                        img(src = 'Tutorial(7).jpg',
                                            width = '250px',
                                            style = "display: block; margin-left: auto; margin-right: auto;"),
                                        p("After clicking it, you are going to be back at the",
                                          actionLink("enzymeHelp2", "enzyme section"),
                                          ", make sure the welcome panel in the top of the page ",
                                          "(just below the toolbar) says \"",icon("exclamation-triangle"),"The enzyme your enter will be added to the search\"",
                                          "if this is not the case, please go back to the Protein table seccion (by dashboard menu or by the quick access button in ",
                                          "toolbar showed below) and press the \"Add proteins\" button again."),
                                        img(src = 'Tutorial(8).jpg',
                                            width = '500px',
                                            style = "display: block; margin-left: auto; margin-right: auto;"),
                                        p("If all is set, add enzymes to the query ", tags$a("the same way the first ones were entered.", href = "#1_1"))
                                        ),
                                    div(id = "1_2_2", class = "seccion level3",
                                        h3("Erase the query"),
                                        p("If you made a mistake and the ec number enter to the query is not the one you want to. You can erase this query and ",
                                          "look for another one by clicking the \"Go back and search another Enzyme\" in the top//right corner."),
                                        img(src = 'Tutorial(9).jpg',
                                            width = '250px',
                                            style = "display: block; margin-left: auto; margin-right: auto;"),
                                        p("In this case, the welcome panel should show: "),
                                        img(src = 'Tutorial(10).jpg',
                                            width = '500px',
                                            style = "display: block; margin-left: auto; margin-right: auto;"),
                                        p("Sadly, if you had more than one enzymes in the query, all of them are going to be erased.",
                                          div(style = "display: inline-block;font-weight: bold;",
                                              "There is no way to just erase the last one.")),
                                        p("Log out has the same effect. This is going to erase everything generated in the site. But, this has the advantage that ",
                                          "specific tables is going to be", tags$a("saved", href = "#8")),
                                        img(src = 'Tutorial(11).jpg',
                                            width = '350px',
                                            style = "display: block; margin-left: auto; margin-right: auto;")
                                        ),
                                    div(id = "1_2_3", class = "seccion level3",
                                        h2("Unavoidable errors"),
                                        p("This is an app on develpment...")
                                        ),
                                    div(id = "2", class = "seccion level1",
                                        h1("Queries from the organism table"),
                                        p("After the organism table ",
                                          "(", actionLink("proteinHelp", "Protein Table"),
                                          ") is generated, it can be used to search for ", tags$a("numerical parameters", href = "#3"),
                                          ", ", tags$a("amino acids sequence", href = "#2_2"), " or ", tags$a("PDB codes", href = "#2_1"),
                                          " with their respective link to the ", tags$a("RCSB PDB", href = "https://www.rcsb.org/#Category-learn"),
                                          " (The Protein Data Bank of the Research Collaboratory for Structural Bionformatics)."),
                                        p("If you are interested in just one of this queries, you can skip the other in this tutorial. They are not",
                                          "depended to each other.")
                                        ),
                                    div(id = "2_1", class = "seccion level2",
                                        h2("Get PDB"),
                                        p("Generete a table with the PDB codes and their links to the ", tags$a("RCSB PDB", href = "https://www.rcsb.org/#Category-learn"),
                                          " is done by clicking the \"Get PDB\" button above the ",
                                          actionLink("proteinHelp2", "Protein Table"), " as show below."),
                                        img(src = 'Tutorial(12).jpg',
                                            width = '350px',
                                            style = "display: block; margin-left: auto; margin-right: auto;"),
                                        p("The generated table looks like this"),
                                        img(src = 'Tutorial(13).jpg',
                                            width = '500px',
                                            style = "display: block; margin-left: auto; margin-right: auto;"),
                                        p("The columns show the ec number, the organism, the PDB code and the link. If you click any of this link, you are going to ",
                                          "be redirected to the PDB site with an image like this"),
                                        img(src = 'Tutorial(14).jpg',
                                            width = '500px',
                                            style = "display: block; margin-left: auto; margin-right: auto;"),
                                        p("Also, you can download the whole table clicking the blue button, or return to the Protein table section clicking the orange one")
                                        ),
                                    div(id = "2_2", class = "seccion level2",
                                        h2("Get amino acids sequence"),
                                        p("The uniprot code that some proteins have, are a distintive code provides by the ",
                                          tags$a("Uniprot database", href = "https://www.uniprot.org/help/"),
                                          ", that a particular enzyme has. With this code you can access to the amino acids sequence of the protein. Brenda ",
                                          "provides this code and the sequence with it. By clicking the \"Get sequence\" blue button, as show below, a table ",
                                          "reporting the sequence found by BRENDA are going to be shown."),
                                        img(src = 'Tutorial(15).jpg',
                                            width = '250px',
                                            style = "display: block; margin-left: auto; margin-right: auto;"),
                                        p("This table look like this"),
                                        img(src = 'Tutorial(16).jpg',
                                            width = '500px',
                                            style = "display: block; margin-left: auto; margin-right: auto;"),
                                        p("You can access to the sequence code by download a .txt file clicking the blue button. The sequence to be downloaded can ",
                                          "be selected in the report table clicking the respective row or download all the found sequence switching the \"Download ",
                                          "all the list\" button in the sidebar.")
                                        ),
                                    div(id = "2_3", class = "seccion level2",
                                        h2("Get functional parameters"),
                                        p("The complete functional parameters query tutorial is in the ",
                                          tags$a("next section", href = "#3"), ". However, to generete a table with the 12 numerical parameters available in Brenda ",
                                          "for every protein in the Protein table, you have to activate the \"Select all functional parameters\" switch on the top ",
                                          "of sidebar, let the \"Search for all parameters\" switch activated above the table and click the \"Search for parameters\" ",
                                          "green button at the end of the sidebar."),
                                        img(src = 'Tutorial(17).jpg',
                                            width = '400px',
                                            style = "display: block; margin-left: auto; margin-right: auto;")
                                        ),
                                    div(id = "3", class = "seccion level1",
                                        h1("Functional Parameters Query")
                                        ),
                                    div(id = "3_1", class = "seccion level2",
                                        h2("Selecting parameters")
                                        ),
                                    div(id = "3_2", class = "seccion level2",
                                        h2("Selecting by phylogeny")
                                        ),
                                    div(id = "3_3", class = "seccion level2",
                                        h2("Other options of query")
                                        ),
                                    div(id = "3_4", class = "seccion level2",
                                        h2("Filters")
                                        ),
                                    div(id = "4", class = "seccion level1",
                                        h1("Summary table and quick access"),
                                        p("This section describes 2 tools that are available at any moment in the app.")
                                        ),
                                    div(id = "4_1", class = "seccion level2",
                                        h2("Summary table and where to find it"),
                                        p("It is possible to generate a table showing how many attributes of each protein (differentating the ec number ",
                                          "and the organism) have been found."),
                                        img(src = 'Tutorial(18).jpg',
                                            width = '500px',
                                            style = "display: block; margin-left: auto; margin-right: auto;"),
                                        p("You can access this table by two ways. In the dashboard menu, located in the \"Protein Search\" seccion, or, ",
                                          "by clicking the direct access toolbar with the ", icon("table"), " icon. Once in the \"Available Information\" seccion ",
                                          "click the \"Refresh\" blue button to generate or update."),
                                        img(src = 'Tutorial(19).jpg',
                                            width = '350px',
                                            style = "display: block; margin-left: auto; margin-right: auto;")
                                        ),
                                    div(id = "4_2", class = "seccion level2",
                                        h2("Quick access toolbar"),
                                        p("At the right\\top corner there are four buttons in the toolbar. These buttons, called quick access, allow you to ",
                                          "change the current seccion to:"),
                                        tags$br(),
                                        p(icon("question"), " = ", "Tutorial seccion"),
                                        img(src = 'Tutorial(20).jpg',
                                            width = '100px',
                                            style = "display: block; margin-left: auto; margin-right: auto;"),
                                        tags$br(),
                                        p(icon("asterisk"), " = ", "Protein table"),
                                        img(src = 'Tutorial(21).jpg',
                                            width = '100px',
                                            style = "display: block; margin-left: auto; margin-right: auto;"),
                                        tags$br(),
                                        p(icon("table"), " = ", "Summary table"),
                                        img(src = 'Tutorial(22).jpg',
                                            width = '100px',
                                            style = "display: block; margin-left: auto; margin-right: auto;"),
                                        tags$br(),
                                        p(icon("user"), " = ", "Principal page to log in or log out"),
                                        img(src = 'Tutorial(23).jpg',
                                            width = '100px',
                                            style = "display: block; margin-left: auto; margin-right: auto;")
                                        ),
                                    div(id = "5", class = "seccion level1",
                                        h1("Visualize")
                                        ),
                                    div(id = "5_1", class = "seccion level2",
                                        h2("Distribution as boxplot")
                                        ),
                                    div(id = "5_2", class = "seccion level2",
                                        h2("Correlation")
                                        ),
                                    div(id = "5_2_1", class = "seccion level3",
                                        h3("Correlation as heatmap matrix")
                                        ),
                                    div(id = "5_2_2", class = "seccion level3",
                                        h3("Correlation as a paired scatter plot")
                                        ),
                                    div(id = "6", class = "seccion level1",
                                        h2("Cluster")
                                        ),
                                    div(id = "6_1", class = "seccion level2",
                                        h2("K-means")
                                        ),
                                    div(id = "7", class = "seccion level1",
                                        h1("External tools")
                                        ),
                                    div(id = "8", class = "seccion level1",
                                        h1("Saved tables")
                                        )
                                    ))
                    ),
                    tabItem(
                      tabName = "acknowledgments",
                      box(width = 3, height = '90vh'
                          
                      ),
                      box(width = 9, height = '90vh'
                              
                      )
                    ),
                    tabItem(
                      tabName = "faq",
                      box(width = 3, height = '90vh'
                          
                      ),
                      box(width = 9, height = '90vh'
                          
                      )
                    )
                    
                    )
                  )
  )
   
)

# Define server logic required to draw a histogram
server <- function(session, input, output) {
   
}

# Run the application 
shinyApp(ui = ui, server = server)

