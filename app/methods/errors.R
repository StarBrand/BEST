library(shinyalert)
library(shinyWidgets)

# Errors

workingOnIt <- function(){
  shinyalert("App Under Maintenance",
             paste(
               "Due to changes on architecture, we are currently",
               "developing the app. Enzyme are being populated on database. It's going",
               "to take a while, but it will work faster. Enzymes available are indefined",
               "while this process is being done.",
               "We apologize for the inconvience."),
             type = "warning")
}

# Anonymous User
enterAnonymously <- function(session){
  confirmSweetAlert(session = session,
                    inputId = "enterAnonymous",
                    type = "primary",
                    title = "You have to accept licence",
                    text = helpText("By continue as an anonymous user, you accept ",
                                    tags$a("our GNU licence.", href = "https://github.com/StarBrand/BEST/blob/master/LICENSE.md", target = "_blank"),
                                    "Also, our BEST app is based on enzymes' information obtained from ",
                                    tags$a("Brenda database,", href = "https://www.brenda-enzymes.org/index.php", target = "_blank"),
                                    "we acknowledge their work by adding their licence as well. Their license ",
                                    "is available ", tags$a("here.", href = "https://www.brenda-enzymes.org/copy.php", target = "_blank"),
                                    "By clicking below (on Accept both licence) you declare that you read the licences terms and accept them both."),
                    btn_labels = c("Cancel", "Accept both licence"),
                    danger_mode = FALSE)
}

## Registration

### Required fields
isRequired <- function(what){
  shinyalert("Missing required field",
             sprintf("%s is required", what),
             type = "error")
}

### Match
doesNotMatch <- function(what){
  shinyalert(sprintf("%s does not match", what),
             sprintf("%s and Confirm %s must be the same", what, what),
             type = "error")
}

#### Mail
mailDoesNotMatch <- function(){
  doesNotMatch("Mail")
}

#### Password
passDoesNotMatch <- function(){
  doesNotMatch("Password")
}

### Save user
registrationSuccess <- function(){
  shinyalert("User register, you can now log in and save your progress",
             type = "success")
}

userExists <-function(mail){
  shinyalert("You already have an account",
             sprintf("%s is already registered", mail),
             type = "error")
}

registrationError <-function(cond){
  shinyalert(sprintf("Error on registration:\n%s", conditionMessage(cond)),
             type = "error")
}

## Enter Brenda User
noSession <- function(){
  shinyalert("Accept Licence",
             "You won't be available to search enzymes if you haven't accept the licence",
             type = "error")
}

## Wrong password
wrongPassword <- function(){
  shinyalert("Wrong password",
             "Not the password of this user",
             type = "error")
}

## Not a Brenda Account
noAccount <- function(){
  shinyalert("This is mail doesn't have an account",
             "Type it again or register an account",
             type = "error")
}

## Make an enzyme query
noSearch <- function(what){
  shinyalert(paste(what, " of what?", sep = ""),
             paste("We need a list of enzymes (or at least one) to look for ",
                   what,
                   sep = ""),
             type = "error")
}

## Java has failed
javaError <- function(what, session){
  confirmSweetAlert(session = session,
                    inputId = "toReportBug",
                    type = "error",
                    title = paste("An error has happen on",
                                  what, "search"),
                    text = helpText("This error isn't suppose to happen, ",
                                    "try again and if this error persist ",
                                    "you can reported on the Suggestion section. ",
                                    "Please, when reported indicate all the entered input and ",
                                    "as many datails as possible"),
                    btn_labels = c("Try again", "Report this bug"),
                    danger_mode = TRUE)
}

## Handled
handledJavaError <- function(session, what){
  confirmSweetAlert(session = session,
                    inputId = paste("handlerJavaE", what, sep = ""),
                    type = "warning",
                    title = "Want to continue?",
                    text = helpText("A SOAP issue happened. Do you want to try again?"),
                    btn_labels = c("Cancel", "Retry"),
                    danger_mode = TRUE)
}

## Cannot add
noProteins <- function(){
  shinyalert("Cannot add",
             "It is not necessary to add enzymes, because you are not enter yet",
             type = "error")
}

## Make functional parameters query
noParameters <- function(art, what){
  shinyalert(paste(what, "of what?"),
             paste("To do", art, what, ", a functional parameter query must be done"),
             type = "error")
}

## Wrong number of parameter selected
### k-means
kmeansError <- function(s){
  n <- 2 - s
  m <- abs(3 - s)
  if(n < 0){con2 <- "deselect"
  } else {con2 <- "select"}
  if(m == 1){con3 <- "parameter"
  } else {con3 <- "parameters"}
  shinyalert("Wrong number of parameter selected",
             paste("The visualization takes each parameter as a dimension, therefore, you need to",
                   con2, abs(n), "(for a two dimensional clustering) or ",
                   m, con3, "(for a three dimensional one)"),
             type = "error")
}
### Paired scatter plot
corScatterError <- function(s){
  n <- 2 - s
  m <- s - 4
  if(m == 1){nparameters = "parameter"
  } else{nparameters = "parameters"}
  if(n == 1){nparameters = "one parameter more"
  } else if(n == 2){nparameters = "two parameters"}
  if(n < 0){aMessage <- paste("Too many parameters are selected, due to storage issues, please, deselect",
                              abs(m), nparameters, "to show the correlation")
  } else {aMessage <- paste("There are not enough parameter to do a correlation, select at least",
                            nparameters, "to show the correlation")}
  shinyalert("Wrong number of parameter selected",
             aMessage,
             type = "error")
}

## Copy to Clipboard error
copyAAError <- function(){
  shinyalert("Invalid sequence selection",
             "ONE selected must be selected to be able to copy to clipboard",
             type = "error")
}

standardError <- function(what, detail){
  shinyalert(sprintf("Error while doing %s", what),
             detail,
             type = "error")
}
