library(shinyalert)

# Errors

## Enter Brenda User
noSession <- function(){
  shinyalert("Enter Brenda User",
             "You won't be available to search enzymes if we haven't your Brenda account",
             type = "error")
}

## Wrong password
wrongPassword <- function(){
  shinyalert("Wrong password",
             "Type it again or make sure your password is the same of your Brenda account",
             type = "error")
}

## Not a Brenda Account
noAccount <- function(){
  shinyalert("This is not a Brenda Account",
             "Type it again or register an account on the Brenda page",
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
  m <- s - 6
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