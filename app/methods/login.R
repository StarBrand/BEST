library(safer)
source("methods/database.R")
source("methods/errors.R")
source("resources/secrets.R")

checkMail <- function(mail, mail2){
  mail == mail2
}

checkPassword <- function(password, password2){
  password == password2
}

save_user <- function(fname, lname, mail, password){
  conn <- connect()
  pass <- encrypt_string(password, key = key)
  sql <- sprintf("SELECT * FROM best_users
                 WHERE mail = '%s';",
                 mail)
  ans <- standardQuery(sql)
  if (nrow(ans) != 0){
    userExists(mail)
    -2L
  } else{
    sql <- sprintf("INSERT INTO best_users(first_name, last_name, mail, pass)
                   VALUES ('%s', '%s', '%s', '%s');",
                   fname, lname, mail, pass)
    tryCatch({
      standardQuery(sql)
      registrationSuccess()
      0L
    }, error = function(cond){
      registrationError(cond)
      -1L
    }, finally = {
      dbDisconnect(conn)
     }
    )
  }
}

check_password <- function(mail, password){
  out <- 0L
  sql <- sprintf("SELECT * FROM best_users WHERE mail = '%s';", mail)
  ans <- standardQuery(sql)
  if (nrow(ans) == 0){
    out <- -2L
  } else {
    pass <- encrypt_string(password, key = key)
    if(pass == ans$pass[1]){
      out <- ans$id[1]
    } else {
      out <- -1L
    }
  }
  out
}
