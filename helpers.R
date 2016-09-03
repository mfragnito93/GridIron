#Creates data frame with designated columns (name,used_shiny,years)

meta <- c(id ="Id", ODK = "ODK", QTR = "QTR", O_SCORE = "O SCORE",
          OPP_SCORE = "OPP SCORE", DN = "DN", DIST = "DIST",
          HASH = "HASH", YARD_LN = "YARD LN", OFF_FORM = "OFF FORM",
          PERSONNEL = "PERSONNEL", PLAY_TYPE = "PLAY TYPE", 
          OFF_PLAY = "OFF PLAY", GN_LS = "GN LS")

default <- list(id ="0", ODK = "", QTR = 1, O_SCORE = 0,
             OPP_SCORE = 0, DN = 1, DIST = 10,
             HASH = "M", YARD_LN = 20, OFF_FORM = "",
             PERSONNEL = 0, PLAY_TYPE = "",
             OFF_PLAY = "", GN_LS = 0)

CastData <- function(data) {
  datar <- data.frame(ODK = data["ODK"], 
                      QTR = as.integer(data["QTR"]),
                      O_SCORE = as.integer(data["O_SCORE"]),
                      OPP_SCORE = as.integer(data["OPP_SCORE"]),
                      DN = as.integer(data["DN"]),
                      DIST = as.integer(data["DIST"]),
                      HASH = data["HASH"],
                      YARD_LN = as.integer(data["YARD_LN"]),
                      OFF_FORM = data["OFF_FORM"],
                      PERSONNEL = as.integer(data["PERSONNEL"]),
                      PLAY_TYPE = data["PLAY_TYPE"],
                      OFF_PLAY = data["OFF_PLAY"],
                      GN_LS = as.integer(data["GN_LS"]),
                      stringsAsFactors = FALSE)
  
  rownames(datar) <- data["id"]
  return (datar)
}

#returns the deafualt dataframe using castData
CreateDefaultRecord <- function() {
  mydefault <- CastData(default)
  return (mydefault)
}

#Think this just updates the UI input
UpdateID <- function(data, session) {
  updateTextInput(session, "id", value = unname(rownames(data)))
  # updateTextInput(session, "name", value = unname(data["name"]))
  # updateCheckboxInput(session, "used_shiny", value = as.logical(data["used_shiny"]))
  # updateSliderInput(session, "r_num_years", value = as.integer(data["r_num_years"]))
}


GetNextId <- function() {
  if (exists("responses") && nrow(responses) > 0) {
    max(as.integer(rownames(responses))) + 1
  } else {
    return (1)
  }
}

#adds row to data frame responses
CreateData <- function(data) {
  data <- CastData(data)
  rownames(data) <- GetNextId()
  if (exists("responses")) {
    responses <<- rbind(responses, data)
  } else {
    responses <<- data
  }
}

#output curreent data frame does not return? 
ReadData <- function() {
  if (exists("responses")) {
    responses
  }
}

#updates table if there are changes
UpdateData <- function(data) {
  data <- CastData(data)
  responses[row.names(responses) == row.names(data), ] <<- data
}

#delete a row name
DeleteData <- function(data) {
  responses <<- responses[row.names(responses) != unname(data["id"]), ]
}

#returns friendly meta data
GetTableMetadata <- function() {
  fields <- meta
  result <- list(fields = fields)
  return (result)
}

MakeEntry <- function(data,id){
  datar<-as.list(data)
  datar["id"] <- id
  return(datar)
}

UpdateEntry <- function(data){
  return(MakeEntry(data))
}
