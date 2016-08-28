#Creates data frame with designated columns (name,used_shiny,years)
CastData <- function(data) {
  datar <- data.frame(name = data["name"], 
                      used_shiny = as.logical(data["used_shiny"]), 
                      r_num_years = as.integer(data["r_num_years"]),
                      stringsAsFactors = FALSE)
  
  rownames(datar) <- data["id"]
  return (datar)
}

#returns the deafualt dataframe using castData
CreateDefaultRecord <- function() {
  mydefault <- CastData(list(id = "0", name = "", used_shiny = FALSE, r_num_years = 2))
  return (mydefault)
}

#Think this just updates the UI input
UpdateInputs <- function(data, session) {
  updateTextInput(session, "id", value = unname(rownames(data)))
  updateTextInput(session, "name", value = unname(data["name"]))
  updateCheckboxInput(session, "used_shiny", value = as.logical(data["used_shiny"]))
  updateSliderInput(session, "r_num_years", value = as.integer(data["r_num_years"]))
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

#delete a row
DeleteData <- function(data) {
  responses <<- responses[row.names(responses) != unname(data["id"]), ]
}

#returns friendly meta data
GetTableMetadata <- function() {
  fields <- c(id = "Id", 
              name = "Name", 
              used_shiny = "Used Shiny", 
              r_num_years = "R Years")
  result <- list(fields = fields)
  return (result)
}

