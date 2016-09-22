#CRUD
CastData <- function(data) {
  datar <- data.frame(
    id = data["id"],
    ODK = data["ODK"], 
    QTR = as.integer(data["QTR"]),
    DRIVE = as.integer(data["DRIVE"]),
    O_SCORE = as.integer(data["O_SCORE"]),
    OPP_SCORE = as.integer(data["OPP_SCORE"]),
    DN = as.integer(data["DN"]),
    DIST = as.integer(data["DIST"]),
    YARD_LN = as.integer(data["YARD_LN"]),
    SIDE = data["SIDE"],
    HASH = data["HASH"],
    OFF_FORM = data["OFF_FORM"],
    PERSONNEL = as.integer(data["PERSONNEL"]),
    DEF_FORM = data["DEF_FORM"],
    PLAY_TYPE = data["PLAY_TYPE"],
    RESULT = data["RESULT"],
    GN_LS = as.integer(data["GN_LS"]),
    OFF_PLAY = data["OFF_PLAY"],
    COVERAGE = data["COVERAGE"],
    BLITZ = data["BLITZ"],
    FRONT = data["FRONT"],
    DEF_PLAY = data["DEF_PLAY"],
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
UpdateScoreboard <- function(data, session) {
  updateTextInput(session, "id", value = unname(data["id"]))
  updateRadioButtons(session, "ODK","SIDE",choices = c("O" = "O","D" = "D"), selected = as.character(data["ODK"]))
  updateNumericInput(session, "DRIVE", value = as.integer(data["DRIVE"]))
  updateNumericInput(session, "O_SCORE", value = as.integer(data["O_SCORE"]))
  updateSelectInput(session, "QTR", "QTR", choices = c(1,2,3,4,5), selected = as.integer(data["QTR"]))
  updateNumericInput(session, "OPP_SCORE", value = as.integer(data["OPP_SCORE"]))
  updateSelectInput(session, "DN", "DN", choices = c(1,2,3,4), selected = as.integer(data["DN"]))
  updateNumericInput(session, "DIST", value = as.integer(data["DIST"]))
  updateNumericInput(session, "YARD_LN", value = as.integer(data["YARD_LN"]))
  updateRadioButtons(session, "SIDE","",choices = c("-"="-","+"="+"), selected = as.character(data["SIDE"]))
  updateSelectInput(session, "HASH","HASH", choices = c("L","M","R"), selected = unname(data["HASH"]))
}

UpdateTable <- function(data, session){
  selectList("PERSONNEL","O_PERSONNEL","D_PERSONNEL",data,session,TRUE)
  selectList("OFF_FORM", "O_OFF_FORM", "D_OFF_FORM", data,session,FALSE)
  selectList("DEF_FORM", "O_DEF_FORM", "D_DEF_FORM", data,session,FALSE)
  updateSelectInput(session, "PLAY_TYPE","PLAY TYPE", choices = c("RUN","PASS","SPECIAL"), selected = unname(data["PLAY_TYPE"]))
  updateSelectInput(session, "RESULT", "RESULT",  choices = c("RUSH","COMPLETE","INCOMPLETE","FUMBLE","INTERCEPTION","SPECIAL"), selected = unname(data["RESULT"]))
  updateTextInput(session, "GN_LS", value = unname(data["GN_LS"]))
  selectList("OFF_PLAY","O_OFF_PLAY","D_OFF_PLAY", data, session, FALSE)
  selectList("DEF_PLAY","", "D_DEF_PLAY", data, session, FALSE)
  selectList("COVERAGE", "O_DEF_COVERAGE", "", data, session, FALSE)
  selectList("BLITZ", "O_DEF_BLITZ", "", data, session, FALSE)
  selectList("FRONT","O_DEF_FRONT", "", data, session, FALSE)
}

UpdateForm <- function(odk, session){
  selectListDD("PERSONNEL","O_PERSONNEL","D_PERSONNEL",odk,session,TRUE)
  selectListDD("OFF_FORM", "O_OFF_FORM", "D_OFF_FORM", odk,session,FALSE)
  selectListDD("DEF_FORM", "O_DEF_FORM", "D_DEF_FORM", odk,session,FALSE)
  selectListDD("OFF_PLAY","O_OFF_PLAY","D_OFF_PLAY", odk, session, FALSE)
  selectListDD("DEF_PLAY","", "D_DEF_PLAY", odk, session, FALSE)
  selectListDD("COVERAGE", "O_DEF_COVERAGE", "", odk, session, FALSE)
  selectListDD("BLITZ", "O_DEF_BLITZ", "", odk, session, FALSE)
  selectListDD("FRONT","O_DEF_FRONT", "", odk, session, FALSE)
}

getDDList <- function(column){
  l<-unique(preSetDDs[,column][preSetDDs[,column]!=""])
  if(is.factor(l)) return(levels(l)) else return(l)
}


selectList <- function(name,column_o,column_d,data,session,int = FALSE){
  if(is.null(data)) updateSelectInput(session, name, gsub("_"," ",name), choices = getDDList(column_o) ,selected = NULL) else{
    if(as.character(data["ODK"]) == "O") {
      if(column_o == ""){
      updateSelectInput(session, name, gsub("_"," ",name), choices = c("") ,selected = NULL)
      } else { 
        updateSelectInput(session, name, gsub("_"," ",name), choices = getDDList(column_o) ,selected = if(int) as.integer(data[name]) else unname(data[name]))
        }
    } else { 
      if(column_d == ""){
          updateSelectInput(session, name, gsub("_"," ",name), choices = c("") ,selected = NULL)
         }else{
           updateSelectInput(session, name, gsub("_"," ",name), choices = getDDList(column_d) ,selected = if(int) as.integer(data[name]) else unname(data[name]))
         } 
    }
}}

selectListDD <- function(name,column_o,column_d,odk,session,int = FALSE){
  if(is.null(odk)) updateSelectInput(session, name, gsub("_"," ",name), choices = getDDList(column_o) ,selected = NULL) else{
    if(odk == "O") {
      if(column_o == ""){
        updateSelectInput(session, name, gsub("_"," ",name), choices = c("") ,selected = NULL)
      }else{
        updateSelectInput(session, name, gsub("_"," ",name), choices = getDDList(column_o) ,selected = NULL)
      }
    } else {
      if(column_d==""){
        updateSelectInput(session, name, gsub("_"," ",name), choices = c("") ,selected = NULL)
      }else{
        updateSelectInput(session, name, gsub("_"," ",name), choices = getDDList(column_d) ,selected = NULL)
      }
      
    }
  }}

NullToZero <- function(x){
  if(is.null(x)) return(0) else return(x)
}

GetNextId <- function() {
  if (exists("responses") && NullToZero(nrow(responses)) > 0) {
    max(as.integer(rownames(responses))) + 1
  } else {
    return (1)
  }
}

#adds row to data frame responses
CreateData <- function(data) {
  data <- CastData(data)
  rownames(data) <- GetNextId()
  data["id"] <- GetNextId()
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
GetMetadata <- function(data) {
  fields <- data
  result <- list(fields = fields)
  return (result)
}


MakeEntry <- function(data){
  datar<-as.list(data)
  return(datar)
}

UpdateEntry <- function(data){
  return(MakeEntry(data))
}