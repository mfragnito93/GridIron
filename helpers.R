#Creates data frame with designated columns (name,used_shiny,years)

tableMeta <- c(OFF_FORM = "OFF FORM",
               PERSONNEL = "PERSONNEL", DEF_FORM = "DEF_FORM", PLAY_TYPE = "PLAY TYPE", 
                RESULT = "RESULT", GN_LS = "GN LS", OFF_PLAY = "OFF PLAY", COVERAGE = "COVERAGE",
               BLITZ = "BLITZ", FRONT = "FRONT")

scoreboardMeta <- c(id = "PLAY", ODK = "ODK", QTR ="QTR",DRIVE = "DRIVE", O_SCORE = "O SCORE", OPP_SCORE = "OPP SCORE", DN = "DN", DIST = "DIST", HASH = "HASH",
                YARD_LN = "YARD LN", SIDE = "SIDE")

meta <- c(scoreboardMeta,tableMeta)

scoreboardDefault <- list(id ="0", ODK = "O", QTR = 1, DRIVE = 1, O_SCORE = 0,
                           OPP_SCORE = 0, DN = 1, DIST = 10,
                           HASH = "M", YARD_LN = 20, SIDE = "-")

tableDefault <- list(OFF_FORM = "", PERSONNEL = 0, DEF_FORM = "", PLAY_TYPE = "", RESULT ="",  GN_LS = 0,  OFF_PLAY = "", COVERAGE = "",
                     BLITZ = "", FRONT = "")

default <- c(scoreboardDefault,tableDefault)

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
                      HASH = data["HASH"],
                      YARD_LN = as.integer(data["YARD_LN"]),
                      SIDE = data["SIDE"],
                      OFF_FORM = data["OFF_FORM"],
                      PERSONNEL = as.integer(data["PERSONNEL"]),
                      DEF_FORM = data["DEF_FORM"],
                      PLAY_TYPE = data["PLAY_TYPE"],
                      RESULT = as.integer(data["RESULT"]),
                      GN_LS = as.integer(data["GN_LS"]),
                      OFF_PLAY = data["OFF_PLAY"],
                      COVERAGE = data["COVERAGE"],
                      BLITZ = data["BLITZ"],
                      FRONT = data["FRONT"],
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
  print(data)
  updateTextInput(session, "id", value = unname(data["id"]))
  updateRadioButtons(session, "ODK","",inline = TRUE, choices = c("OSIDE" = "O","OPP" = "D"), selected = as.character(data["ODK"]))
  updateNumericInput(session, "DRIVE", value = as.integer(data["DRIVE"]))
  updateNumericInput(session, "O_SCORE", value = as.integer(data["O_SCORE"]))
  updateNumericInput(session, "QTR", value = as.integer(data["QTR"]))
  updateNumericInput(session, "OPP_SCORE", value = as.integer(data["OPP_SCORE"]))
  updateNumericInput(session, "DN", value = as.integer(data["DN"]))
  updateNumericInput(session, "DIST", value = as.integer(data["DIST"]))
  updateNumericInput(session, "YARD_LN", value = as.integer(data["YARD_LN"]))
  updateTextInput(session, "HASH", value = unname(data["HASH"]))
  updateRadioButtons(session, "SIDE","Side",choices = c("-"="-","+"="+"), selected = as.character(data["SIDE"]))
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

ScoreBoardCalc <- function(){
  if (exists("responses")) {
  lastPlay <- tail(ReadData(),1)
  nextPlay <- lastPlay
  nextPlay["DN"]<-CalcDN(lastPlay)
  nextPlay["DIST"]<-CalcDist(lastPlay)
  nextPlay["GN_LS"]<-as.integer(0)
  nextPlay["YARD_LN"]<-CalcYardLn(lastPlay)
  nextPlay["SIDE"]<-CalcSide(lastPlay)
  nextPlay["ODK"]<-CalcODK(lastPlay)
  nextPlay["DRIVE"]<-CalcDrive(lastPlay)
  nextPlay["id"]<-"0"
  return(nextPlay)}else return(CreateDefaultRecord())
}

getDN <- function(data){
  as.integer(data["DN"])
}

getDIST <- function(data){
  as.integer(data["DIST"])
}

getYLN <- function(data){
  as.integer(data["YARD_LN"])
}

getSIDE <- function(data){
  data["SIDE"]
}

getDRIVE <- function(data){
  as.integer(data["DRIVE"])
}

getODK <- function(data){
  data["ODK"]
}

getGNL <- function(data){
  as.integer(data["GN_LS"])
}

getSCORE <- function(data,team){
  as.integer(data[team])
}

NextDN <- function(data){
  if(getDIST(data)-getGNL(data)>=0) return(getDN(data)+1) else return(1)
}

CalcDN <- function(data){
  if(NextDN(data) == 5) return(1) else return(NextDN(data))
}

CalcDist <- function(data){
  if(CalcDN(data)==1) return(10) else return(getDIST(data)-getGNL(data))
}

NextYardLn <- function(data){
  if(getSIDE(data) == "+") return(getYLN(data)-getGNL(data)) else return(getYLN(data)+getGNL(data))
}

CalcYardLn <- function(data){
  if(NextYardLn(data)>50) return(50+(50-NextYardLn(data))) else return(max(NextYardLn(data),0))
}

CalcSide <- function(data){
  if(NextYardLn(data)>50 || NextDN(data) == 5 || CalcYardLn(data) == 0) {
    if(getSIDE(data) == "+") return("-") else return("+")
  } else return(getSIDE(data))
}

CalcDrive <- function(data){
  if(NextDN(data) == 5) return(getDRIVE(data)+1) else return(getDRIVE(data))
}

CalcODK <- function(data){
  if(CalcYardLn(data) == 0 || NextDN(data) == 5) {
    if(getODK(data)== "O") return("D") else return("O")
  } else return(getODK(data))
}

