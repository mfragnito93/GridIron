#CRUD
CastData <- function(data,type) {
  if(type=="plays"){
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
      OLINE = data["OLINE"],
      OFF_PLAY = data["OFF_PLAY"],
      COVERAGE = data["COVERAGE"],
      BLITZ = data["BLITZ"],
      FRONT = data["FRONT"],
      DEF_PLAY = data["DEF_PLAY"],
      stringsAsFactors = FALSE)
    rownames(datar) <- data["id"]}
  else if(type=="scoreboard"){
    datar <- data.frame(
      id_o = data["id"],
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
      PLAY_TYPE = data["PLAY_TYPE"],
      RESULT = data["RESULT"],
      GN_LS = as.integer(data["GN_LS"]),
      stringsAsFactors = FALSE)
    rownames(datar) <- data["id"]}
  else if(type == "offense"){
    datar <- data.frame(
      id_o = data["id_o"],
      ODK_O = data["ODK_O"],
      OFF_FORM = data["OFF_FORM"],
      OLINE = data["OLINE"],
      OFF_PLAY = data["OFF_PLAY"],
      PERSONNEL = as.integer(data["PERSONNEL"]),
      stringsAsFactors = FALSE)
    rownames(datar) <- data["id_o"]
  } else if(type == "defense"){
    datar <- data.frame(
      id_d = data["id_d"],
      ODK_D = data["ODK_D"],
      DEF_FORM = data["DEF_FORM"],
      DEF_PLAY = data["DEF_PLAY"],
      COVERAGE = data["COVERAGE"],
      BLITZ = data["BLITZ"],
      FRONT = data["FRONT"],
      stringsAsFactors = FALSE)
    rownames(datar) <- data["id_d"]
  }
  return (datar)
}

#returns the deafualt dataframe using castData
CreateDefaultRecord <- function(default,type) {
  mydefault <- CastData(default,type)
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
  updateSelectInput(session, "PLAY_TYPE","PLAY TYPE", choices = c("RUN","PASS","SPECIAL"), selected = unname(data["PLAY_TYPE"]))
  updateSelectInput(session, "RESULT", "RESULT",  choices = c("RUSH","COMPLETE","INCOMPLETE","FUMBLE","INTERCEPTION","SPECIAL"), selected = unname(data["RESULT"]))
  updateTextInput(session, "GN_LS", value = 0)
}

UpdateOffenseTable <- function(data, session){
  updateRadioButtons(session, "ODK_O","SIDE",choices = c("O" = "O","D" = "D"), inline = TRUE,selected = as.character(data["ODK_O"]))
  updateTextInput(session, "id_o", value = unname(data["id_o"]))
  selectList("PERSONNEL","O_PERSONNEL","D_PERSONNEL","ODK_O",data,session,TRUE)
  selectList("OFF_FORM", "O_OFF_FORM", "D_OFF_FORM","ODK_O", data,session,FALSE)
  selectList("OLINE","O_LINE","","ODK_O",data,session,FALSE)
  selectList("OFF_PLAY","O_OFF_PLAY","D_OFF_PLAY", "ODK_O",data, session, FALSE)
}

UpdateDefenseTable <- function(data, session){
  updateRadioButtons(session, "ODK_D","SIDE",choices = c("O" = "O","D" = "D"), inline = TRUE, selected = as.character(data["ODK_D"]))
  updateTextInput(session, "id_d", value = unname(data["id_d"]))
  selectList("DEF_FORM", "O_DEF_FORM", "D_DEF_FORM", "ODK_D",data,session,FALSE)
  selectList("DEF_PLAY","", "D_DEF_PLAY", "ODK_D",data, session, FALSE)
  selectList("COVERAGE", "O_DEF_COVERAGE", "", "ODK_D",data, session, FALSE)
  selectList("BLITZ", "O_DEF_BLITZ", "","ODK_D", data, session, FALSE)
  selectList("FRONT","O_DEF_FRONT", "", "ODK_D",data, session, FALSE)
}

UpdateOffenseForm <- function(odk, session){
  updateTextInput(session, "id_o", value = "0")
  updateRadioButtons(session, "ODK_O","SIDE",choices = c("O" = "O","D" = "D"), inline = TRUE,selected = odk)
  selectListDD("PERSONNEL","O_PERSONNEL","D_PERSONNEL",odk,session,TRUE)
  selectListDD("OFF_FORM", "O_OFF_FORM", "D_OFF_FORM", odk,session,FALSE)
  selectListDD("OLINE", "O_LINE","",odk,session,FALSE)
  selectListDD("OFF_PLAY","O_OFF_PLAY","D_OFF_PLAY", odk, session, FALSE)
}

UpdateDefenseForm <- function(odk, session){
  updateTextInput(session, "id_d", value = "0")
  updateRadioButtons(session, "ODK_D","SIDE",choices = c("O" = "O","D" = "D"), inline = TRUE,selected = odk)
  selectListDD("DEF_FORM", "O_DEF_FORM", "D_DEF_FORM", odk,session,FALSE)
  selectListDD("DEF_PLAY","", "D_DEF_PLAY", odk, session, FALSE)
  selectListDD("COVERAGE", "O_DEF_COVERAGE", "", odk, session, FALSE)
  selectListDD("BLITZ", "O_DEF_BLITZ", "", odk, session, FALSE)
  selectListDD("FRONT","O_DEF_FRONT", "", odk, session, FALSE)
}

UpdateNextPlay <- function(session){
  updateTextInput(session, "n_s_s", "SCOREBOARD", value = GetNextId("scoreboard"))
  updateTextInput(session, "n_s_o", "OFFENSE", value = GetNextId("offense"))
  updateTextInput(session, "n_s_d", "DEFENSE", value = GetNextId("defense"))
  updateTextInput(session, "n_o_s", "SCOREBOARD", value = GetNextId("scoreboard"))
  updateTextInput(session, "n_o_o", "OFFENSE", value = GetNextId("offense"))
  updateTextInput(session, "n_o_d", "DEFENSE", value = GetNextId("defense"))
  updateTextInput(session, "n_d_s", "SCOREBOARD", value = GetNextId("scoreboard"))
  updateTextInput(session, "n_d_o", "OFFENSE", value = GetNextId("offense"))
  updateTextInput(session, "n_d_d", "DEFENSE", value = GetNextId("defense"))
}

updateODK <- function(){
  return(ScoreBoardCalc()[["ODK"]])
}

getDDList <- function(column){
  l<-unique(preSetDDs[,column][preSetDDs[,column]!=""])
  if(is.factor(l)) return(levels(l)) else return(l)
}

getDDByFreq <- function(name){
  l<-as.character(colSort(getTable(GetData(),c(ddMap[name])),"value")[["Var1"]])
  names(l) <- l
  y <- getDDList(name)
  x <- l[names(l) %in% y]
  names(y) <- y
  y <- y[!names(y) %in% x]
  return(c(x,y))
}

selectList <- function(name,column_o,column_d,column_odk,data,session,int = FALSE){
  if(is.null(data)) updateSelectInput(session, name, gsub("_"," ",name), choices = getDDList(column_o) ,selected = NULL) else{
    if(as.character(data[column_odk]) == "O") {
      if(column_o == ""){
      updateSelectInput(session, name, gsub("_"," ",name), choices = c("") ,selected = NULL)
      } else { 
        updateSelectInput(session, name, gsub("_"," ",name), choices = getDDByFreq(column_o) ,selected = if(int) as.integer(data[name]) else unname(data[name]))
        }
    } else { 
      if(column_d == ""){
          updateSelectInput(session, name, gsub("_"," ",name), choices = c("") ,selected = NULL)
         }else{
           updateSelectInput(session, name, gsub("_"," ",name), choices = getDDByFreq(column_d) ,selected = if(int) as.integer(data[name]) else unname(data[name]))
         } 
    }
}}

selectListDD <- function(name,column_o,column_d,odk,session,int = FALSE){
  if(is.null(odk)) updateSelectInput(session, name, gsub("_"," ",name), choices = getDDByFreq(column_o) ,selected = NULL) else{
    if(odk == "O") {
      if(column_o == ""){
        updateSelectInput(session, name, gsub("_"," ",name), choices = c("") ,selected = NULL)
      }else{
        updateSelectInput(session, name, gsub("_"," ",name), choices = getDDByFreq(column_o) ,selected = NULL)
      }
    } else {
      if(column_d==""){
        updateSelectInput(session, name, gsub("_"," ",name), choices = c("") ,selected = NULL)
      }else{
        updateSelectInput(session, name, gsub("_"," ",name), choices = getDDByFreq(column_d) ,selected = NULL)
      }
      
    }
  }}

NullToZero <- function(x){
  if(is.null(x)) return(0) else return(x)
}

GetNextId <- function(type) {
  if (ReadData(type)[1,1]!="TEMP") {
    max(as.integer(ReadData(type)[,idMap[[type]]])) + 1
  } else {
    return (1)
  }
}

#adds row to data frame responses
CreateData <- function(data,type) {
  data <- CastData(data,type)
  rownames(data) <- GetNextId(type)
  data[idMap[[type]]] <- GetNextId(type)
  if(ReadData(type)[,idMap[[type]]][[1]]!="TEMP") EntryToCSV(rbind(ReadData(type),data),type)
  else EntryToCSV(data,type)
}

#output curreent data frame does not return? 
ReadData <- function(type) {
  tmp <- readCSV(dataPathMap[[type]],templatePathMap[[type]])
  if(nrow(tmp)>1) tmp[,1] <- rownames(tmp)
  return(tmp)
}

###
#updates table if there are changes
UpdateData <- function(data,type) {
  data <- CastData(data,type)
  tmp <- ReadData(type)
  tmp[tmp[,idMap[[type]]] == row.names(data), ] <- data
  EntryToCSV(tmp,type)
}

#delete a row name
DeleteData <- function(data,type) {
  EntryToCSV(ReadData(type)[ReadData(type)[,idMap[[type]]] != unname(data[idMap[[type]]]), ],type)
}

#returns friendly meta data
GetMetadata <- function(data) {
  fields <- data
  result <- list(fields = fields)
  return (result)
}

GetMetadataNames <-function(data){
  return(names(GetMetadata(data)$fields))
}

GetMetadataValues <- function(data){
  return(unname(GetMetadata(data)$fields))
}

GetMetaMinusFirstN <- function(data,n){
  return(GetMetadataNames(data)[!GetMetadataNames(data) %in% GetMetaFirstN(data,n)])
}

GetMetaFirstN <- function(data,n){
  return(head(GetMetadataNames(data),n))
}

MakeEntry <- function(data){
  datar<-as.list(data)
  return(datar)
}

UpdateEntry <- function(data){
  return(MakeEntry(data))
}

EntryToCSV<-function(data,type){
  write.csv(data,dataPathMap[[type]],row.names = FALSE)
}

MergeData <- function(idMap,odkMap) {
  tmp <- ReadData(names(idMap)[1])
  for(type in 2:length(names(idMap))){
    tmp<-merge(tmp,select(ReadData(names(idMap)[type]),-which(colnames(ReadData(names(idMap)[type]))==odkMap[names(idMap)[type]])),by =1, sort = FALSE)
  }
  return(tmp)
}

GetData <- function(){
  return(MergeData(idMap,odkMap))
}

ReadScore <- function(){
  return(ReadData("scoreboard"))
}

ReadO <- function(){
  return(ReadData("offense"))
}

ReadD <- function(){
  return(ReadData("defense"))
}

#n here is the number of common columns need for each split. IE 2 for the first two columns of the data should be in each split
DataSplit <- function(data,n = 2){
  o<-data[,c(GetMetaFirstN(scoreboardMeta,n),GetMetaMinusFirstN(offenseMeta,n))]
  colnames(o)[1:n]<-GetMetaFirstN(offenseMeta,n)
  write.csv(o,dataPathMap[["offense"]],row.names = FALSE)
  d<-data[,c(GetMetaFirstN(scoreboardMeta,n),GetMetaMinusFirstN(defenseMeta,n))]
  colnames(d)[1:n]<-GetMetaFirstN(defenseMeta,n)
  write.csv(d,dataPathMap[["defense"]],row.names = FALSE)
  s<-data[,c(GetMetadataNames(scoreboardMeta))]
  write.csv(s,dataPathMap[["scoreboard"]],row.names = FALSE)
}



