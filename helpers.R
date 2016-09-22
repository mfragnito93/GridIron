#Creates data frame with designated columns (name,used_shiny,years)
library(reshape2)
library(plotly)
library(dplyr)

teamColors <- c('#87B5FF','#010014','#D3D3D3','#03002B', '#0C00FF', '#C9C9C9')

tableMeta <- c(OFF_FORM = "OFF FORM",
               PERSONNEL = "PERS", DEF_FORM = "DEF FORM", PLAY_TYPE = "TYPE", 
                RESULT = "RES", GN_LS = "GN LS", OFF_PLAY = "PLAY", COVERAGE = "COVGE",
               BLITZ = "BLTZ", FRONT = "FRONT")

scoreboardMeta <- c(id = "PLAY", ODK = "ODK", QTR ="QTR",DRIVE = "DRIVE", O_SCORE = "O SCORE", OPP_SCORE = "OPP SCORE", DN = "DN", DIST = "DIST", HASH = "HASH",
                YARD_LN = "YDLN", SIDE = "SIDE")

driveSummaryMeta <- c(id ="PLAY", DN="DN",DIST="DST",PERSONNEL="PERS",  OFF_FORM = "OFF FORM", PLAY_TYPE = "TYPE",
                      OFF_PLAY = "PLAY",DEF_FORM = "DEF FORM",COVERAGE = "COVGE",FRONT = "FRONT",BLITZ = "BLTZ")

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
                      RESULT = data["RESULT"],
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
  updateRadioButtons(session, "SIDE","",choices = c("-"="-","+"="+"), selected = as.character(data["SIDE"]))
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
  if(getDIST(data)-getGNL(data)>0) return(getDN(data)+1) else return(1)
}

CalcDN <- function(data){
  if(NextDN(data) == 5) return(1) else return(NextDN(data))
}

CalcDist <- function(data){
  if(CalcDN(data)==1) return(min(10,CalcYardLn(data))) else return(min(getDIST(data)-getGNL(data),CalcYardLn(data)))
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

rpOnly <- function(data){
  return(filter(data, PLAY_TYPE == "RUN" | PLAY_TYPE == "PASS"))
}

getTable <- function(data, x){
  return(melt(table(rpOnly(data)[x])))
}

colSort <- function(data,col,desc = TRUE){
  if(desc) return(data[order(-data[col]),]) else return(data[order(data[col]),]) 
}


getN <- function(data,col,top=TRUE,n=5){
  return(head(colSort(data,col,top),n))
}

plot.bars <- function(data,stack = "group", title = "", xaxis = "", yaxis = "", showLegend = T){
  a <- list(
    title = xaxis
  )
  b <- list(
    title = yaxis
  )
  colnames(data) <- c("group","x","y")
  data$x<-as.factor(data$x)
  data$group <- as.factor(data$group)
  plot_ly(
    data = data,
    x = x,
    y = y,
    color = group,
    colors=teamColors[1:length(unique(data$group))],
    type = "bar") %>% layout(barmode = stack, title = title, showlegend=showLegend, xaxis = a, yaxis = b)
}

plot.oneBar<- function(data,title = "", xaxis = "", yaxis = "", showLegend = F){
  a <- list(
    title = xaxis
  )
  b <- list(
    title = yaxis
  )
  colnames(data) <- c("x","y")
  
  plot_ly(
    data = data,
    x = x,
    y = y,
    color = y,
    type = "bar",
    marker = list(color = c('#010014')) 
  )  %>% layout(title = title, showlegend=showLegend, xaxis = a, yaxis = b)
}

plot.donut <- function(data, title = "", showLegend = F){
  colnames(data) <- c("x","y")
  data$x<-as.factor(data$x)
  plot_ly(
    data = data,
    labels = x,
    values = y,
    marker = list(colors=teamColors[1:length(unique(data$x))]),
    type = "pie", hole = 0.6) %>% layout(showlegend = showLegend, title = title)
}

makeWaterFall <- function(x){
  df<-data.frame()
for (play in 1:length(x$id)){
  df[play,'id'] <- play
  df[play,'start']<-if(x[1,"SIDE"] == "-") as.integer(x[1,"YARD_LN"]) else (50 - as.integer(x[1,"YARD_LN"])) + 50
  df[play,'scrimmage']<-if(play==1) df[play,'start'] else df[play-1,'scrimmage']+df[play-1,'gain']+df[play-1,'loss']
  df[play,'gain']<-if(as.integer(x[play,"GN_LS"])>=0) as.integer(x[play,"GN_LS"]) else 0
  if(as.integer(x[play,"GN_LS"])<0){
    df[play,'loss']<-as.integer(x[play,"GN_LS"]) 
  }  else df[play,'loss'] <- 0
}
  return(melt(select(df,id,gain,loss,scrimmage,-start), id.vars = "id"))
}

plot.drive <- function(data, drive){
a <- list(
  title = ""
)
b <- list(
  title =""
)
plot_ly(
  data = makeWaterFall(drive(data,drive)),
  x = id,
  y = value,
  color = variable,
  colors =c('#ffffff','#87B5FF','#010014'),
  type = "bar") %>% layout(barmode = "stack", title = "Drive Progression", showlegend=F, xaxis = a, yaxis = b)
}


drive <- function(data,drive){
  return(filter(rpOnly(data),DRIVE==as.integer(drive)))
}

driveSummary <- function(data,drive){
  return(select(drive(data,drive),id,DN,DIST,PERSONNEL,OFF_FORM,PLAY_TYPE,OFF_PLAY,DEF_FORM,COVERAGE,FRONT,BLITZ))
}

countFactor <- function(data,col,val){
  return(as.integer(filter(getTable(data, c(col)), Var1 == val)['value']))
}

dnConv <- function(data,down){
  third <- 0
  convert <- 0
  for (drive in unique(data$DRIVE)){
    for (play in 1:length(filter(data,DRIVE==drive)$id)){
      if(filter(data,DRIVE==drive)[play,"DN"]==down){
        third <- third + 1 
        if(filter(data,DRIVE==drive)[min(play+1,length(filter(data,DRIVE==drive)$id)),"DN"]==1){
          convert <- convert + 1 
        } 
      }
    }
  }
  return(convert/third)
}

addDistBucket <- function(data){
  for (play in 1:length(data[,1])){
    dist <- data[play,'DIST']
    data[play,'DIST_BUCKET'] <- if (dist<4) "SHORT" else if (dist<8) "MEDIUM" else if(dist<13) "LONG" else "+12"
  }
  return(data)
}

rpYards <- function(data){
  rp <- data.frame()
  rp[1,"PLAY_TYPE"] <- "RUN"
  rp[2,"PLAY_TYPE"] <- "PASS"
  rp[1,"value"] <- as.integer(sum(filter(data, PLAY_TYPE == "RUN")$GN_LS))
  rp[2,"value"] <- as.integer(sum(filter(data, PLAY_TYPE == "PASS")$GN_LS))
  return(rp)
}

rpYardsAvg <- function(data){
  rp <- data.frame()
  rp[1,"PLAY_TYPE"] <- "RUN"
  rp[2,"PLAY_TYPE"] <- "PASS"
  rp[1,"value"] <- round(mean(filter(data, PLAY_TYPE == "RUN")$GN_LS),2)
  rp[2,"value"] <- round(mean(filter(data, PLAY_TYPE == "PASS")$GN_LS),2)
  return(rp)
}

rpYardsByFactor <- function(data,factor){
  rp <- data.frame()
  i<-1
  for(fact in unique(rpOnly(data)[,factor])){
    rp[i,"PLAY_TYPE"] <- "RUN"
    rp[i+1,"PLAY_TYPE"] <- "PASS"
    rp[i,"group"] <- fact
    rp[i+1,"group"] <- fact
    rp[i,"value"] <- as.integer(sum(data[data[,"PLAY_TYPE"]=="RUN" & data[,factor]==fact,]$GN_LS))
    rp[i+1,"value"] <- as.integer(sum(data[data[,"PLAY_TYPE"]=="PASS" & data[,factor]==fact,]$GN_LS))
    i<-i+2
  }
  return(rp)
}

rpYardsAvgByFactor <- function(data,factor){
  rp <- data.frame()
  i<-1
  for(fact in unique(rpOnly(data)[,factor])){
    rp[i,"PLAY_TYPE"] <- "RUN"
    rp[i+1,"PLAY_TYPE"] <- "PASS"
    rp[i,"group"] <- fact
    rp[i+1,"group"] <- fact
    rp[i,"value"] <- round(mean(data[data[,"PLAY_TYPE"]=="RUN" & data[,factor]==fact,]$GN_LS),2)
    rp[i+1,"value"] <- round(mean(data[data[,"PLAY_TYPE"]=="PASS" & data[,factor]==fact,]$GN_LS),2)
    i<-i+2
  }
  return(rp)
}