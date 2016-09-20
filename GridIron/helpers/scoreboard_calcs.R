#Scoreboard calcs

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
  if(CalcDN(data)==1) {
    if(getSIDE(data)=="+") return(min(10,CalcYardLn(data))) else return(10)
  }  else {
    if(getSIDE(data) =="+") return(min(getDIST(data)-getGNL(data),CalcYardLn(data))) else return(getDIST(data)-getGNL(data))
  }
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