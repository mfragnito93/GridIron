#Data API
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


drive <- function(data,drive){
  drive <- if(is.null(drive)) 1 else drive
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

replaceNull <- function(input){
  return(if(is.null(input)) "" else input)
}

defForm <- function(data,form){
  return(filter(data,DEF_FORM == replaceNull(form)))
}

offForm <- function(data,form){
  return(filter(data,OFF_FORM == replaceNull(form)))
}
