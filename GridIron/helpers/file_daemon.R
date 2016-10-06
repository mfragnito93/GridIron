#file functions 
readCSV<- function(path,backup){
  tryCatch({if(nrow(read.csv(path, stringsAsFactors = FALSE))>0) return(read.csv(path, stringsAsFactors = FALSE)) else{
    tmp <- read.csv(backup, stringsAsFactors = FALSE)
    write.csv(tmp,path,row.names = FALSE)
    return(tmp)
  }}, error = function(e){
    tmp <- read.csv(backup, stringsAsFactors = FALSE)
    write.csv(tmp,path,row.names = FALSE)
    return(tmp)
  })
}


UpdateTimeStamps <- function(){
  for(file in 1:length(dataPathMap)) fileUpdates[names(dataPathMap)[file]] <<- GetFileStamp(dataPathMap[[file]])
}

GetFileStamp <- function(path){
  return(file.info(path)$mtime)
}

CompareStamps <- function(type){
  if(fileUpdates[[type]]!=GetFileStamp(dataPathMap[[type]])){
    fileUpdates[type]<<-GetFileStamp(dataPathMap[[type]])
    return(FALSE)
  } else{
    return(TRUE)
  }
}

CompareAllStamps <- function(){
  tmp <- TRUE
  for(type in names(dataPathMap)) tmp <- tmp && CompareStamps(type)
  return(tmp)
}

CompareScoreStamps <- function(){
  return(CompareStamps("scoreboard"))
}

CompareOStamps <- function(){
  return(CompareStamps("offense"))
}

CompareDStamps <- function(){
  return(CompareStamps("defense"))
}

GetSStamp <- function(){
  return(GetFileStamp(dataPathMap[["scoreboard"]]))
}

GetOStamp <- function(){
  return(GetFileStamp(dataPathMap[["offense"]]))
}

GetDStamp <- function(){
  return(GetFileStamp(dataPathMap[["defense"]]))
}

GetMaxStamp <- function(){
  return(max(GetSStamp(),GetOStamp(),GetDStamp()))
}
