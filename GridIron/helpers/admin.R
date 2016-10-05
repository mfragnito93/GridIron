#Admin/setting functions
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

preSetDDsTemplate <- read.csv(preSetDDTemplatePath, stringsAsFactors = FALSE)
preSetDDs <- read.csv(preSetDDPath, stringsAsFactors = FALSE)

preSetPDTemplate <- read.csv(preSetPDTemplatePath, stringsAsFactors = FALSE)
plays <- readCSV(preSetPDPath,preSetPDTemplatePath)
scoreboard <- readCSV(preSetScorePath,preSetScoreTemplatePath)
offense <- readCSV(preSetOffensePath,preSetOffenseTemplatePath)
defense <- readCSV(preSetDefensePath,preSetDefenseTemplatePath)


rownames(plays) <- plays[["id"]]
colnames(preSetDDs)<-preSetHeader



#presets


#