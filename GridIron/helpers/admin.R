#Admin/setting functions
preSetDDs <- read.csv(preSetDDPath)

#presets
getDDList <- function(column){
  l<-unique(preSetDDs[,column][preSetDDs[,column]!=""])
  if(is.factor(l)) return(levels(l)) else return(l)
}

#