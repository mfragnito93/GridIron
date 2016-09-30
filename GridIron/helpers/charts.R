#Plotly charts
plot.bars <- function(data,stack = "group", title = "", xaxis = "", yaxis = "", showLegend = T){
  a <- list(
    title = xaxis,
    type = 'category'
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
    title = xaxis,
    type = 'category'
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
