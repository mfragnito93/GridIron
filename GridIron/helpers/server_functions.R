##Server functions

##Field Map
hashPlot <- function(data,field,title){
  renderPlotly({
    plot.bars(getRPHashField(data,field), title = title, stack = "stack", showLegend = T)
  })
}