
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shinysky)

shinyServer(function(input, output, session) {
  
    UpdateTable <- function(data){
      output$hot <- renderRHandsontable({
        rhandsontable(data, stretchH = "all", height = 65)
      })
    }
    
    UpdateForm <- reactive({
      entry <- hot.to.df(input$hot)
      colnames(entry) <- names(GetMetadata(tableMeta)$fields[-1])
      entry
    })
    
  
    responses <- CreateDefaultRecord()
    
    # input fields are treated as a group -- the row
    formData <- reactive({
      MakeEntry(UpdateForm(),input$id)
      #sapply(names(GetTableMetadata()$fields), function(x) input[[x]])
    })
    
    # Click "Submit" button -> save data
    observeEvent(input$submit, {
      if (input$id != "0") {
        UpdateData(formData())
      } else {
        CreateData(formData())
        UpdateScoreboard(CreateDefaultRecord(), session)
        UpdateTable(CreateDefaultRecord())
      }
    }, priority = 1)
    
    # Press "New" button -> display empty record
    observeEvent(input$new, {
      UpdateScoreboard(CreateDefaultRecord(), session)
      UpdateTable(CreateDefaultRecord())
    })
    
    # Press "Delete" button -> delete from data
    observeEvent(input$delete, {
      DeleteData(formData())
      UpdateScoreboard(CreateDefaultRecord(), session)
      UpdateTable(CreateDefaultRecord())
    }, priority = 1)
    
    # Select row in table -> show details in inputs
    observeEvent(input$responses_rows_selected, {
      if (length(input$responses_rows_selected) > 0) {
        data <- ReadData()[input$responses_rows_selected, ]
        UpdateScoreboard(data,session)
        UpdateTable(data)
      }
      
    })
    
    # display table
    output$responses <- DT::renderDataTable({
      #update after submit is clicked
      input$submit
      #update after delete is clicked
      input$delete
      ReadData()
    }, server = FALSE, selection = "single",
    colnames = unname(GetMetadata(tableMeta)$fields)
    ) 
    

    #initialize handsome table
    UpdateTable(CreateDefaultRecord())
    
    

    
})   

